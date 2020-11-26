library("googlesheets4")
library("ggplot2")
library("DT")
library("lubridate")
library("shinydashboard")
library("shiny")
library("tidyverse")

Sys.setenv(TZ = "UTC")

# Load 17H Tracker ----
gs4_deauth()
raw_data <- googlesheets4::read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/15334OCPKXEp6HIRIWLiJdVqJq97VnJujI6R9FB1sK7c/edit#gid=0',
  sheet = 1,
  range = cell_cols('A:G'),
  # locale = locale(tz = "UTC"),
  col_types = 'cciiccc' # reading dates and times as characters and handling in next step
  ## Leaving this for when googlesheets4 implements cols() column type specification
  # col_types = cols(date           = col_date(format = "%y-%m-%d"), 
  #                  route          = 'c',
  #                  bus_num        = 'd',
  #                  stop_id        = 'i',
  #                  stop_desc      = 'c',
  #                  time_at_stop   = 't',
  #                  time_at_pentagon = 't'
  # )
)

data <- raw_data %>% 
  rename(
    date_raw = date, 
    time_at_stop_raw = time_at_stop, 
    time_at_pentagon_raw = time_at_pentagon
  ) %>% 
  mutate(
    date = lubridate::ymd(date_raw), 
    time_at_stop = gsub('^(\\d+)(\\d{2})$', '\\1:\\2', time_at_stop_raw),
    time_at_pentagon = gsub('^(\\d+)(\\d{2})$', '\\1:\\2', time_at_pentagon_raw),
    
    # time_at_stop = lubridate::hm(time_at_stop))
    time_at_stop = lubridate::parse_date_time(time_at_stop, orders="HM"),
    time_at_pentagon = lubridate::parse_date_time(time_at_pentagon, orders="HM"),
    
    trip_duration = lubridate::interval(time_at_stop, time_at_pentagon) %/% minutes()
  )
    

# Load 2018 Tracker ----
# tracker <- googlesheets::gs_url(
#   x      = "https://docs.google.com/spreadsheets/d/11JAREyf0pTdR0d0b_DXDjOsI8P5L2WH_P31ALfqtSXI/edit?usp=sharing",
#   lookup = TRUE
# )

# Read 'commute' tab (ws = 2) ----
commuteRaw <- gs_read(
  ss = tracker,
  ws = 2,
  range = cell_cols('A:T'),
  locale = locale(tz = "UTC"),
  col_types = cols(date                   = col_date(format = "%y-%m-%d"), 
                   alarm                  = col_time(),
                   departHouse            = 't',
                   arrivePlatform_Morning = 't',
                   route_Morning          = 'c',
                   mpg_Morning            = 'd',
                   score_Morning          = 'i',
                   trainNum_Morning       = 'c',
                   trainDepart_Morning    = 't',
                   trainArrive_Morning    = 't',
                   arriveWork             = 't',
                   departWork             = 't',
                   trainNum_Evening       = 'c',
                   arrivePlatform_Evening = 't',
                   trainDepart_Evening    = 't',
                   carDepart_Evening      = 't',
                   arriveHome             = col_time(), 
                   route_Evening          = 'c',
                   mpg_Evening            = 'd',
                   score_Evening          = 'i')
)

# Remove future rows ----
commuteRaw <- commuteRaw %>% filter(date <= Sys.Date())


# Convert times to POSIXct ----
commute <- commuteRaw %>% mutate(alarm               = as.POSIXct(alarm, format="%H:%M"),
                              arriveHome             = as.POSIXct(arriveHome, format="%H:%M"), #, tz="America/New_York"),
                              arrivePlatform_Morning = as.POSIXct(arrivePlatform_Morning, format="%H:%M"),
                              arriveWork             = as.POSIXct(arriveWork, format="%H:%M"),
                              carDepart_Evening      = as.POSIXct(carDepart_Evening, format="%H:%M"),
                              departHouse            = as.POSIXct(departHouse, format="%H:%M"),
                              departWork             = as.POSIXct(departWork, format="%H:%M"),
                              arrivePlatform_Evening = as.POSIXct(arrivePlatform_Evening, format="%H:%M"),
                              trainArrive_Morning    = as.POSIXct(trainArrive_Morning, format="%H:%M"),
                              trainDepart_Evening    = as.POSIXct(trainDepart_Evening, format="%H:%M"),
                              trainDepart_Morning    = as.POSIXct(trainDepart_Morning, format="%H:%M"))

# Compute fields ----
commute <- commute %>% mutate(totalCommute_Morning = arriveWork - departHouse,
                              totalCommute_Evening = arriveHome - departWork,
                              totalDriveHome       = arriveHome - carDepart_Evening)


# Train Ride Density Plot
# tempPlotData <- commute %>% filter(trainNum_Morning == "328") %>% 
#   select(departHouse, arrivePlatform_Morning, trainDepart_Morning, trainArrive_Morning, arriveWork) %>% 
#   gather()
# 
# plot <- data.frame(tempPlotData) %>% ggplot(aes(x=value, fill=key))
# plot + geom_density(alpha=0.25)


# plot <- data.frame(commute) %>% ggplot(aes(x=arriveWork))
# plot + geom_density()
# 
# plot <- data.frame(commute) %>% ggplot(aes(x=alarm))
# plot + geom_density()


# Correct Google Sheet Format (spread and rewrite to sheets) --------------
# commute <- commute %>% select(date, alarm, departHouse, arrivePlatform_Morning, route_Morning,
#                               mpg_Morning, score_Morning, trainNum_Morning, trainDepart_Morning,
#                               trainArrive_Morning, arriveWork, departWork, trainNum_Evening, 
#                               trainArrive_Evening, trainDepart_Evening, carDepart_Evening,
#                               arriveHome, route_Evening, mpg_Evening, score_Evening) %>%
#         rename(arrivePlatform_Evening = trainArrive_Evening,
#                arriveCar_Evening      = carDepart_Evening)
#   
# googlesheets::gs_ws_new(ss = tracker,
#                         ws_title = "commute")
# gs_gs(tracker)
# googlesheets::gs_edit_cells(ss = tracker,
#                             ws = 4,
#                             input = commute,
#                             anchor = "A1",
#                             col_names = TRUE)

