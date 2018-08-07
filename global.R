library("googlesheets")
library("ggplot2")
suppressPackageStartupMessages(library("dplyr"))
library("lubridate")
library("shinydashboard")
library("shiny")
library("tidyverse")



# Load 2018 Tracker, Daily Times sheet
tracker <- gs_url(
  x = "https://docs.google.com/spreadsheets/d/11JAREyf0pTdR0d0b_DXDjOsI8P5L2WH_P31ALfqtSXI/edit?usp=sharing",
  lookup = TRUE
)

# Read 'Daily Times' tab (ws = 2)
commuteRaw <- gs_read(
  ss = tracker,
  ws = 2,
  range = cell_cols('A:T'),
  col_types = cols(date                   = col_date(format = "%y-%m-%d"), 
                   alarm                  = 't',
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
                   arriveHome             = "t", 
                   route_Evening          = 'c',
                   mpg_Evening            = 'd',
                   score_Evening          = 'i')
)

# Remove future rows
commuteRaw <- commuteRaw %>% filter(date < Sys.Date())


# Convert times to POSIXct ----
commute <- commuteRaw %>% mutate(alarm               = as.POSIXct(alarm, format="%H:%M"),
                              arriveHome             = as.POSIXct(arriveHome, format="%H:%M"),
                              arrivePlatform_Morning = as.POSIXct(arrivePlatform_Morning, format="%H:%M"),
                              arriveWork             = as.POSIXct(arriveWork, format="%H:%M"),
                              carDepart_Evening      = as.POSIXct(carDepart_Evening, format="%H:%M"),
                              departHouse            = as.POSIXct(departHouse, format="%H:%M"),
                              departWork             = as.POSIXct(departWork, format="%H:%M"),
                              arrivePlatform_Evening = as.POSIXct(arrivePlatform_Evening, format="%H:%M"),
                              trainArrive_Morning    = as.POSIXct(trainArrive_Morning, format="%H:%M"),
                              trainDepart_Evening    = as.POSIXct(trainDepart_Evening, format="%H:%M"),
                              trainDepart_Morning    = as.POSIXct(trainDepart_Morning, format="%H:%M"))

commute <- commute %>% mutate(totalCommute_Morning = arriveWork - departHouse,
                              totalCommute_Evening = arriveHome - departWork,
                              totalGoneForDay      = arriveHome - departHouse)


# Train Ride Density Plot
tempPlotData <- commute %>% filter(trainNum_Morning == "328") %>% 
  select(departHouse, arrivePlatform_Morning, trainDepart_Morning, trainArrive_Morning, arriveWork) %>% 
  gather()

plot <- data.frame(tempPlotData) %>% ggplot(aes(x=value, fill=key))
plot + geom_density(alpha=0.25)


# Typical arrival home for given evening train
tempPlotData <- commute %>% filter(trainNum_Evening == "331") %>% 
  select(arriveHome) %>% 
  gather()
plot <- data.frame(tempPlotData) %>% ggplot(aes(x=value, fill=key))
plot + geom_density(alpha=0.25)

# Table of typicial arrival home based on train
commute %>% filter(!is.na(arriveHome)) %>% group_by(trainNum_Evening) %>% summarize(result = mean(arriveHome))


# Density plot of arrive home by evening train
tempPlotData <- commute %>% 
  filter(!is.na(arriveHome)) %>% #filter(!trainNum_Evening=="333") %>% 
  select(trainNum_Evening, arriveHome) 
plot <- data.frame(tempPlotData) %>% ggplot(aes(x=arriveHome, fill=trainNum_Evening))
plot + geom_histogram()

plot + geom_density(alpha=0.5)




plot <- data.frame(commute) %>% ggplot(aes(x=arriveWork))
plot + geom_density()

plot <- data.frame(commute) %>% ggplot(aes(x=alarm))
plot + geom_density()







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

