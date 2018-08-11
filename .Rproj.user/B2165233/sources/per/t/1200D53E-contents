dashboardPage(
  title = "Commute Dashboard",

  dashboardHeader(
    title = div(
      id = "header_title",
      span(tagList(icon("cubes"), "Commute Dashboard"))
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed; overflow: visible;",
      actionButton(
        inputId = "ab_refreshData",
        label = "Refresh Data",
        icon = icon("refresh")
      ),
      checkboxInput(
        inputId = "cb_imputeDriveHome",
        label = "Impute Drive Home, if possible",
        value = TRUE
      )
    )
  ),

  dashboardBody(
    box(
      title       = "Morning and Evening Commute Averages",
      width       = 12,
      collapsible = TRUE,
      collapsed   = FALSE,
      
      infoBoxOutput("morningCommute_InfoBox"),
      infoBoxOutput("eveningCommute_InfoBox"),
      infoBoxOutput("totalGoneForDay_InfoBox")
    ),
    box(
      title       = "Morning and Evening Train Average Arrival Times",
      width       = 12,
      collapsible = TRUE,
      collapsed   = FALSE,
      
      DTOutput("arriveWorkMeanByTrain_Table"),
      DTOutput("arriveHomeMeanByTrain_Table")
    ),
    box(
      title       = "Gas Mileage",
      width       = 12,
      collapsible = TRUE,
      collapsed   = FALSE,
      
      DTOutput("mpgTestTable")
    ),
    plotOutput("arriveAndLeaveWork_DensityPlot"),
    plotOutput("arriveHomeForEachTrain_DensityPlot")#,
    # DTOutput("ts_Data")

  )
)
