# test comment
shinyServer(function(input, output, session) {


  # Reactive base data ----
  data <- reactive({
    if (input$cb_imputeDriveHome == TRUE) {
      averageDriveHomeSec <- commute %>%
        filter(!is.na(totalDriveHome)) %>%
        select(totalDriveHome) %>%
        summarize(mean(totalDriveHome)) %>%
        unlist() %>%
        unname() %>%
        as.double() * 60

      commuteImputed <- commute %>% 
        mutate(arriveHome      = if_else((is.na(arriveHome) & !is.na(carDepart_Evening)),
                                    carDepart_Evening + averageDriveHomeSec,
                                    arriveHome
                                    ),
               totalGoneForDay = arriveHome - departHouse
               )
      print(commuteImputed %>% select(arriveHome))
      return(commuteImputed)
    } else {
      commute <- commute %>% 
        mutate(totalGoneForDay = arriveHome - departHouse)
      return(commute)
    }
  })

  # Work arrive/depart density plot ----
  output$arriveAndLeaveWork_DensityPlot <- renderPlot({
    tempPlotData <-
      data() %>%
      select(arriveWork, departWork) %>%
      gather()

    plot <- data.frame(tempPlotData) %>% ggplot(aes(x = value, fill = key))
    plot + geom_density(alpha = 0.25)
  })

  # Typical arrival home for given evening train ----
  output$arriveHomeForGivenTrain_DensityPlot <- renderPlot({
    tempPlotData <- data() %>%
      filter(trainNum_Evening == "331") %>%
      select(arriveHome) %>%
      gather()
    plot <- data.frame(tempPlotData) %>% ggplot(aes(x = value, fill = key))
    plot + geom_density(alpha = 0.25)
  })

  # Density Plot of arriveHome by trainNum_Evening ----
  output$arriveHomeForEachTrain_DensityPlot <- renderPlot({
    tempPlotData <- data() %>%
      filter(!is.na(arriveHome)) %>% # filter(!trainNum_Evening=="333") %>%
      select(trainNum_Evening, arriveHome)
    plot <- data.frame(tempPlotData) %>% ggplot(aes(x = arriveHome, fill = trainNum_Evening))
    # plot + geom_histogram()
    plot + geom_density(alpha = 0.5)
  })

  # Table of typicial arrival home based on evening train----
  output$arriveHomeMeanByTrain_Table <- renderDT({
    data() %>%
      filter(!is.na(arriveHome)) %>%
      group_by(trainNum_Evening) %>%
      summarize(
        result = mean(arriveHome),
        count = n()
      ) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Evening Train Number",
          "Average Arrive Home Time",
          "Count"
        ),
        filter = "none",
        autoHideNavigation = TRUE
      ) %>%
      formatDate(
        columns = "result",
        method = "toLocaleTimeString",
        params = list(
          "en-US",
          list(
            hour = "2-digit",
            minute = "2-digit",
            timeZone = "GMT",
            hour12 = FALSE
          )
        )
      )
  })



  # Table of typicial arrival at work based on morning train----
  output$arriveWorkMeanByTrain_Table <- renderDT({
    data() %>%
      filter(!is.na(arriveWork)) %>%
      group_by(trainNum_Morning) %>%
      summarize(
        result = mean(arriveWork),
        count = n()
      ) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Morning Train Number",
          "Average Arrive Work Time",
          "Count"
        ),
        filter = "none",
        autoHideNavigation = TRUE
      ) %>%
      formatDate(
        columns = "result",
        method = "toLocaleTimeString",
        params = list(
          "en-US",
          list(
            hour = "2-digit",
            minute = "2-digit",
            timeZone = "GMT",
            hour12 = FALSE
          )
        )
      )
  })



  # Morning Commute InfoBox----
  output$morningCommute_InfoBox <- renderInfoBox({
    
    morningCommuteAverage <- data() %>%
      filter(!is.na(totalCommute_Morning)) %>%
      select(totalCommute_Morning) %>%
      summarize(
        meanMorningCommute = round(mean(totalCommute_Morning), 2)
      )

    infoBox(
      title = "Average Morning Commute",
      value = paste0(morningCommuteAverage, " hours"),
      icon = icon("building"),
      color = "orange",
      fill = TRUE
    )
  })
  
  # Evening Commute InfoBox----
  output$eveningCommute_InfoBox <- renderInfoBox({

    eveningCommuteAverage <- data() %>%
      filter(!is.na(totalCommute_Evening)) %>%
      select(totalCommute_Evening) %>%
      summarize(
        meanEveningCommute = round(mean(totalCommute_Evening), 2)
      )
    
    infoBox(
      title = "Average Evening Commute",
      value = paste0(eveningCommuteAverage, " hours"),
      icon = icon("home"),
      color = "maroon",
      fill = TRUE
    )
  })

  # Total Gone For Day InfoBox----
  output$totalGoneForDay_InfoBox <- renderInfoBox({
    
    meanTotalGoneForDay <- data() %>%
      filter(!is.na(totalGoneForDay)) %>%
      pull(totalGoneForDay) %>%
      mean() %>% 
      round(1)
      
    
    infoBox(
      title = "Average Gone For Day",
      value = paste0(meanTotalGoneForDay, " hours"),
      #icon = icon("stopwatch"),
      icon = icon("time", lib = "glyphicon"),
      color = "maroon",
      fill = TRUE
    )
  })
  # MPG Analysis----
  
    # morningMPG_Mean <- commute %>% 
    #   filter(!is.na(mpg_Morning)) %>% 
    #   group_by(route_Morning) %>% 
    #   summarize(meanMPG = mean(mpg_Morning), count = n())
    # 
    # eveningMPG_Mean <- commute %>% 
    #   filter(!is.na(mpg_Evening)) %>% 
    #   group_by(route_Evening) %>% 
    #   summarize(meanMPG = mean(mpg_Evening), count = n())
  output$mpgTestTable <- renderDT({
    morningMPG_Vec <- commute %>% 
      filter(!is.na(mpg_Morning)) %>% 
      pull(mpg_Morning)
    
    eveningMPG_Vec <- commute %>% 
      filter(!is.na(mpg_Evening)) %>% 
      pull(mpg_Evening)
    
    tTest <- t.test(x = morningMPG_Vec, y = eveningMPG_Vec)
    
    tTestSignificance <- 
      if(tTest$p.value > 0.10) {
        ""
      } else if(tTest$p.value > 0.05)  {
        "*"
      } else if(tTest$p.value > 0.01)  {
        "**"
      } else {
        "***"
      }
        
    mpgTestTable <- tibble(mean(morningMPG_Vec),
                           mean(eveningMPG_Vec),
                           mean(morningMPG_Vec) - mean(eveningMPG_Vec),
                           tTest$p.value,
                           tTestSignificance)
    mpgTestTable %>% 
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          bInfo = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Average Morning MPG",
          "Average Evening MPG",
          "Difference",
          "2-Sample t-Test P-Value",
          "Significance"
        ),
        filter = "none",
        autoHideNavigation = TRUE
      ) %>% 
      formatRound(1:3, 1) %>% 
      formatSignif(4, digits = 2, interval = 3, mark = ",", 
                   dec.mark = getOption("OutDec"))
  
  })
  # Troubleshooting table to show data() ----
  output$ts_Data <- renderDT({
    data() %>%
      select(trainNum_Evening, arriveHome) %>%
      datatable(rownames = FALSE) %>%
      formatDate("arriveHome",
        method = "toLocaleTimeString",
        params = list(
          "en-US",
          list(
            hour = "2-digit",
            minute = "2-digit",
            timeZone = "GMT",
            hour12 = FALSE
          )
        )
      )
  })
})
