
shinyServer(function(input, output, session) {


  # Reactive base data ----
  data <- reactive({
    
    if(input$cb_imputeDriveHome == TRUE) {
      averageDriveHome <- commute %>% filter(!is.na(totalDriveHome)) %>% 
        select(totalDriveHome) %>% 
        summarize(mean(totalDriveHome)) %>% 
        unlist() *60 #%>% as.POSIXct(origin="1970-01-01 00:00:00")
      
      commuteImputed <- commute %>% mutate(arriveHome = ifelse((is.na(arriveHome) & !is.na(carDepart_Evening)),
                                                                as.POSIXct(carDepart_Evening + averageDriveHome), 
                                                                arriveHome))
      print(commuteImputed %>% select(arriveHome))
      return(commuteImputed)
    } else {
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
    plot + geom_histogram()
    # plot + geom_density(alpha = 0.5)
  })

  # Table of typicial arrival home based on train ----
  output$arriveHomeMeanByTrain_Table <- renderDT({
    
    data() %>% filter(!is.na(arriveHome)) %>% 
      group_by(trainNum_Evening) %>% 
      summarize(result = mean(arriveHome)) %>% 
      datatable() # %>% 
      #formatDate('result', method = "toLocaleTimeString")

    })
  

  # Troubleshooting table to show data() ----
  output$ts_Data <- renderDT({
    data() %>% select(trainNum_Evening, arriveHome) %>% 
      datatable() # %>% 
      #formatDate(2, method = "toLocaleTimeString", params = NULL)
    
  })
  
  })
