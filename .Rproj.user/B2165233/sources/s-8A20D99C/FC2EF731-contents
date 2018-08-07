

shinyServer(function(input, output, session) {

  # Work arrive/depart density plot
  output$arriveAndLeaveWorkPlot <- renderPlot({
    tempPlotData <-
      commute %>%
      select(arriveWork, departWork) %>%
      gather()

    plot <- data.frame(tempPlotData) %>% ggplot(aes(x = value, fill = key))
    plot + geom_density(alpha = 0.25)
  })
})
