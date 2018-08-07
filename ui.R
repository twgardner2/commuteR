dashboardPage(
  
  # title = tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'treasury_logo.png'),
  #                   tags$title('BGP - PAPRS 2.0')),
  
  dashboardHeader(
    title = div(id = "header_title",
                span(tagList(icon("cubes"), "Commute Analysis")))),  
  
  dashboardSidebar(
    actionButton(inputId = "refreshDataActionButton",
                 label   = "Refresh Data",
                 icon    = icon("refresh"))
  ),
  
  dashboardBody(
    plotOutput("arriveAndLeaveWorkPlot")
  )
  
)