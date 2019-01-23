ui <- shinyUI(pageWithSidebar(
  headerPanel("Flight Arrivals Plot"),
  sidebarPanel(
    sliderInput("cutoff", "Enter Arrivals cutoff", "120000", min=1 , max =400000),
    sliderInput("Nflights", "Enter number of flights cutoff:", "7500", min=1 , max =14000)
    
    
  ),
  mainPanel(
    plotOutput(outputId='main_plot')
)
)
)



