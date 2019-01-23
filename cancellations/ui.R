ui <- shinyUI(pageWithSidebar(
  headerPanel("Flight Cancellation Plot"),
  sidebarPanel(
    sliderInput("cutoff", "Enter Arrivals cutoff:", "12000", min=1 , max =400000),
    sliderInput("Ncanceled", "Enter cancellation cutoff:", "50", min=1 , max =900)
    
    
  ),
  mainPanel(
    plotOutput(outputId='main_plot')
)
)
)



