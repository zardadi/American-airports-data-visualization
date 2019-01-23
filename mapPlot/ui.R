ui <- fluidPage( headerPanel("Choosing AIPN Conference Destination"), leafletOutput(outputId = "mymap"), sidebarPanel(
    sliderInput("cutoff", "Number of arrivals cutoff:", "2000", min =10000, max= 400000),
    sliderInput("proDelay","proArrival delay cutoff:", "0.1", min = 0, max = 1 )
  )


)

#pageWithSidebar