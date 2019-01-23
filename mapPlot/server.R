server <-
  function(input,output){
    
        #create the map
      output$mymap <- renderLeaflet({
        # import airports data
        source("airports.R")
        
        #import airlines data from Flightbehavior 
        #we can import the whole data from FlightBehaviour by: source("FlightBehaviour.R")
        load('flights.RData') 
        #define the color of the circles
        pal2 <- colorNumeric(
          palette = c('red', 'blue'),
          domain = FlightBehaviour$propArrDelay
        )
        cutoff <- as.numeric(input$cutoff)
        proDelay <- as.numeric(input$proDelay)
  
        leaflet(data) %>%
          setView(lat =45, lng =-99, zoom = 4)  %>% #setting the view over ~ center of North America
          addTiles() %>%
          addCircles(data =  subset(airports, NArrivals>cutoff), lat = ~ latitude_deg,lng = ~ longitude_deg
                     ,radius = ~ sqrt(NArrivals)*200  , popup = ~as.character(iata_code),
                     color=~ifelse(FlightBehaviour$propArrDelay<proDelay, "red", "blue"),fillOpacity=0.0) %>%
          addLegend("bottomright", values = FlightBehaviour$propArrDelay,title = "Legend",pal=pal2, opacity = 1)
     } )   
        
  }
  


