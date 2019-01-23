server <-
function(input,output){
    output$main_plot <- renderPlot({
 
        
        source("airports.R")
        source("cancellations.R")
        plot(latitude_deg ~ longitude_deg, data = subset(airports, NArrivals>input$cutoff),
             cex=sqrt(NArrivals)/100,
             ylim=c(21, 50), xlim=c(-125, -65), axes=FALSE, xlab="", ylab="", col="grey")
        title("Where do the airplanes fly?")
        legend("bottomleft", legend=c("size: No. Flights"), pch=c(1))
        with(subset(cancellations, Number>input$Nflights), segments(longOrigin, latOrigin,
                                                          longDest, latDest, col="grey", lwd=Number/2000))
        text(latitude_deg ~ longitude_deg, label=iata_code,
             data = subset(airports, NArrivals>input$cutoff),
             ylim=c(21, 50), xlim=c(-125, -65))
        
        
        
    })
}
