server <-
function(input,output){
    output$main_plot <- renderPlot({
 
        
        source("airports.R")
        source("cancellations.R")
        par(mar=c(1, 1, 3, 1))
        cutoff <- as.numeric(input$cutoff )
        plot(latitude_deg ~ longitude_deg, data = subset(airports,
                                                         NArrivals>cutoff), cex=sqrt(NArrivals)/100,
             ylim=c(21, 50), xlim=c(-125, -65), axes=FALSE, xlab="", ylab="",
             col="grey")
        title("Which connections are frequently cancelled?")
        legend("bottomleft", legend=c("size: No. Flights"), pch=c(1))
        legend("bottomright", legend=c("Code size: No. Cancellations"))
        with(subset(cancellations, Cancelled > input$Ncanceled),
             segments(longOrigin, latOrigin, longDest, latDest, col="grey",
                      lwd=Cancelled/500))
        text(latitude_deg ~ longitude_deg, label=iata_code,
             data = subset(airports, NArrivals>cutoff),
             ylim=c(21, 50), xlim=c(-125, -65), cex=sqrt(Cancelled)/70)
        
        
        
        
    })
}
