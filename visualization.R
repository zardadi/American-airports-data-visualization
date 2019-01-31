# Project: airline cancellations and delays
# Author: Mohsen Zardadi
# January 2019

##################################################################################################
##################################################################################################
# The data in air.csv relate to airline cancellations and delays for the year 2008.
# You can download the air.csv dataset from
# http://rtricks4kids.ok.ubc.ca/wjbraun/DS550/air.csv.
# It is too large to fit on the regular github repository.
# In order to read the data from air.csv into R, you will need to type

#air <- read.csv("air.csv")
#summary(air)
#head(air)


# airorts.R and FightBehaviour.R are derived from air.csv which include record of over 7 million flights.

source("airports.R")
summary(airports)

# Data in cancellations give the coordinates of originating and terminating airports of
# flights taking place in the U.S. For each connection, the number of flights has been calculated (Number),
#as well as the corresponding numbers (and proportions) of cancelled
# and diverted flights
source("cancellations.R")
summary(cancellations)


# FlightBehaviour contains similar kinds of counts, but broken down by airline carrier
# (Carrier). It also includes ArrDelay which gives information summarizing the number
# of flights that were delayed by more than 5 minutes.
source("FlightBehaviour.R")
summary(FlightBehaviour)


# cancellations shiny app is a shiny app which provide data visualization on cancellations.R
# It contains two cut-off levels for arrivals and cancellation. 
library(shiny)
runApp("cancellations")

# Here is a shiny app for arrivals data visualization
runApp("arrivals")


###
library(dplyr)
data = subset(airports, NArrivals>140000)
nrow(data)

subair <- air %>% select(Dest, Origin, Month, ArrDelay, DepDelay, Cancelled)


season <- list('spring' = 3:5 , 'summer' = 6:8, 'fall'= 9:11 , 'winter' = c(1,2,12))   

subair$season <- ifelse(subair$Month %in% season$fall, "fall", 
                        ifelse(subair$Month %in% season$winter, "winter",
                         ifelse(subair$Month %in% season$spring, "spring", "summer")))


#Average seasonal cancelation:

aseasondata <- aggregate(subair$Cancelled, by = list(subair$season), FUN = mean, na.rm = TRUE)
aseasondata$x <- (aseasondata$x)*100
xx <- barplot(aseasondata$x, main = "Seasonal Cancelled Flights", las=1,
              names.arg = aseasondata$Group.1,
              ylab = "average percentage of cancelled flights", ylim = c(0,4))
text(x = xx, y = aseasondata$x, label = round(aseasondata$x), pos = 3, cex = 0.8, col = "blue")


#Average seasonal delay:
subair <- subset(subair, ArrDelay > 0 )
aseasondata <- aggregate(subair$ArrDelay, by = list(subair$season), FUN = mean, na.rm = TRUE)
aseasondata <- as.list(aseasondata)
xx <- barplot(aseasondata$x, main = "Seasonal Arrival Delay for US Flights", las=1,
        names.arg = aseasondata$Group.1,
        width = c(1,1,1,1),
        xlab = "seasons",
        ylab = "average delay per flight (min)", ylim = c(0,40))
text(x = xx, y = aseasondata$x, label = round(aseasondata$x), pos = 3, cex = 0.8, col = "blue")



# 
airports_lessdelay <- c('LGA', 'DTW', 'ORD', 'IAD', 'RDU', 'DFW', 'MEM', 'PDX', 'MCI', 'STL', 'CVG' )
library(gplots)
falldata <- subset(subair, season == 'fall')

#airports_fall <- subset(falldata, Dest %in% data$iata_code)
airports_fall <- subset(falldata, Dest %in% airports_lessdelay)

airports_fall <- subset(airports_fall, ArrDelay > 0 )

gdata <- aggregate(airports_fall$ArrDelay, by = list(airports_fall$Dest), FUN= mean,  na.rm = TRUE)
gdata <- gdata[order(gdata$x),]
xx <- barplot(gdata$x, main = "Arrival Delay in Fall ", las=2,
        names.arg = gdata$Group.1,
        ylab = "average delay per flight (min)",
        xlab = "airports",
        ylim = c(0,40), axisnames = FALSE)
text(x = xx, y = gdata$x, label = round(gdata$x), pos = 3, cex = 0.8, col = "blue")
angleAxis(side = 1, at = c(1,2.1,3.2,4.3,5.5,6.8,8.1,9.1,10.5,11.5,12.8), labels = gdata$Group.1, srt =45)


#Now we calculate the number of Cancelation for Fall

airports_fall <- subset(falldata, Dest %in% airports_lessdelay)


gdata <- aggregate(airports_fall$DepDelay, by = list(airports_fall$Dest), FUN= mean,  na.rm = TRUE)
gdata <- gdata[order(gdata$x),]
xx <- barplot(gdata$x, main = "Departure Delay in Fall ", las=2,
              names.arg = gdata$Group.1,
              ylab = "average delay per flight (min)",
              xlab = "airports",
              ylim = c(0,40), axisnames = FALSE)
text(x = xx, y = gdata$x, label = round(gdata$x), pos = 3, cex = 0.8, col = "blue")
angleAxis(side = 1, at = c(1,2.1,3.2,4.3,5.5,6.8,8.1,9.1,10.5,11.5,12.8), labels = gdata$Group.1, srt =45)
