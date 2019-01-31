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

############################################# data wrangling ##############################################################
###########################################################################################################################
library(dplyr)

# Exploring air data set:
summary(air)

# replace all the empty vlaues with 'Null'
air[air==""] <- NA
# checking null values
sapply(air,function(x) sum(is.na(x)))

# missing values visualizing. 
#library(Amelia)
#missmap(air, main = "Missing values vs observed")

# drop columns which are non-relevent or have the most missing values
drop <- c('DepTime', 'ArrTime', 'ActualElapsedTime', 'CRSElapsedTime','AirTime',
          'TaxiIn','TaxiOut','LateAircraftDelay', 'SecurityDelay', 'NASDelay',
          'WeatherDelay', 'CarrierDelay','TailNum', 'CancellationCode', 'CRSDepTime', 'FlightNum', 'Distance', 'Cancelled',
          'Diverted')
airdf <- air[,!(names(air)%in%drop)]


# checking missing values in the  sub-set
sapply(airdf, function(x) sum(is.na(x)))

# removing all rows with null values.
# (we may replace null values with some specific values)
airdf <- na.omit(airdf)

# checking null values

sapply(airdf, function(x) sum(is.na(x)))

# no more null values, Hooray!

# explor the data
sapply(airdf, function(x) length(unique(x)))


################################################################################################################################
###############################################################################################################################


# Finding all the Airports in the data set and sort them by most arrivals
Airports <- airdf %>% group_by(Dest) %>% count(Dest)
Airports <- Airports[order(-Airports$n),]
# Number of airports in the data set
nrow(Airports)
# 30 most busiest airports by arrivals
head(Airports[1],30)
# select top 10 busiest airports by arrival
topAirports <- Airports[1:10,][1]
topAirports
top10 <- subset(airdf, Dest %in% topAirports$Dest)
head(top10)


library(lattice)
# removing negative arrivals
top10 <- top10[top10$ArrDelay > 5,]
# Arrdelay box plot for top 10 airports
bwplot(ArrDelay~Dest, data = top10, ylim=c(0,180), do.out= FALSE, col='black', pch=20,
       xlab = "airports", ylab = "arrival delay per minute",
       main = "Arrival Delay for Top 10 Busiest Airports")

# Departure box plot for top 10 airports
top10.dep <- subset(airdf, Origin %in%topAirports$Dest )
top10.dep <- top10.dep[top10.dep$DepDelay > 0 ,]
bwplot(DepDelay~Origin, data = top10.dep, ylim=c(0,160), do.out= FALSE, col='black', pch=20,
       xlab = "airports", ylab = "departure delay per minute",
       main = "Departure Delay for Top 10 Busiest Airports")


# It seems ORD(Chicago airport) is the worst airport based on Arrival and departure delay
# Let's have a look at ORD airpot and see if there is any seasonal trend in the data.
ord <- subset(top10, Dest == 'ORD')
x11()
ord$Month <- as.factor(ord$Month)
ord$DayOfWeek <- as.factor(ord$DayOfWeek)

bwplot(ArrDelay~Month, data=ord, ylim = c(0,250), do.out=FALSE, col ='black', pch =20,
       xlab = "month", ylab="arrival delay per minute",
       main = "Arrival Flight Delays at ORD Airport")

bwplot(ArrDelay~DayOfWeek | Month, data = ord, ylim = c(0,300), do.out =FALSE)
# Not much information we can get
# Let's do seasonal analysis
# Add four seasons into data
season <- list('spring' = 3:5 , 'summer' = 6:8, 'fall'= 9:11 , 'winter' = c(1,2,12))   

ord$season <- ifelse(ord$Month %in% season$fall, "fall", 
                      ifelse(ord$Month %in% season$winter, "winter",
                             ifelse(ord$Month %in% season$spring, "spring", "summer")))
bwplot(ArrDelay~season, data = ord, ylim = c(0,210), do.out =FALSE, col='black', pch=16,
       xlab = "seasons", ylab = "arrival delay per minute",
       main = "Arrival Flight Delays at ORD Airport per Season")
# Winter has the most delays in Chicago airport

# sort ORD data by airlines 

airlines.ord <- ord %>% group_by(UniqueCarrier) %>% count(UniqueCarrier)
airlines.ord <- airlines.ord[order(-airlines.ord$n),] 
# Number of airlines 
nrow(airlines.ord)
topAirlines.ord <- airlines.ord[1:10,]
topAirlines.ord
top10.airlines <- subset(ord, UniqueCarrier %in% topAirlines.ord$UniqueCarrier)
# removing arrival delays less than 5 minutes
top10.airlines <- top10.airlines[top10.airlines$ArrDelay > 5,]
# Arrdelay box plot for top 10 airports
bwplot(ArrDelay~UniqueCarrier, data = top10.airlines, ylim=c(0,250), do.out=FALSE,
       xlab = "airline", ylab = "arrival delay per minute",
       main ="Arrival Flight Delays at ORD airport")
# We can conclude that two airlines YV and OO have the worst delay result

# Average delay based on airlines:
top10.airlines <- subset(ord, UniqueCarrier %in% topAirlines.ord$UniqueCarrier)
# removing arrival delays less than 0 minutes
top10.airlines <- top10.airlines[top10.airlines$ArrDelay >= 0,]

ave.delay <- aggregate(top10.airlines$ArrDelay, by = list(top10.airlines$UniqueCarrier), FUN = mean, na.rm = TRUE)
ave.delay <- ave.delay[order(ave.delay$x),]
xx <- barplot(ave.delay$x, main = "Average Arrival Delays at ORD Airport", las=1,
              names.arg = ave.delay$Group.1,
              width = c(1,1,1,1),
              xlab = "airline",
              ylab = "average delay per flight (min)", ylim = c(0,80))
text(x = xx, y = ave.delay$x, label = round(ave.delay$x), pos = 3, cex = 0.8, col = "black")

xx <- barplot(topAirlines.ord$n, main = "Number of Inbound Flights per Airline at ORD Airport", las=1,
              names.arg = topAirlines.ord$UniqueCarrier,
              width = c(1,1,1,1),
              xlab = "airline",
              ylab = "number of flights", ylim = c(0,50000))
text(x = xx, y = topAirlines.ord$n, label = round(topAirlines.ord$n), pos = 3, cex = 0.8, col = "black")
library(shiny)
library(leaflet)
library(markdown)
runApp("ORD")


# top 5 busiest airports:
topAirports <- Airports[1:5,][1]
top5 <- subset(airdf, Dest %in% topAirports$Dest)
head(top5)

# removing negative arrivals
top5 <- top5[top5$ArrDelay > 5,]
# Arrdelay box plot for top 5 airports
bwplot(ArrDelay~Dest, data = top5, ylim=c(0,250))

# Let's compare all the top airports

top5$Month <- as.factor(top5$Month)
x11()
bwplot(ArrDelay~Month | Dest, data=top5, ylim = c(0,300))
# not much we can infer
# Let's do four seasons analysis
# Add four seasons into data
season <- list('spring' = 3:5 , 'summer' = 6:8, 'fall'= 9:11 , 'winter' = c(1,2,12))   

top5$season <- ifelse(top5$Month %in% season$fall, "fall", 
                        ifelse(top5$Month %in% season$winter, "winter",
                               ifelse(top5$Month %in% season$spring, "spring", "summer")))

x11()
bwplot(ArrDelay~season | Dest, data=top5, ylim = c(0,200))
# we can see Fall has the lowest delays compare to the other seasons.

# Repeat the same analysis for top 10 airports
top10$season <- ifelse(top10$Month %in% season$fall, "fall", 
                      ifelse(top10$Month %in% season$winter, "winter",
                             ifelse(top10$Month %in% season$spring, "spring", "summer")))

top10$Month <- as.factor(top10$Month)
x11()
bwplot(ArrDelay~Month | Dest, data=top10, ylim = c(0,300))
xyplot(ArrDelay~Month | Dest, data=top10, ylim = c(0,300))

x11()
bwplot(ArrDelay~season | Dest, data=top10, ylim = c(0,200))
# for most airports fall has the least delays.

# let's see if there is any daily effect.
top10$DayOfWeek <- as.factor(top10$DayOfWeek)
x11()
bwplot(ArrDelay~DayOfWeek | Dest, data=top10, ylim = c(0,200))

################################################################################################################################
################################################################################################################################
# checking airlines
airlines <- airdf %>% group_by(UniqueCarrier) %>% count(UniqueCarrier)
airlines <- airlines[order(-airlines$n),]
# Number of airlines in the data set
nrow(airlines)
# select top 10 busiest airlines
topAirlines <- airlines[1:10,]
topAirlines
top10 <- subset(airdf, UniqueCarrier %in% topAirlines$UniqueCarrier)
head(top10)
# removing arrival delays less than 5 minutes
top10 <- top10[top10$ArrDelay > 5,]
# Arrdelay box plot for top 10 airports
bwplot(ArrDelay~UniqueCarrier, data = top10, ylim=c(0,250))


top10$Month <- as.factor(top10$Month)
x11()
bwplot(ArrDelay~Month | UniqueCarrier, data=top10, ylim = c(0,300))






#############################################################################################################################
#############################################################################################################################
# Select interesting columns from the air data set
subair <- airdf %>% select(Dest, Origin, Month, ArrDelay, DepDelay, Cancelled)

# Adding seasons into the data


subair$season <- ifelse(subair$Month %in% season$fall, "fall", 
                        ifelse(subair$Month %in% season$winter, "winter",
                               ifelse(subair$Month %in% season$spring, "spring", "summer")))


# Average seasonal cancellation:

aseasondata <- aggregate(subair$Cancelled, by = list(subair$season), FUN = mean, na.rm = TRUE)
aseasondata$x <- (aseasondata$x)*100
xx <- barplot(aseasondata$x, main = "Seasonal Cancelled Flights", las=1,
              names.arg = aseasondata$Group.1,
              ylab = "average percentage of cancelled flights", ylim = c(0,4))
text(x = xx, y = aseasondata$x, label = round(aseasondata$x), pos = 3, cex = 0.8, col = "blue")


# Average seasonal delay:
subair <- subset(subair, ArrDelay > 0 )
aseasondata <- aggregate(subair$ArrDelay, by = list(subair$season), FUN = mean, na.rm = TRUE)
aseasondata <- as.list(aseasondata)
xx <- barplot(aseasondata$x, main = "Seasonal Arrival Delay for US Flights", las=1,
              names.arg = aseasondata$Group.1,
              width = c(1,1,1,1),
              xlab = "seasons",
              ylab = "average delay per flight (min)", ylim = c(0,40))
text(x = xx, y = aseasondata$x, label = round(aseasondata$x), pos = 3, cex = 0.8, col = "blue")
# So we found fall has the least seasonal delay


# Select some airport based on most number of flight arrivals and less proportional delay.
# These airporst are selected through shinyapp "mapPlot"

airports_lessdelay <- c('LGA', 'DTW', 'ORD', 'IAD', 'RDU', 'DFW', 'MEM', 'PDX', 'MCI', 'STL', 'CVG' )

library(gplots)
# Choose just Fall data
falldata <- subset(subair, season == 'fall')

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


# Now we may want to know the number of Cancelation for Fall

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
