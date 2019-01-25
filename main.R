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
          'WeatherDelay', 'CarrierDelay','TailNum', 'CancellationCode')
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
# 40 most busiest airports by arrivals
head(Airports[1],40)



# Select interesting columns from the air data set
subair <- air %>% select(Dest, Origin, Month, ArrDelay, DepDelay, Cancelled)

# Adding seasons into the data

season <- list('spring' = 3:5 , 'summer' = 6:8, 'fall'= 9:11 , 'winter' = c(1,2,12))   

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
