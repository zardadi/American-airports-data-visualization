# American-airports-data-visualization
# Author: Mohsen Zardadi
# January 20, 2019

##################################################################################################
##################################################################################################
 The data in air.csv relate to airline cancellations and delays for the year 2008.
 You can download the air.csv dataset from
 http://rtricks4kids.ok.ubc.ca/wjbraun/DS550/air.csv.
 It is too large to fit on the regular github repository.

airorts.R and FightBehaviour.R are derived from air.csv which include record of over 7 million flights.
Data in cancellations give the coordinates of originating and terminating airports of
flights taking place in the U.S. For each connection, the number of flights has been calculated (Number), as well as the corresponding numbers (and proportions) of cancelled
and diverted flights

FlightBehaviour contains similar kinds of counts, but broken down by airline carrier (Carrier). It also includes ArrDelay which gives information summarizing the number
of flights that were delayed by more than 5 minutes.

"cancelations", "arrivals", and "mapPlot" folders are shinny app for data visualization. These apps run through visulization.R.
"main.R" includes data wrangling and seasonal explorations. 
