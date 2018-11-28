setwd("C:/Users/laure/stack/GITHUB/Check if feature is near to GPS coordinate")


library(data.table)
library(hutils)
library(tidyverse)
library(geosphere)
library(sp)
library(rbenchmark)




# create some fake GPS data
set.seed(1)
number_of_GPS_coordinates <- 100000
gpsdata.fake <- data.frame(id=1:number_of_GPS_coordinates,
                           device_id=1,
                           latitude=runif(number_of_GPS_coordinates,50.5,53.5), 
                           longitude=runif(number_of_GPS_coordinates,4,7))


number_of_bus_stops <- 5000
bus_stops.fake <- data.frame(id=1:number_of_bus_stops,
                             name=replicate(number_of_bus_stops, paste(sample(LETTERS, 15, replace=TRUE), collapse="")),
                             latitude=runif(number_of_bus_stops,50.5,53.5), 
                             longitude=runif(number_of_bus_stops,4,7))


source("function version 0.R")
source("function version 1.R")
source("function version 2.R")
source("function version 3.R")


benchmarks <- benchmark(
  
  "0" = {
    output0 <-check_if_close_original(dataset1     = gpsdata.fake,       
                                    dataset2     = bus_stops.fake,     
                                    desired.dist = .2)
  },
  "1" = {
    output1 <- check_if_close(dataset1     = gpsdata.fake,       
                              dataset2     = bus_stops.fake,     
                              n.splits     = 1000,
                              desired.dist = .2)
  },
  "2" = {
    output2 <- check_if_close2(dataset1     = gpsdata.fake,       
                               dataset2     = bus_stops.fake,
                               margin       = 0.0025,
                               desired.dist = .2)
    
  },
  "3" = {
    output3 <- check_if_close3(dataset1     = gpsdata.fake,       
                               dataset2     = bus_stops.fake,     
                               margin       = 0.0025,
                               desired.dist = .2)
    
    
  },
  replications = 1,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self"))

benchmarks