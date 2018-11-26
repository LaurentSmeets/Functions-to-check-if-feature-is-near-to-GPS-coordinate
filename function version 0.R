check_if_close_original <- function(dataset1,       
                                    dataset2,      
                                    desired.dist = .2){
  
  # 1) we only select the variables we need from dataset 1
  dataset1 <- setDT(dataset1)[,c("id", "device_id", "latitude", "longitude")]
  setnames(dataset1, old = c("id", "latitude", "longitude"), new = c("id_dataset1", "latitude_gps", "longitude_gps"))
  
  # 2) we only select the variables we need from dataset 2
  dataset2 <- setDT(dataset2)[,c("id", "name", "latitude", "longitude")]
  setnames(dataset2, old = c("id", "latitude", "longitude"), new = c("id_dataset2", "latitude_feature", "longitude_feature"))
  
  # 3) only keep subet of dataset2 that falls within dataset 1. 
  #    There is no reason to check if features are close that already fall out of the GPS coordinates in the trip we want to check
  #    We do add a 0.01 point margin around it to be on the save side. Maybe a feature falls just out the GPS coordinates, 
  #    but is still near to a GPS point
  dataset2 <- dataset2[latitude_feature  %between%  (range(dataset1$latitude_gps) + c(-0.01, +0.01)) 
                       & longitude_feature %between% (range(dataset1$longitude_gps) + c(-0.01, +0.01)), ]
  


  checkdatapoint <- function(p) {
  distances <- spDistsN1(data.matrix(dataset1[,c("longitude_gps","latitude_gps")]), 
                         p,
                         longlat=TRUE) # in km
  return(which(distances <= .2)) # distance is now set to 200 meters
}


# code to check per data point if a bus stop is near and save this per bus stop in a list entry
  datapoints.near.feature       <- apply(data.matrix(dataset2[,c("longitude_feature","latitude_feature")]), 1, checkdatapoint)


# rename list entries
names(datapoints.near.feature) <- dataset2$name

# melt list into one big data.frame
long.datapoints.near.feature      <- melt(datapoints.near.feature)

# now switch to data.table grammar to speed up process
# set data.table
setDT(dataset1)
dataset1[, rowID := 1:nrow(dataset1)]
setkey(dataset1, key = "rowID")
setDT(long.datapoints.near.feature)

# merge the data, and filter non-unique entries 
setkey(long.datapoints.near.feature, key = "value")
dataset1.joined        <- merge(x = dataset1, y = long.datapoints.near.feature, by.x= "rowID", by.y= "value", all.x=TRUE)
dataset1.joined.unique <- unique(dataset1.joined, by="id_dataset1") # mak

# this last part of the code is needed to make sure that if there are more than 1 bus stop nearby it puts these bus stop in a list
# instead of adding row and making the final data.frame longer than the original one
GPS.joined.unique2 <- setDT(dataset1.joined.unique)[order(id_dataset1, L1), list(L1=list(L1)), by=id_dataset1]
GPS.joined.unique2[, nearby := TRUE][is.na(L1), nearby := FALSE] # add a dummy to check if any bus stop is nearby.

return(GPS.joined.unique2)

} 
