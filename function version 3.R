# this app could be much faster if it would filter by duplicate GPS coordinates

check_if_close3 <- function(dataset1,       
                            dataset2,     
                            desired.dist = .2,
                            margin       = 0.0025){
  
  # dataset1 needs at least the columns 
  #  - "id", 
  #  - "device_id"
  #  - "latitude"
  #  - "longitude"
  
  # dataset2 needs at least the columns 
  #  - "id", 
  #  - "name"
  #  - "latitude"
  #  - "longitude"
  
  # the margin should not be too small, because that will lead to the missing of matches. 
  

  # 1) we only select the variables we need from dataset 1
  dataset1_orig <- copy(dataset1)
  #dataset2_orig <- copy(dataset2)
  
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
  
  
  # 4) we copy the original coordinates and add the margin for longitude and latitude
  setkey(dataset1, latitude_gps)
  
  dataset1[, gps_lat := latitude_gps + 0]
  dataset1[, gps_lon := longitude_gps + 0]

  
  dataset2[, lat := latitude_feature  + 0]
  dataset2[, lon := longitude_feature + 0]
  

  setkey(dataset2, lat)
  
  #dataset1_by_lat <- dataset1[, .(id), keyby = "gps_lat"]
  
  
  By_latitude <- 
    dataset2[dataset1, 
                 on = "lat==latitude_gps",
                 
                 # within margin of latitude
                 roll = margin, 
                 # +/-
                 rollends = c(TRUE, TRUE),
                 
                 # and remove those beyond 0.5 degrees
                 nomatch=0L] %>%
    .[, .(id_lat = id_dataset1,
          name = name,
          latitude_feature = latitude_feature,
          longitude_feature = longitude_feature,
          gps_lat,
          gps_lon),
      keyby = .(lon = gps_lon)]
  
  setkey(dataset2, lon)
  
  By_latlon <-
    dataset2[By_latitude,
                 on = c("name", "lon"),
                 
                 # within margin of longitude
                 roll = margin, 
                 # +/-
                 rollends = c(TRUE, TRUE),
                 # and remove those beyond 0.5 degrees
                 nomatch=0L]
  
  By_latlon[, distance := haversine_distance(lat1 = gps_lat, 
                                             lon1 = gps_lon,
                                             lat2 = latitude_feature,
                                             lon2 = longitude_feature)]
  
  valuesnearstop<-By_latlon[distance < desired.dist]
  
  setkey(valuesnearstop, id_lat)
  setkey(dataset1_orig, id)
  
  
  finallist <- valuesnearstop[dataset1_orig][, nearby := TRUE][is.na(name), nearby := FALSE][, .(lat=latitude, lon=longitude, nearby=nearby, device_id=device_id, id=id_lat),]
  
  # 12) if a point is near multiple features at once these are listed in a list,
  #     instead of having duplicate rows with the same id but different features
  #finallist <- fulldata[, .(lat=latitude, lon=longitude, nearby=nearby, device_id=device_id, id=id_lat),]
  
  return(finallist)
}