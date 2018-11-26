# this app could be much faster if it would filter by duplicate GPS coordinates

check_if_close2 <- function(dataset1,       
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
  
  dataset2[, feature.lat.minus := latitude_feature  - margin]
  dataset2[, feature.lat.plus  := latitude_feature  + margin]
  
  setkey(dataset1, latitude_gps, gps_lat)
  setkey(dataset2, feature.lat.minus, feature.lat.plus)
  

  
  
  check<-foverlaps(dataset1, dataset2, nomatch = 0L, type="within")[, .(longitude_feature=longitude_feature,
                                                                        latitude_feature=latitude_feature,
                                                                        name=name,
                                                                        gps_lat=latitude_gps,
                                                                        gps_lon=longitude_gps,
                                                                        gps_lon2=longitude_gps,
                                                                        id=id_dataset1,
                                                                        device_id=device_id)]


  
  
  
  dataset2[, longitude_feature_minus:= longitude_feature - margin]
  dataset2[, longitude_feature_plus := longitude_feature  + margin]

  
  setkey(check, gps_lon, gps_lon2)
  setkey(dataset2, longitude_feature_minus, longitude_feature_plus)
  
  check2 <- foverlaps(check, dataset2, nomatch = 0L, type="within")[, .(longitude_feature=longitude_feature,
                                                                        latitude_feature=latitude_feature,
                                                                        name=name,
                                                                        gps_lat=gps_lat,
                                                                        gps_lon=gps_lon,
                                                                        id=id,
                                                                        device_id=device_id)]
  
  
  # 5) this is the actual function we use to check if a datapoint is nearby, by using the haversine method
  valuesnearstop <- unique(check2[, distance := haversine_distance(lat1 = gps_lat, 
                                                                   lon1 = gps_lon,
                                                                   lat2 = latitude_feature,
                                                                   lon2 = longitude_feature)][distance < desired.dist], by=c("name", "id"))
  
  # 6) now we left join the original dataset and and the data point that are near a feature
  # 7) we add a new logical variable to check if any bus stop is near
  
  setkey(dataset1_orig, "id")
  setkey(valuesnearstop, "id")
  
  # add a dummy to check if any bus stop is nearby.
  
  fulldata <- valuesnearstop[dataset1_orig][, nearby := TRUE][is.na(name), nearby := FALSE]
  
  # 8) if a point is near multiple features at once these are listed in a list,
  #     instead of having duplicate rows with the same id but different features
  finallist <- unique(setDT(fulldata)[order(id, name), list(feature_name=list(name), lat=latitude, lon=longitude, nearby=nearby), by="id"], by="id")

  return(finallist)
}