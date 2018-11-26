# this app could be much faster if it would filter by duplicate GPS coordinates

check_if_close <- function(dataset1,       
                           dataset2,      
                           n.splits     = 500,
                           desired.dist = .2){
  
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
  
  # these are the average coordinates of the Netherlands. A change of ,.0017 in latitude leads to a change of 189 meters 
  # spDistsN1(matrix(c(5.2913, 52.1326), ncol=2), matrix(c(5.2913, 52.1326+.0017), ncol=2), longlat=TRUE)*1000
  # [1] 189.1604
  # this means that the latitude slices we can cut (the subsection of) the Netherlands is have to be at least .0017 wide.
  # if we look at the Netherlands a whole this would mean we can use max  (53.5-50.5)/.0017 = 1765 slices.
  # if we look only at a small subsection (because we are only looking a a single trip for example we need much less slices.  
  
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
  
  # 4) we cut the dataset2 into slices on the latitude dimension
  #    some trial  and error is involved getting the right amount. if you add to many you get a large and redudant amount of empty values
  #    if you add to few you get you need to check too many GPS to feauture distances per slice
  
  
  
  dataset2[, range2 := as.numeric(Hmisc::cut2(dataset2$latitude_feature, g=n.splits))]
  
  # 5) calculate the ranges of the slices we just created
  ranges <- dataset2[,list(Min=min(latitude_feature), Max= max(latitude_feature)), by=range2][order(range2)]
  setnames(ranges, old = c("range2", "Min", "Max"), new = c("latitude_range", "start", "end"))
  
  
  # 6) now we assign too which slice every GPS coordinate in our dataset1 belongs
  #    this is super fast when using data.table grammar
  elements1 <- dataset1$latitude_gps
  ranges <- setDT(ranges)[data.table(elements1), on = .(start <= elements1, end >=elements1)]
  ranges[, rowID := seq_len(.N)]
  dataset1[,rowID := seq_len(.N)]
  setkey(dataset1, rowID)
  setkey(ranges, rowID)
  dataset1<-dataset1[ranges]
  
  # 7) this is the actual function we use to check if a datapoint is nearby.
  #    potentially there are faster function to do this??
  checkdatapoint <- function(p, h, dist=desired.dist) {
    distances <- spDistsN1(data.matrix(filter(dataset1,latitude_range==h)[,c("longitude_gps","latitude_gps")]), 
                           p,
                           longlat=TRUE) # in km
    return(which(distances <= dist)) # distance is now set to 200 meters
  }
  
  # 8) we assign a ID to the dataset1 starting again at every slice.
  #    we need this to later match the data again 
  dataset1[, ID2 := sequence(.N), by = latitude_range]
  
  # 9) here we loop over all the splits and for every point check if there is a feature nearby in the slice it falls in
  #    to be on the save side we also check the slice left and right of it, just to make sure we do not miss features that
  #    are nearby, but just fall in a different slice.
  #         9a: create an empty list we fill with dataframes later 
  TT<-vector("list", length=n.splits)
  #         9b: loop over the number of slices using above defined function
  
  for(i in 1:n.splits){
    datapoints.near.feature<-apply(data.matrix(dataset2[range2 %in% c(i-1,i, i+1), c("longitude_feature","latitude_feature")]), 1, checkdatapoint, h=i)
    #         9c: if in that slice there was no match between a GPS coordinate and an nearby feature, we create an empty list input
    if(class(datapoints.near.feature)=="integer"|class(datapoints.near.feature)=="matrix"){
      TT[[i]] <-NULL
    } else {
      #         9d: if there was a match we get a list of data point that are named
      names(datapoints.near.feature)    <- dataset2[range2 %in% c(i-1,i, i+1), name]
      #         9e: then we 'melt' this list into  data.frame
      temp <- melt(datapoints.near.feature)
      #         9f: then we transform it into a data.table and change the names
      setDT(temp)
      setnames(temp, old=c("value", "L1"), new= c("value", "feature_name"))
      #         9h: then we only select the data point in dataset1 that fall in the current slice give them an 
      #             ID and merge them with the file of nearby busstops
      gpsdata.f <- dataset1[latitude_range==i, ]
      gpsdata.f[, rowID2 := seq_len(.N)]
      setkey(gpsdata.f, key = "rowID2")
      setkey(temp, key = "value")
      GPS.joined.temp <- merge(x = gpsdata.f, y = temp, by.x= "rowID2", by.y= "value", all.x=TRUE)
      #         9i: we only keep the unique entries and for every slice save them to the list
      GPS.joined.unique.temp <- unique(GPS.joined.temp, by=c("id_dataset1", "feature_name"))
      TT[[i]] <-  GPS.joined.unique.temp 
      cat(paste0(round(i/n.splits*100), '% completed'), " \r"); flush.console()
      
      
      #cat(i/n.splits*100, " \r"); flush.console()
      
    }
    
  }
  
  # 10) now we left join the original dataset and and the data point that are near a feature
  #     However if there are no matches between GPS features, it is ofcourse not possibe to merge anything,
  #     so we just keep the original dataset in those cases.
  y.table <- rbindlist(TT[vapply(TT, Negate(is.null), NA)])
  if(nrow(y.table) == 0){
    finallist = dataset1
    finallist[, feature_name:=NA]
    finallist[, nearby:=FALSE]
    
  }else{
  finallist<- merge(x = dataset1, 
                    y = y.table, 
                    by.x= "id_dataset1", 
                    by.y= "id_dataset1", 
                    all.x=TRUE)
  
  # 11) we add a new logical variable to check if any bus stop is near
  finallist[, nearby := TRUE][is.na(feature_name), nearby := FALSE] # add a dummy to check if any bus stop is nearby.
  
  # 12) if a point is near multiple features at once these are listed in a vector,
  #     instead of having duplicate rows with teh same id but different features
  finallist <- unique(setDT(finallist)[order(id_dataset1, feature_name), list(feature_name=list(feature_name), id=id_dataset1, lat=latitude_gps.x, lon=longitude_gps.x, nearby=nearby), by=id_dataset1], by="id_dataset1")
  }
  return(finallist)
}