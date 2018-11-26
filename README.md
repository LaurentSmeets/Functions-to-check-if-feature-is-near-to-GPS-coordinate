# Functions to check if feature is near to GPS coordinate
For my thesis project I need to check for millions of GPS coordinates (split in different trips of a few thousand GPS coordinates each) whether or not they are near to a bus stop in the Netherlands. I wrote some code that does this, but that was (well still is) very slow at doing so. To get some tips I posted my question on Stackoverflow [here](https://stackoverflow.com/questions/53212103/how-to-efficiently-calculate-distance-between-gps-points-in-one-dataset-and-gps), and got some very useful tips and code. Using these tips and code I created 3 new versions of the function.
1. **Function 1** uses a devide and conquer strategy that is much faster than the original code and does not suffer from memory overflow problems when the file gets too large. 
2. **Function 2** is much, much, much faster, by using a full data.table implementation, but when there are too many features (bus stops, train stations, etc.) that need to be matched with the GPS coordinates it can lead to the creation of such large data.tables that the memory can not hold it all at once
3.  **Function 3** is 90% based on the code proposed by [Hugh](https://stackoverflow.com/users/1664978/hugh) on Stackoverflow. This code is even faster, by using data.table's rolling joins, but only checks is a feature is near, not if multiple features are near.

The *compare functions* file holds the file that creates some fake GPS coordinates and all the packages that are needed to run the functions. The other four files are the different versions of the functions.

with a 100,000 GPS points and 5000 features this is how the different functions preform on my laptop:

```
#  test replications elapsed relative user.self sys.self
#1    0            1  198.48 1044.632    189.82     8.28
#2    1            1   29.93  157.526     29.69     0.45
#3    2            1    7.94   41.789      7.36     0.94
#4    3            1    0.19    1.000      0.19     0.00
```

If, instead of using simulated GPS coordinates of busstops in the Netherlands, you want to use the actual data, you can load them  by using this code:
```r
temp <- tempfile()
download.file("http://data.openov.nl/haltes/stops.csv.gz", temp) 
gzfile(temp, 'rt')
busstopdata <- read.csv(temp, stringsAsFactors = FALSE)
unlink(temp)
bus_stops <- fread("bus_stops.csv")
busdata.data <- busstopdata %>%
  mutate(latitude_bustops = latitude)%>%
  mutate(longitude_bustops = longitude)%>%
    dplyr::select(name, latitude_bustops,  longitude_bustops)
```
