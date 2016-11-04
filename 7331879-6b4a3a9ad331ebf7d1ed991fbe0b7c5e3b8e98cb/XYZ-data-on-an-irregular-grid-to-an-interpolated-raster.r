## Working with remote sensing data from GEM Gradiometer

# First get data from machine onto a computer. Here we've got the 
# data on a google sheet...

# get data from google drive... connect to google sheet
require(RCurl) # if you get an error message here that says something like 'there is no package called 'RCurl''
# then you need to install the package by typing this into the console (and then hitting enter): install.packages("RCurl")
# wait for the package to download and install, then run line 3 again before proceeding.
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
# if the file is in google drive, it needs to be opened in google sheets first then
# in google spreadsheet, go to file-> publish to web -> get link to publish to web -> get csv file
# this is the URL for "15-18"
goog <- "https://docs.google.com/spreadsheet/pub?key=0AiSgZmtbWmWmdGQ2UDRoZmpwTkZFT3NYbjVmcXZkRUE&output=csv"
# assign data from google sheet to R object - this may take some time
data <- read.csv(textConnection(getURL(goog)), 
                 stringsAsFactors = FALSE, 
                 header = FALSE) # if the data have column names then change this to TRUE
# inspect to confirm that data has imported OK...
head(data)
# subset just X, Y and mag (easting, northing and mag data)
data1 <- data[,c(2,3,6)]
# convert all to numeric for computation           
data1 <- na.omit(data.frame(apply(data1, 2, function(x) as.numeric(as.character(x)))))
# find max and min values for later steps
xmn=min(data1[,1]); xmx=max(data1[,1])
ymn=min(data1[,2]); ymx=max(data1[,2])


# Plot points over google maps
# just a sample as it's rather slow!
# n <- 100
# data2 <- data1[sample(nrow(data1), n), ]
data2 <- data1

# convert UTM to latlong
require(sp); require(rgdal)
a<-data.frame(cbind(data2$V2, data2$V3))
coordinates(a) = ~X1 + X2
proj4string(a) <-CRS("+proj=utm +zone=10 +datum=WGS84")
# use spTransform now
a1 <- spTransform(a,CRS("+proj=longlat"))
# inspect output
head(coordinates(a1)) 
# insert lat-longs back into data
data2$long <-  a1$X1
data2$lat <-   a1$X2

# plot as raster
library(raster) # if you get an error, type install.packages("raster") at the console
r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
ras <- rasterize(data1[,1:2], r, field = data1[,3])
# make a plot of the data
plot(ras)

## This is ok, but we want to fill in the gaps in the plot
## to get a nice continuous surface of data. So we need to 
## interpolate. Let's do that...

# plot as interpolated raster
library(akima)
akima.li <- interp(data1[,1], data1[,2], data1[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=100),
                   yo=seq(ymn,ymx, length=100))
# plot data collection points
plot(data1[,2] ~ data1[,1], data = data1, pch = 3, cex = 0.5,
     xlab = "Easting", ylab = "Northing")
# plot interpolated raster over the top
image(akima.li, add=TRUE, col = rainbow(100, alpha = 1))
# plot interpolated contour over the top
contour(akima.li, add=TRUE)
# plot data collection points over the top
points(data1[,1], data1[,2], pch = 3, cex = 0.25)


# Using ggmap and google maps to plot data on a basemap...
require(ggmap)
# get base map at appropriate zoom scale
dataMap <-  get_map(c(data2$long[1], data2$lat[1]), 
                    color = "color", 
                    zoom = 20, 
                    source = "google", 
                    maptype =  'satellite')
# create data layer
ggdataMap <- ggmap(dataMap, base_layer = ggplot(aes(x = long, y = lat), data = data2))
# plot data and base map
ggdataMap + geom_point(data=data2, aes(x=long, y=lat, color = V6))

# plot interpolated raster with ggplot, using lat-longs, without base map
data4 <- data.frame(expand.grid(X=akima.li$x, Y=akima.li$y), z=c(akima.li$z))
ggplot(data4) + 
  geom_tile(aes(X, Y, fill=z)) + 
  scale_fill_gradient(low="white", high="black", space = "Lab") + 
  coord_fixed(ratio = 1)

# convert UTM to lat-long again... for the newly interpolated data
# convert UTM to latlong
require(sp); require(rgdal)
a<-data.frame(cbind(data4$X, data4$Y))
coordinates(a) = ~X1 + X2
proj4string(a) <-CRS("+proj=utm +zone=10 +datum=WGS84")
# use spTransform now
a1 <- spTransform(a,CRS("+proj=longlat"))
# inspect output
head(coordinates(a1)) 
# insert lat-longs back into data
data4$long <-  a1$X1
data4$lat <-   a1$X2

# now plot interpolated data onto base map
require(ggmap)
# get base map at appropriate zoom scale
data4 <- na.omit(data4)
dataMap <-  get_map(c(data4$long[1], data4$lat[1]), 
                    color = "color", 
                    zoom = 20, 
                    source = "google", 
                    maptype =  'satellite')
# draw map                    
ggmap(dataMap) %+% data4 + 
  aes(x = long,
      y = lat,
      z = z) +
  stat_summary2d(bins = 250) + 
  scale_fill_gradientn(name = "Median",
                       space = "Lab",
                       colours = rainbow(7)) + 
  labs(x = "Longitude",
       y = "Latitude") 



# consider https://groups.google.com/forum/#!topic/ggplot2/8c6eYP9r9F0
# and http://kohske.wordpress.com/2011/04/01/alpha-version-of-colorbar-legend-in-ggplot2/