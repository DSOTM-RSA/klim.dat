# we can plot all files together or one at time. To plot
# many files together, put them all in one folder. To 
# plot one at a time, just put that one file in a folder
# by itself

# BEFORE analysing the raw data with R, make a copy of
# the data txt file that you got from the instrument, 
# then open the copy and delete all the lines above the 
# data. So delete all the lines above and including 
# the line that starts with “line”. If you’ve done this 
# correctly the first line of your text file wil look 
# something like this (without the #) :

# 0592991.82  5376877.04 -000013  50779.60 -3638.27 59  000000.00 09 181120.0  445  0 

# What is the full path to the folder containing your file(s)?
my_dir <- "E:/My Documents/My UW/Teaching/Independant Study Students/AY13-14/WI14 Deanna De Boer/temp"
my_files <- list.files(my_dir, pattern = "txt", full.names = TRUE)


# convert Gradiometer raw data text files into data frames
input_gradio <- function(my_file){
  xx <- scan(my_file, what = "character")
  df <- data.frame()
  df <- data.frame(matrix(ncol = 11))
  sequ <- seq(1, length(xx), 11)
  # every 11 items to new row
  for(i in 1:((length(xx)/11)-1)){
    print(paste0("reading line ", i))
    df[i,] <- xx[sequ[i]:(sequ[i]+10)] 
    }
  # if we can't reshape it (less than 2 lines, etc), skip to next file
  # error handling - skips to next file if it gets an error
    df <- data.frame(apply(df, 2, as.numeric))
    # subset just X, Y and mag (easting, northing and mag data)
 df <- tryCatch(df[,c(1,2,5)], error=function(e) NA)
if(class(df) == "NA"){ next } else {df}
}

# get data from all files
my_dfs <- lapply(my_files, input_gradio)
# get rid of malformed files
my_dfs <- my_dfs[!is.na(my_dfs)]
# combine them into one big df
my_df <- do.call(rbind, my_dfs)

# rename a bit
data2 <- my_df
data2$V2 <- data2$X1
data2$V3 <- data2$X2
data2 <- data2[-1,]

# have a bit of a look for outliers, etc
hist(data2$X5, breaks = 1000) # it's highly peaked with huge tails

# first we can exclude the obviously extreme values
data2 <- data2[data2$X5 < 9999 & data2$X5 > -9999,]

# have a bit of a look again....
hist(data2$X5, breaks = 1000)

# Now we have two options for further cleaning the data
# Choose one and then run the rest 
# of the code. Then try the other one

############## first option ###########################
# First, just cut off some of the extreme values
# mean(data2$X5); sd(data2$X5)
# let's the n standard deviations from the mean and exlcude the rest
n <- 4 # change this to 3 and 4 and see how you like it
three_sd <- (sd(data2$X5)* n) + mean(data2$X5)
data2 <- data2[data2$X5 < three_sd & data2$X5 > -three_sd,]
# now skip the second option and run all the code below

############## second option ##########################
# Second, exlude the very most extreme values
data2 <- data2[data2$X5 < 9999 & data2$X5 > -9999,]
# the take logs to compress the range
 idx <- sign(data2$X5) == -1
 data2$X5 <- log(abs(data2$X5))
 data2$X5[idx] <- -data2$X5[idx]
# remove non-numbers
data2 <- data2[!data2$X5 == -Inf,]


####### to switch options, start again  ###################
####### from "data2 <- my_df" above     ###################


# have another look at the distribution
hist(data2$X5, breaks = 1000)

# get limits of survered area for mapmaking
xmn=min(data2[,1]); xmx=max(data2[,1])
ymn=min(data2[,2]); ymx=max(data2[,2])


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
# check to see the proportion of the raster to size the plot appropriately
pro <- (xmx-xmn) / (ymx-ymn)

r <- raster(nrows=100, ncols=100, 
            xmn=xmn, xmx=xmx, 
            ymn=ymn, ymx=ymx )
ras <- rasterize(data2[,1:2], r, field = data2[,3])
# make a plot of the data
plot(ras, xlim= c(xmn, xmx), ylim = c(ymn, ymx), asp = 1)


## This is ok, but we want to fill in the gaps in the plot
## to get a nice continuous surface of data. So we need to 
## interpolate. Let's do that...

# plot as interpolated raster using base plotting functions
library(akima)
akima.li <- interp(data2[,1], data2[,2], data2[,3], duplicate = "median",
                   xo=seq(xmn,xmx, length=150),
                   yo=seq(ymn,ymx, length=150))
# plot data collection points
plot(data2[,2] ~ data2[,1], data = data2, pch = 3, cex = 0.5,
     xlab = "Easting", ylab = "Northing", asp = 1)
# Option 1: plot interpolated raster over the top
image(akima.li, add=TRUE, col = rainbow(100, alpha = 1))
# Option 2: plot interpolated contour over the top
contour(akima.li, add=TRUE)
# Option 3: plot data collection points over the top
points(data2[,1], data2[,2], pch = 3, cex = 0.25)

# to save one of these plots individually:
png() # begin saving plot...
plot(data2[,2] ~ data2[,1], data = data2, pch = "", cex = 0.5,
     xlab = "Easting", ylab = "Northing", asp = 1)
# uncomment the line below to include in the plot
 image(akima.li, add=TRUE, col = rainbow(100, alpha = 1))
# contour(akima.li, add=TRUE)
# points(data2[,1], data2[,2], pch = 3, cex = 0.25)
dev.off() # ... end saving plot
# where is the file you just made?
getwd() # here


# Using ggmap and google maps to plot data on a basemap...
require(ggmap)
# get base map at appropriate zoom scale
dataMap <-  get_map(c(data2$long[1], data2$lat[1]), 
                    color = "color", 
                    zoom = 17, # change this to zoom in or out!
                    source = "google", 
                    maptype =  'satellite')
# view the map you just got to check it's the right on
ggdataMap

# create data layer
ggdataMap <- ggmap(dataMap, base_layer = ggplot(aes(x = long, y = lat), data = data2))

# plot data and base map
ggdataMap + geom_point(data=data2, aes(x=long, y=lat, color = X5))

# plot interpolated raster with ggplot, using lat-longs, without base map
# useful for spotting points of interest
data4 <- data.frame(expand.grid(X=akima.li$x, Y=akima.li$y), z=c(akima.li$z))
ggplot(data4) + 
  geom_tile(aes(X, Y, fill=z)) + 
  scale_fill_gradient(low="white", high="black", space = "Lab") + 
  coord_fixed(ratio = 1)
