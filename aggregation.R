########################
# klimDWD: aggregation.R

# load libraries
library(dplyr)
library(tidyr)

# load pre-procesed data ----
load("mergedDF.RData")
dat <- merged.df


# scaling to time-window and making a list
sub.df <- merged.df %>% filter(.,date.start>=19500101) # needed some data are not collinear!


# calculate seasonal deviations from raw rainfall data
sub.df$monthly.means <-with(sub.df, ave(precip,list(month,station.id), 
                                        FUN=mean))
sub.df$monthly.anomaly.perc <- with(sub.df, ave(precip, list(month,station.id), 
                                                FUN=function(x) (x-mean(x))/mean(x)*100))


# prepare list for results
sub.df.process <- sub.df %>% select(.,station.id,date.start,date.end,precip,frameID,counter,
                                    month,year,lat,lon,locale,monthly.means,monthly.anomaly.perc)

sub.list <- split(sub.df.process,sub.df.process$frameID)


# spatial interpolation using akima
library(akima)


# interpolate data
results <- list()

i=1

for (i in i:length(sub.list)){
  results[[i]]<-with(sub.list[[i]], interp(x = lon, y = lat, z = monthly.anomaly.perc, duplicate = "mean"))
}


# keep track of frames and Dates :: new approach a list of lists
mylist.names <- c("framesCounter", "dates","yearsCnt")
framesDates <- vector("list", length(mylist.names))
names(framesDates) <- mylist.names

i=1
for (i in i:length(sub.list)){
  framesDates$framesCounter[[i]]<-sub.list[[i]]$counter
  framesDates$dates[[i]]<-sub.list[[i]]$year
  framesDates$yearsCnt[[i]]<-with(sub.list[[i]],mean(sub.list[[i]]$year))
}



# output of image files to dir
where<-getwd()

i=1
for (i in i:length(results)){
  png(filename=paste0(where,"/maps150/",i,"anom.png"))
  filled.contour(x = results[[i]]$x,
                 y = results[[i]]$y,
                 z = results[[i]]$z,
                 color.palette =
                   colorRampPalette(c("white", "blue")),zlim=c(-100,200),xlim=c(6,15),ylim=c(47.5,55),
                 xlab = "Longitude",
                 ylab = "Latitude", main=paste0(framesDates$yearsCnt[[i]], " and frameCounter ",framesDates$framesCounter[[i]][1]),
                 key.title = title(main = "dD (%)", cex.main = 1))
  dev.off()      
  
}
