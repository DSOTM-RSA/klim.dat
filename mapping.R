# load libraries
library(dplyr)
library(tidyr)

# aggregation ----
load("mergedDF.RData")
dat <- merged.df

# scaling to time-window and making a list
sub.df <- merged.df %>% filter(.,date.start>=18500101)

# calculate seasonal deviations from raw rainfall data
sub.df$monthly.means <-with(sub.df, ave(precip,list(month,station.id), FUN=mean))
sub.df$monthly.anomaly.perc <- with(sub.df, ave(precip, list(month,station.id), FUN=function(x) (x-mean(x))/mean(x)*100))


# prepare list for results
sub.df.process <- sub.df %>% select(.,station.id,date.start,date.end,precip,frameID,month,year,lat,lon,locale,monthly.means,monthly.anomaly.perc)

sub.list <- split(sub.df.process,sub.df.process$frameID)



# spatial interpolation using akima
library(akima)

# interpolation results
results <- list()

i=1

for (i in i:length(sub.list)){
  results[[i]]<-with(sub.list[[i]], interp(x = lon, y = lat, z = monthly.anomaly.perc, duplicate = "mean"))
}

i=1

for (i in i:length(sub.list)){
  results[[i]]<-sub.list[[i]]$year
}


# when it happend old approach
when <- list()
i=1

for (i in i:length(sub.list)){
  when[[i]]<-with(sub.list[[i]], mean(sub.list[[i]]$year))
}


# new approach a list of lists
mylist.names <- c("frames", "dates")
mylist <- vector("list", length(mylist.names))
names(mylist) <- mylist.names

i=1
for (i in i:length(sub.list)){
  mylist$frames[[i]]<-sub.list[[i]]$frameID
  mylist$dates[[i]]<-sub.list[[i]]$year
}







# output of image files to dir
where<-getwd()

i=1
for (i in i:length(results)){
  png(filename=paste0(where,"/mapsnew2/",i,"anom.png"))
  filled.contour(x = results[[i]]$x,
                 y = results[[i]]$y,
                 z = results[[i]]$z,
                 color.palette =
                   colorRampPalette(c("white", "blue")),zlim=c(-100,200),xlim=c(6,15),ylim=c(47.5,55),
                 xlab = "Longitude",
                 ylab = "Latitude", main=paste0(when[[i]], ""),
                 key.title = title(main = "Rain (mm)", cex.main = 1))
  dev.off()      
  
}