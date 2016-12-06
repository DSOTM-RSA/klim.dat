
# load libraries
library(dplyr)
library(tidyr)



# read in and process files ----
files <- dir(pattern = '*.txt', full.names = TRUE)

raw.df <- files %>%  lapply(., read.table,sep=";",header=TRUE,fill=TRUE) %>% 
  lapply(., function(x) x[complete.cases(x),]) %>%  lapply(.,"[",1:6) %>% dplyr::bind_rows(.) %>% 
  .[complete.cases(.),] %>% setNames(c('station.id','date.start','date.end','quality','type','precip'))

# get frame intervals from date.start
date.str <- as.character(raw.df$date.start)
date.num<-substr(date.str, 1, 6) %>% as.double(.)

frame <- mutate(raw.df,frameID = date.num-date.num[1]) # get working frame

# convert factors to numeric and fix NaN
frame$station.id <- as.factor(frame$station.id)
frame$station.id <- as.numeric(frame$station.id)
frame$precip[frame$precip==-999] <- NA
frame$precip[frame$precip<=0] <- NA

str(frame)

# change working dir here in dev mode #

# merging with station meta data
#raw.meta <-read.table("Demonstration/stat.csv",header=TRUE, sep =",")
raw.meta <-read.table("Demonstration/stations-single-all-meta.csv",header = TRUE, sep = ",")
raw.meta$station.id <-raw.meta$stat_id %>% as.numeric(.)

library(data.table)

dat.var = as.data.table(frame)
dat.meta = as.data.table(raw.meta)

merged.df.tmp<-merge(dat.var,dat.meta, by="station.id")

# suspect the ids are inconsistently numbered
frame.ids<-sort(unique(frame$station.id))
meta.ids<-sort(unique(raw.meta$station.id))

# adjusting meta to match what lies in frame
meta.ids.adj <- frame.ids
dat.meta$station.id <- meta.ids.adj

merged.df<-merge(dat.var,dat.meta, by="station.id")

merged.df$precip[is.na(merged.df$precip)] <- 0
summary(merged.df)

rm(merged.df.tmp) # rm temporary file

save(merged.df,file = "mergedDF.RData") # save merged file for later load

rm(dat.meta,dat.var,frame,merged.df,raw.df,raw.meta,date.num,date.spl,date.str,files,frame.ids,meta.ids,meta.ids.adj)

# plotting ----

library(sp)

load("mergedDF.RData")
dat <- merged.df


coordinates(dat) <- ~lon+lat
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")

library(ggmap)    # loads ggplot2 as well
map <- get_map(location=rowMeans(bbox(dat)), zoom=5)   # get Google map
ggmap(map) + 
  geom_point(data=as.data.frame(dat), aes(lon,lat,fill=height), 
             color="grey70", size=0.5, shape=21) +
  scale_fill_gradientn(colours=rev(heat.colors(5)))


out <- split(re.merged.df,as.factor(re.merged.df$frameID)) # get into a list : apply interp to this...



# sclaing to time-window and making a list
sub.df <- merged.df %>% filter(.,date.start>=18500101)
sub.list <- split(sub.df,sub.df$frameID)
results <- list()


# interpolation
library(akima)


i=1

for (i in i:length(sub.list)){
  results[[i]]<-with(sub.list[[i]], interp(x = lon, y = lat, z = precip, duplicate = "mean"))
  
}



# output of image files to dir

where<-getwd()

i=1
for (i in i:length(results)){
  png(filename=paste0(where,"/mapsnew/",i,"out.png"))
      filled.contour(x = results[[i]]$x,
                     y = results[[i]]$y,
                     z = results[[i]]$z,
                     color.palette =
                       colorRampPalette(c("white", "blue")),zlim=c(0,160),xlim=c(6,15),ylim=c(47.5,55),
                     xlab = "Longitude",
                     ylab = "Latitude", main=paste0(i, ""),
                     key.title = title(main = "Rain (mm)", cex.main = 1))
      dev.off()      
  
}


