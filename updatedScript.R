
# load libraries
library(dplyr)
library(tidyr)

where<-getwd()

# read in and process files ----
files <- dir(pattern = '*.txt', full.names = TRUE)

pro.df <- files %>%  lapply(., read.table,sep=";",header=TRUE,fill=TRUE) %>% 
  lapply(., function(x) x[complete.cases(x),]) %>%  lapply(.,"[",1:6) %>% dplyr::bind_rows(.) %>% 
  .[complete.cases(.),] %>% setNames(c('station.id','date.start','date.end','quality','type','precip'))

# adjusting strings
date.str <- as.character(pro.df$date.start)
 
date.beg <-as.character(pro.df$date.start) %>% as.Date(., format='%Y%m%d')
diff_in_days = difftime(date.beg, date.beg[1], units = "days")
diff_in_years = as.double(diff_in_days)/365 # absolute years
abs.years<-floor(diff_in_years)

months_diff = as.double(substring(date.str, 5, 6)) - as.double(substring(date.str[1], 5, 6))
total_months = floor(diff_in_years)*12 + months_diff

frame <- mutate(pro.df,frameID = total_months) %>% .[-114369,] # trim to remove erroneous last obs

# convert factors to numeric and fix NaN
frame$station.id <- as.factor(frame$station.id)
frame$station.id <- as.numeric(frame$station.id)
frame$precip[frame$precip==-999] <- NA
frame$precip[frame$precip<=0] <- NA

# merging with station meta data
stat.meta<-read.table("Demonstration/stat.csv",header=TRUE, sep =",")
stat.meta$station.id <-stat.meta$stat_id %>% as.numeric(.)

library(data.table)

dat.var = as.data.table(frame)
dat.meta = as.data.table(stat.meta)

merged.df<-merge(dat.var,dat.meta, by="station.id")

merged.df$precip[is.na(merged.df$precip)] <- 0


# plotting ----

library(sp)

dat <- merged.df
coordinates(dat) <- ~lon+lat
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")

library(ggmap)    # loads ggplot2 as well
map <- get_map(location=rowMeans(bbox(dat)), zoom=5)   # get Google map
ggmap(map) + 
  geom_point(data=as.data.frame(dat), aes(lon,lat,fill=height), 
             color="grey70", size=3.5, shape=21) +
  scale_fill_gradientn(colours=rev(heat.colors(5)))


out <- split(merged.df,as.factor(merged.df$frameID)) # get into a list : apply interp to this...

# interpolation
library(akima)

df <- merged.df %>% filter(.,frameID==2000)
fld <- with(df, interp(x = lon, y = lat, z = precip))
filled.contour(x = fld$x,
               y = fld$y,
               z = fld$z,
               color.palette =
                 colorRampPalette(c("white", "blue")),
               xlab = "Longitude",
               ylab = "Latitude",
               main = "Germany Rainfall September 2010",
               key.title = title(main = "Rain (mm)", cex.main = 1))

df.sta <- merged.df %>% filter(.,frameID==600)
sta<-with(df.sta, interp(x = lon, y = lat, z = precip))

filled.contour(x = sta$x,
               y = sta$y,
               z = sta$z,
               color.palette =
                 colorRampPalette(c("white","blue")),zlim = c(0,100),
               xlab = "Longitude",
               ylab = "Latitude",
               main = "Germany Rainfall January 1894",
               key.title = title(main = "Rain (mm)", cex.main = 1))


# using a list and getting a single result
sub.df <- merged.df %>% filter(.,frameID>=600)
sub.list <- split(sub.df,sub.df$frameID)
results <- list()
#results<-with(sub.list$`600`, interp(x = lon, y = lat, z = precip))


i=1

for (i in i:length(sub.list)){
  results[[i]]<-with(sub.list[[i]], interp(x = lon, y = lat, z = precip,duplicate = "mean"))
  
}

# output of image files to dir
i=1
for (i in i:length(results)){
  png(filename=paste0(where,"/maps/",i,"out.png"))
      filled.contour(x = results[[i]]$x,
                     y = results[[i]]$y,
                     z = results[[i]]$z,
                     color.palette =
                       colorRampPalette(c("white", "blue")),zlim=c(0,150),
                     xlab = "Longitude",
                     ylab = "Latitude",
                     key.title = title(main = "Rain (mm)", cex.main = 1))
      dev.off()      
  
}


x <- list(a=11,b=12,c=13) # Changed to list to address concerns in commments
lapply(seq_along(x), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=x, n=names(x))

lapply(seq_along(sub.list),interp,x=lon,y=lat,z=precip)
lapply(seq_along(sub.list), function(x,y,z,i) 
  {interp(x=i$lon,y=i$lat,z=i$precip)})

lapply(seq_along(sub.list), function(i) paste(names(sub.list)[[i]])) #works
out<-lapply(seq_along(sub.list), function(i,x,y,z) with(sub.list[[i]],interp(x=lon,y=lat,z=precip))
out<-lapply(seq_along(sub.list), function(i,x,y,z) with(sub.list[[i]],interp),x=lon,y=lat,z=precip)
out<-lapply(seq_along(sub.list), function(x,y,z,i) {with(sub.list[[i]],interp)},x=lon,y=lat,z=precip)

###
# Create a Grid 1
x.range <- as.numeric(c(min(merged.df$lon), max(merged.df$lon)))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(merged.df$lat), max(merged.df$lat)))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.25), y = seq(from = y.range[1], 
                                                                                   to = y.range[2], by = 0.25))

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd, cex = 1.5, col = "grey")
points(dat, pch = 1, col = "red", cex = 1)

library(gstat)
idw <- idw(formula = precip ~ 1, locations = dat, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred))



