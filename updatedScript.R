
# load libraries
library(dplyr)
library(tidyr)

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
stat.meta<-read.table("stat.csv",header=TRUE, sep =",")
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
             color="grey70", size=3.5, shape=21)+
  scale_fill_gradientn(colours=rev(heat.colors(5)))


out <- split(merged.df,as.factor(merged.df$frameID))


