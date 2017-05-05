#######################
# klimDWD: pre-processing.R

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
date.month.num <-substr(date.str,5,6) %>% as.double(.)
date.year.num <-substr(date.str,1,4) %>% as.double(.)

frame <- mutate(raw.df,frameID = date.num-date.num[1], 
                month = date.month.num, year = date.year.num) # get working frame


# convert factors to numeric and fix NaN
frame$station.id <- as.factor(frame$station.id)
frame$station.id <- as.numeric(frame$station.id)
frame$precip[frame$precip==-999] <- NA
frame$precip[frame$precip<=0] <- NA

str(frame)



# use lookup table to get smooth framID transition
holder <- data.frame(frameID=unique(frame$frameID))
counter <- (1:length(holder$frameID))
ticker <- data.frame(holder,counter)
                     
frame <- left_join(frame,ticker,by=c('frameID'='frameID')) 


# merging with station meta data

# change working dir here in dev mode 
raw.meta <-read.table("Demonstration/stat.csv",header=TRUE, sep =",") # 150 stations only
#raw.meta <-read.table("Demonstration/stations-single-all-meta.csv",header = TRUE, sep = ",") # all meta data
raw.meta$station.id <-raw.meta$stat_id %>% as.numeric(.)


library(data.table)
# merging data and meta files
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

rm(dat.meta,dat.var,frame,merged.df,raw.df,raw.meta,date.num,date.month.num,
   date.year.num,date.str,files,frame.ids,meta.ids,meta.ids.adj,holder,ticker,counter)




