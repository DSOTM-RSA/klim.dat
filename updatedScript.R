
# load libraries
library(dplyr)
library(tidyr)

# Read in and Process Files
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

frame <- mutate(pro.df,frameID = total_months)



out=strtrim(pro.df$date.start,6)
pro.df$date <-out
pro.df$nums <- as.numeric(pro.df$date)
pro.df$dateDF <-pro.df$date %>% as.Date(., format='%Y%m')

frame <- mutate(pro.df, frameID = nums-min(nums)+1)
frame <- mutate(merged.df,frameID = date_start-min(date_start)+1)

%>% as.Date(., format='%Y%m')
pro.df$date.end <-as.character(pro.df$date.end) %>% as.Date(., format='%Y%m%d')


pro.df$station.id <- as.factor(pro.df$station.id)
pro.df$precip[pro.df$precip==-999] <- NA


pro.df.ts <- as.ts(pro.df)

dt <- pro.df


# merging id data
new<-read.table("stat.csv",header=TRUE, sep =",")

new$station.id <-new$stat_id


library(data.table)

dat = as.data.table(dt)
dat2 = as.data.table(new)

dat2$station.id <-dat2$stat_id
labels.join <- select(dat2,lat:station.id)
merged.df<-merge(dat,labels.join, by="station.id")

frame <- mutate(merged.df,frameID = date_start-min(date_start)+1)

