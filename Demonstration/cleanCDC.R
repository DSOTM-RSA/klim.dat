
# load libraries
library(dplyr)
library(tidyr)

# Read in and Process Files
files <- dir(pattern = '*.txt', full.names = TRUE)

pro.df <- files %>%  lapply(., read.table,sep=";",header=TRUE,fill=TRUE) %>% 
  lapply(., function(x) x[complete.cases(x),]) %>%  lapply(.,"[",1:6) %>% dplyr::bind_rows(.) %>% 
  .[complete.cases(.),] %>% setNames(c('station.id','date.start','date.end','quality','type','precip'))


# Conversion to TS
pro.df$date.start <-as.character(pro.df$date.start) %>% as.Date(., format='%Y%m%d')
pro.df$date.end <-as.character(pro.df$date.end) %>% as.Date(., format='%Y%m%d')
pro.df$station.id <- as.factor(pro.df$station.id)

pro.df$precip[pro.df$precip==-999] <- NA
pro.df.ts <- as.ts(pro.df)

dt <- pro.df %>% select(date.start:precip)


# merging id data
new<-read.table("stat.csv",header=TRUE, sep =",")

new$station.id <-new$stat_id

out<-pro.df %>% left_join(new)

