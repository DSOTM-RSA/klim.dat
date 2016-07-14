##############################################
# Processing Code



library(dplyr)
library(tidyr)
library(lattice)
library(magrittr)
li

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


dt <- data.df.FF %>% select(date.start:precip)



#########################
# Quick Look at Data


#--->>> merging id data
new<-read.table("stat.csv",header=TRUE, sep =",")
dat = as.data.table(dt.arranged)
dat2 = as.data.table(new)
dat2$station.id <-dat2$stat_id

newdt<-merge(dat, dat2, by="station.id")

write.dir <- "downloads/product/figs/"


ii <- unique(newdt$locale)
i<-unique(newdt$station.id)

n<-1

for (n in n:150){                                               
  png(filename = paste(write.dir,"Fig-",ii[n],".png",sep=""))
  print(xyplot(precip ~ date.start |locale , data = newdt, layout=c(1,1,ii[n]),as.table=FALSE,
               strip=TRUE, pch = 16, type=c("l"),col.line="blue",horizontal=FALSE, aspect="free",
               ylab = "Precipation (mm)", xlab = "Years",
               panel = function (x,y, ...){
                 panel.xyplot(x,y, ...)
               }))
  dev.off()
}




########################
library(zoo)

# Example of using a custom axis
# Months are labelled with smaller ticks for weeks and even smaller
# ticks for days.
Days <- seq(from = as.Date("2006-1-1"), to = as.Date("2006-8-8"), by = "day")
z <- zoo(seq(length(Days))^2, Days)
Months <- Days[format(Days, "%d") == "01"]
Weeks <- Days[format(Days, "%w") == "0"]
xyplot(z, scales = list(x = list(at = Months)))
trellis.focus("panel", 1, 1, clip.off = TRUE)
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
           tck = .7, at = as.numeric(Weeks))
panel.axis("bottom", check.overlap = TRUE, outside = TRUE, labels = FALSE, 
           tck = .4, at = as.numeric(Days))
trellis.unfocus()


z <-as.ts(dt)
zz<-na.omit(z)


tablesmin[[1]]$stat<-as.character(tablesmin[[1]]$STATIONS_ID)
out$stat <-as.factor(out$STATIONS_ID)


new<-read.table("stat.csv",header=TRUE, sep =",")
dataTable <- read.table("names.txt",header=T)

read.csv("stat.csv")




