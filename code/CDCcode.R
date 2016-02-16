# CDC code steps

# count files in a dir
ls -1 | wc -l

# trim html code for filenames
awk -F\" '{print $2}' source.txt > names.txt


# create filenames using prefix.txt
awk '{print "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/more_precip/historical/" $0;}' names.txt > filelist.txt


# download all files
wget -i filelist.txt 

# extract all zips
for i in *.zip; do unzip "$i"; done

# copying files of same name
cp [S]* outputfolder

##############################################
# Processing Code

library(dplyr)
library(tidyr)
library(lattice)

# read in files

files <- dir(pattern = '*.txt', full.names = TRUE)
tables.of.data <- lapply(files, read.table,sep=";",header=TRUE,fill=TRUE)


# correcting for false factors in data

fix.factors <-function(df) {
  df[complete.cases(df),]
}

tables.of.data.trimmed<-lapply(tables.of.data,fix.factors)

# subset columns of interest
tables.subset<-lapply(tables.of.data.trimmed,"[",1:6)

# remove un-needed datafiles
rm(tables.of.data.trimmed); rm(tables.of.data)


# test plot
#plot(tables.subset[[1]]$NIEDERSCHLAGSHOEHE,type="l")

# OR convert to BIG df
data.df<-dplyr::bind_rows(tables.subset)

# deal with FALSE factors
data.df.FF<-data.df[complete.cases(data.df),] # remove NA's (false factors and missing data)

# Various Cleaning

data.df.FF$date.start <-as.character(data.df.FF$MESS_DATUM_BEGINN) %>% as.Date(., format='%Y%m%d')
data.df.FF$date.end <-as.character(data.df.FF$MESS_DATUM_ENDE) %>% as.Date(., format='%Y%m%d')

data.df.FF$station.id <- as.factor(data.df.FF$STATIONS_ID)
data.df.FF$precip <- data.df.FF$NIEDERSCHLAGSHOEHE

dt$precip[dt$precip==-999] <- NA

dt <- data.df.FF %>% select(date.start:precip)

dt.ts <- as.ts(dt)

# remove un-needed datafiles
rm(data.df); rm(data.df.FF);


########################
library(zoo)
library(lattice)

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


                                               R                             
xyplot(depth ~ amount | prop, data = dt.long, layout=c(3,3,1),as.table=TRUE,
       strip=TRUE, pch = 16, type=c("p","smooth"),col.line="red",horizontal=TRUE, aspect="free",
       ylab = "Depth (cm)", xlab = "Counts",
       panel = function (x,y, ...){
             panel.xyplot(x,y, ...)
       })


z <-as.ts(dt)
zz<-na.omit(z)


tablesmin[[1]]$stat<-as.character(tablesmin[[1]]$STATIONS_ID)
out$stat <-as.factor(out$STATIONS_ID)
