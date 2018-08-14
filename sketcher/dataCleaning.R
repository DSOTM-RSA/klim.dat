

# Load Libraries
library(tidyverse)
library(forcats)
library(ggjoy)
library(viridis)
library(extrafont)
library(gganimate)


# Bulk Data Ingestion
where<-getwd()
files <- dir(path=paste0(where,"/data"), pattern = '*.txt', full.names = TRUE)

dataRaw <- files %>% 
  lapply(., read_delim, ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  dplyr::bind_rows(.)

# extract individual months and years
dateStr <- as.character(dataRaw$MESS_DATUM_BEGINN)
dateNum <- substr(dateStr, 1, 6) %>% as.double()
dateMonth <-substr(dateNum, 5, 6) %>% as.double()
dateYear <- substr(dateNum, 1, 4) %>% as.double()

# make temporary table
frameTemp <- mutate(dataRaw, month = dateMonth, 
                    year = dateYear, frameID = dateNum-dateNum[1])

# convert factors to numeric and fix NaN
frameTemp$monthFct <-as.factor(frameTemp$month)
frameTemp$MO_RR[frameTemp$MO_RR==-999] <-NA

# RM helper objects 
rm(dateMonth,dateNum,dateStr,dateYear,files)


# Use Lookup Table to Get Smooth frameID Transition
frames <- data.frame(frameID=unique(frameTemp$frameID))
counts <- (1:length(frames$frameID))
lut_frames_counts <- data.frame(frames,counts)

frameTemp <- left_join(frameTemp,lut_frames_counts,by=c('frameID'='frameID')) 

# RM helper objects
rm(frames,lut_frames_counts,counts)


# Read in Metadata
Stationen_RF <- read_delim(paste0(where,"/RR_Monatswerte_Beschreibung_Stationen_RF.tsv"),
                           "\t", escape_double = FALSE, trim_ws = TRUE)

# Clean Names and Join
Stationen_RF$STATIONS_ID <-Stationen_RF$Stations_id 
frameComplete<-frameTemp %>% left_join(Stationen_RF) %>% select(-Stations_id)

# RM temporary data.frames
rm(dataRaw,frameTemp,Stationen_RF)

# select, mutate, and plot in one go
table_condensed <- frameComplete %>% filter(!is.na(MO_RR)) %>% 
  mutate(labels=fct_recode(monthFct,"Januar" = "1", "Februar" = "2", "März" = "3", "April" = "4",
                           "Mai"= "5", "Juni" = "6", "Juli" = "7", "August" = "8",
                           "September" = "9", "Oktober" = "10", "November" = "11", "Dezember" = "12"))


# create labels, and select necessary vars
table_test <- frameComplete %>% select(counts,month,year,
                                       monthFct,
                                       MO_RR,STATIONS_ID,Stationshoehe,Stationsname,Bundesland) %>%  
  mutate(labels=fct_recode(monthFct,"Januar" = "1", "Februar" = "2", "März" = "3", "April" = "4",
                           "Mai"= "5", "Juni" = "6", "Juli" = "7", "August" = "8",
                           "September" = "9", "Oktober" = "10", "November" = "11", "Dezember" = "12"))

# calculate Z-scores per station and month
table_Z <- table_test %>% group_by(STATIONS_ID,month) %>% 
  mutate(t1 = MO_RR - min(MO_RR), t2 = max(MO_RR)-min(MO_RR),
         Z = t1/t2) %>% filter(year >= 2015, STATIONS_ID == 836)

# counter from start-window
table_Z$dYr <- table_Z$year - table_Z$year[1]
table_Z$dMo <- table_Z$month - table_Z$month[1]
table_Z$dT <- table_Z$dYr*12 + table_Z$dMo + 1

table_Z$statFct <-as.factor(table_Z$STATIONS_ID)
table_Z$ease <- 'bounce-out'


dt <- tween_elements(table_Z, 'dT', 'StatFct', 'ease', nframes = 48)


# draft plot
# todo: build in offset for each station
ggplot(table_Z, aes(dT,Z)) +
  geom_point(size=3,show.legend = FALSE) + theme_bw() + 
  coord_flip() + scale_x_reverse() +
  transition_time(dT) + ease_aes('linear')



