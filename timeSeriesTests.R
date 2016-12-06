library(tidyr)

mini <- merged.df

mini$date.str <- as.character(mini$date.start)
mini$month<-substr(mini$date.str, 5, 6) %>% as.double(.)


library(plyr)

seasonal.comps <- ddply(mini, .(locale, month), summarise,
               MeanBase = mean(precip, na.rm=TRUE))
