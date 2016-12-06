library(tidyr)

mini <- merged.df

mini$date.str <- as.character(mini$date.start)
mini$month<-substr(mini$date.str, 5, 6) %>% as.double(.)


library(plyr)

seasonal.comps <- ddply(mini, .(locale, month), summarise,
               MeanBase = mean(precip, na.rm=TRUE))

#########
# test-dat for calculating anomalies
month <- rep(1:12,4)
stat <- c(rep("A",24),rep("B",24))
value <- runif(48, 0, 1)   

dp <- as.data.frame(month)
dp$stat <- stat
dp$value <- value

dp$grp.means <-with(dp, ave(value,list(month,stat), FUN=mean))

dp$yr.anomaly <- with( dp, ave(value, list(month,stat), FUN=function(x) x- mean(x)))
