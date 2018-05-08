install.packages("dygraphs")
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(dygraphs)
#ncdc <- read.csv("https://www.ncei.noaa.gov/orders/cdo/1336927.csv")
#ncdc$TAVG <- ceiling((ncdc$TMAX+ncdc$TMIN)/2)
#save(ncdc, file="ncdc.Rdata")

#boston <- read.csv("https://www.ncei.noaa.gov/orders/cdo/1337764.csv")
#boston$TAVG <- ceiling((boston$TMAX+boston$TMIN)/2)
#save(boston, file="boston.Rdata")

load("boston.Rdata")
#1. temp map
datetxt <- boston$DATE
datetxt <- as.Date(datetxt)
boston <- add_column(boston,
                 YEAR = as.numeric(format(datetxt, format = "%Y")),
                 Month = as.numeric(format(datetxt, format = "%m")),
                 DAY = as.numeric(format(datetxt, format = "%d")), .after = 6) %>% 
  mutate(MONTH = month.abb[Month])
boston$month_ordered <- factor(boston$MONTH, levels = month.abb)
boston <- boston[,c("NAME","DATE","YEAR","Month","DAY","PRCP","SNOW","SNWD","TAVG","TMAX","TMIN","MONTH","month_ordered","AWND")]

##average temperature
ggplot(data=boston, aes(x=YEAR,y=month_ordered)) + 
  geom_tile(aes(fill = TAVG),colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral')), na.value = "grey98",
                       limits = c(-20, 100)) + 
  theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), plot.title = element_text(hjust = .5)) + 
  ggtitle("Average Temperature/˚F of Boston from 1936-1-1 to 2018-5-1 (Based on Daily Average)")+
  scale_x_continuous(breaks=seq(1936,2018,5))

##max temperature
ggplot(data=boston, aes(x=YEAR,y=month_ordered)) + 
  geom_tile(aes(fill = TMAX),colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral')), na.value = "grey98",
                       limits = c(-20, 100)) + 
  theme(legend.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),plot.title = element_text(hjust = .5)) + 
  ggtitle("Maximum Temperature/˚F of Boston from 1936-1-1 to 2018-5-1 (Based on Daily Max)")+
  scale_x_continuous(breaks=seq(1936,2018,5))

##min temperature
ggplot(data=boston, aes(x=YEAR,y=month_ordered)) + 
  geom_tile(aes(fill = TMIN),colour = "white") + 
  scale_fill_gradientn(colours=rev(brewer.pal(10,'Spectral')), na.value = "grey98",
                       limits = c(-20, 100)) + 
  theme(legend.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),plot.title = element_text(hjust = .5)) + 
  ggtitle("Minimum Temperature/˚F of Boston from 1936-1-1 to 2018-5-1 (Based on Daily Min)")+
  scale_x_continuous(breaks=seq(1936,2018,5))

##prcp
ggplot(data=boston, aes(x=YEAR,y=month_ordered)) + 
  geom_tile(aes(fill = PRCP),colour = "white") + 
  scale_fill_gradientn(colours=brewer.pal(9,'BrBG'), na.value = "grey50",
                       limits = c(0, 7)) + 
  theme(legend.title=element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank(),plot.title = element_text(hjust = .5)) + 
  ggtitle("Pricipitation/inch of Boston from 1936-1-1 to 2018-5-1 (Based on Daily PRCP)")+
  scale_x_continuous(breaks=seq(1936,2018,5))


#2. time series:
monthly <- boston %>% select(month_ordered,YEAR,TAVG, PRCP, AWND, SNOW) %>% 
  group_by(month_ordered,YEAR) %>% 
  summarise(TAVG = mean(TAVG),PRCP = mean(PRCP), AWND = mean(AWND), SNOW = mean(SNOW)) %>% 
  arrange(YEAR)

##average temp:
myts <- ts(monthly$TAVG,start=c(1936,1), end=c(2018,4), frequency=12)
ts.decompose <- decompose(myts)

plot(decompose(myts))

##prcp:
ts_prcp <- ts(na.omit(monthly$PRCP),start=c(1936,1), end=c(2018,4), frequency=12)
autoplot(decompose(ts_prcp))+ggtitle("precipitation")
View(ts_prcp)

#3. forecast
d.arima <- auto.arima(myts)
d.forecast <- forecast(d.arima, level = c(95), h = 12)
autoplot(d.forecast)

#4. correlation:
pal <- colorRampPalette(c("blue", "yellow", "red", "green"))
mycols=pal(12)

##prcp vs. temp
ggplot(data=monthly,aes(x=TAVG,y=PRCP,color=factor(month_ordered))) + 
  geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
  xlab('Temperature') + ylab('Precipitation')

ggplot(data = monthly,mapping = aes(x=TAVG,y=PRCP)) + geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "gaussian"))

cor.test(monthly$TAVG, monthly$PRCP, use = "complete.obs")

##wind speed vs. temp
ggplot(data=monthly,aes(x=AWND,y=TAVG,color=factor(month_ordered))) + 
  geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
  xlab('Wind speed') + ylab('Temprature')

ggplot(data = monthly,mapping = aes(x = AWND, y = TAVG)) + geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "gaussian")) + 
  xlab('Wind speed') + ylab('Temprature')

cor.test(monthly$TAVG, monthly$AWND, use = "complete.obs")

##snow vs. temp
ggplot(data=monthly,aes(x=TAVG,y=SNOW,color=factor(month_ordered))) + 
  geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
  xlab('Temprature') + ylab('Snow')

ggplot(data = monthly,mapping = aes(x = TAVG, y = SNOW)) + geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "gaussian"))+ 
  xlab('Temprature') + ylab('Snow')


cor.test(monthly$TAVG, monthly$SNOW, use = "complete.obs")

#5. regression:
regression <- lm(data = boston, TAVG ~ AWND)
regression
summary(regression)

AIC(regression)
BIC(regression)


regression2 <- lm(data = boston, TAVG ~ AWND + PRCP + FMTM)
regression2
summary(regression2)

AIC(regression2)
BIC(regression2)


regression3 <- lm(data = boston, TAVG ~ AWND + PRCP + SNOW)
regression3
summary(regression3)

AIC(regression3)
BIC(regression3)

boston$logtemp <- log(boston$TAVG)

regression4 <- lm(data = boston, logtemp ~ AWND + PRCP + FMTM + SNOW)
regression4
summary(regression4)

AIC(regression3)
BIC(regression3)






