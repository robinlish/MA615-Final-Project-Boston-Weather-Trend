# install.packages("tidyverse")
# install.packages(c("RColorBrewer", "dplyr", "dygraphs", "ggfortify"))
install.packages("ggfortify")
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(dygraphs)
library(ggfortify)

# Data Preparation:

#boston <- read.csv("https://www.ncei.noaa.gov/orders/cdo/1337764.csv")
#save(boston, file="boston.Rdata")

load("boston.Rdata")
datetxt <- boston$DATE
datetxt <- as.Date(datetxt)
boston <- add_column(boston,
                     YEAR = as.numeric(format(datetxt, format = "%Y")),
                     Month = as.numeric(format(datetxt, format = "%m")),
                     DAY = as.numeric(format(datetxt, format = "%d")), .after = 6) %>% 
  mutate(MONTH = month.abb[Month])
boston$month_ordered <- factor(boston$MONTH, levels = month.abb)

boston <- boston[,c("NAME","DATE","YEAR","Month","DAY","PRCP","SNOW","SNWD","TAVG","TMAX","TMIN","MONTH","month_ordered","AWND")]

#Data Analysis:

#1. temperature Gradients:

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

##precipitation
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
autoplot(ts.decompose)+ggtitle("Time Sereis of Average Tempetation")

###observed:
dygraph(ts.decompose$x) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)

###trend:
dygraph(ts.decompose$trend) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)

###seasonal:
dygraph(ts.decompose$seasonal) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)

###random:
dygraph(ts.decompose$random) %>% dyRangeSelector() %>% dyOptions(drawGrid = F)

##precipitation:
ts_prcp <- ts(na.omit(monthly$PRCP),start=c(1936,1), end=c(2018,4), frequency=12)
autoplot(decompose(ts_prcp))+ggtitle("Time Sereis of Precipitation")

#3. forecast:
hw <- HoltWinters(myts)
predict_temp <- predict(hw, n.ahead = 24, 
        prediction.interval = TRUE,
        level = as.numeric(0.95))

dygraph(predict_temp, main = "Predicted Average Temperature/Month") %>%
  dySeries(c("lwr", "fit", "upr"), label = "Temperature") %>%
  dyOptions(drawGrid = F) %>% 
  dyRangeSelector()

#4. correlation:
pal <- colorRampPalette(c("blue", "yellow", "red", "green"))
mycols=pal(12)

##prcp vs. temp
ggplot(data=monthly,aes(x=TAVG,y=PRCP,color=factor(month_ordered))) + 
  geom_point(alpha=.5) + scale_color_manual(values=mycols) + 
  xlab('Temperature') + ylab('Precipitation')

ggplot(data = monthly,mapping = aes(x=TAVG,y=PRCP)) + geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "gaussian"))+ 
  xlab('Temperature') + ylab('Precipitation')

cor.test(monthly$TAVG, monthly$PRCP, use = "complete.obs")
cor(monthly$TAVG, monthly$PRCP)

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
cor(na.omit(monthly[, c(3:6)]))

scatterplotMatrix(monthly[, c(3:6)], smooth=2,spread=FALSE,main='Scatter Plot Matrix')

#model 1
lm1 <- lm(data = monthly, TAVG ~ PRCP + AWND + SNOW)
lm1
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)

AIC(regression)
BIC(regression)

#model 2
lm2 <- lm(data = monthly, TAVG ~ AWND + SNOW)
lm2
summary(lm2)
plot(lm2)

AIC(lm2)
BIC(lm2)

#model 3
lm3 <- lm(data = monthly, TAVG ~ AWND+PRCP)
lm3
summary(lm3)
plot(lm3)

AIC(lm3)
BIC(lm3)

#model 4
lm4 <- lm(data = monthly, TAVG ~ AWND)
lm4
summary(lm4)
plot(lm4)

AIC(lm4)
BIC(lm4)


