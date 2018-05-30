library(fpp2)
library(hts)
library(readxl)
library(tidyverse)

US_Petroleum_Supply <- read_excel("~/CWU/ECON 325/ECON 325 Spring 2018/US Petroleum Supply.xlsx")
View(US_Petroleum_Supply)

uscrude <- US_Petroleum_Supply[,3:9]

uscrude <- ts ( uscrude, start = c ( 1981, 1 ), frequency = 12 )

autoplot(uscrude)

uscrude.hts <- hts ( uscrude, characters = c ( 6, 3 ) )

uscrude.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("Thousands of Barrels") + ggtitle("U.S. Petroleum Supply")


cols <- sample(scales::hue_pal(h=c(15,375), c=100,l=65,h.start=0,direction = 1)(NCOL(uscrude)))

as_tibble(uscrude) %>%
  gather(Product) %>%
  mutate(Date = rep(time(uscrude), NCOL(uscrude)),
         Category = str_sub(Product,1,6)) %>%
  ggplot(aes(x=Date, y=value, group=Product, color=Product)) +
  geom_line() +
  facet_grid(Category~., scales="free_y") +
  xlab("Year") + ylab("millions") +
  ggtitle("Petroleum Product Supply by Category") +
  scale_color_manual(values = cols)


uscrude.train <- window ( uscrude.hts, end = c ( 2016,2 ) )
uscrude.test <- window ( uscrude.hts, start = c ( 2016,3 ) )

hts.fcast1 <- forecast(uscrude.train, h = 24, method="bu", fmethod="arima")
accuracy.gts(hts.fcast1, uscrude.test)

hts.fcast2 <- forecast(uscrude.train, h = 24, method="bu", fmethod="ets")
accuracy.gts(hts.fcast2, uscrude.test)

hts.fcast3 <- forecast(uscrude.train, h = 24, method="tdfp")
accuracy.gts(hts.fcast3, uscrude.test)

hts.fcast4 <- forecast(uscrude.train, h = 24, method="mo", level = 1)
accuracy.gts(hts.fcast4, uscrude.test)

### Optimal reconciliation

hts.fcast5 <- forecast(uscrude.train, h = 24, method="comb", weights = "wls" )
accuracy.gts(hts.fcast5, uscrude.test)

####

fcsts <- aggts(hts.fcast5, levels=0:2)
groups <- aggts(uscrude.train, levels=0:2)

uscrudefc <- ts(rbind(groups, fcsts), start=start(groups), frequency=12)

p1 <- autoplot(uscrudefc[,"Total"]) +
  ggtitle("U.S. Petroleum Supply") +
  xlab("Year") + ylab("Thousands of barrels") +
  geom_vline(xintercept=2016.25)

cols <- sample(scales::hue_pal(h=c(15,375), c=100,l=65,h.start=0,direction = 1)(NCOL(groups)))

p2 <- as_tibble(uscrudefc[,2:4]) %>%
  gather(Series) %>%
  mutate(Date = rep(time(uscrudefc), 3),
         Group = str_extract(Series, "([A-Za-z ]*)")) %>%
  ggplot(aes(x=Date, y=value, group=Series, color=Series)) +
  geom_line() +
  xlab("Year") + ylab("Thousands of Barrels") +
  scale_color_manual(values = cols) +
  facet_grid(Group ~ ., scales = "free_y" ) +
  scale_x_continuous(breaks=seq(1981,2019,by=2)) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  geom_vline(xintercept=2016.25)

gridExtra::grid.arrange(p1, p2, ncol = 1)

### Plot of Finished Petroleum Products

autoplot(uscrudefc[,5:9]) + xlab("Year") + ylab("Thousands of Barrels")

### Bagged Forecasts

uscrude2 <- ts(US_Petroleum_Supply$Crude, start = c(1981,1), frequency=12)

uscrude2.train <- window ( uscrude2, end = c(2016,2) )
uscrude2.test <- window ( uscrude2, start = c(2016,3) )

uscrude2.fcast.bagged <- uscrude2.train %>% baggedETS() %>% forecast(h=24)
autoplot(uscrude2) + autolayer(uscrude2.fcast.bagged, series = "Bagged ETS", PI = F)
accuracy(uscrude2.fcast.bagged, uscrude2.test)


### Neural Net

uscrude.nnet <- nnetar(uscrude2.train, bootstrap = T)
uscrude.fcast.nnet <- forecast(uscrude.nnet, h = 24)
accuracy(uscrude.fcast.nnet, uscrude2.test)

autoplot(uscrude2) + autolayer(uscrude2.fcast.bagged, series = "Bagged ETS", PI = F) + 
  autolayer(uscrude.fcast.nnet$mean, series = "Neural Net" ) +
  ggtitle("U.S. Petroleum Supply") +
  xlab("Year") + ylab("Thousands of barrels")

autoplot(tail(uscrude2, 72)) + autolayer(uscrude2.fcast.bagged, series = "Bagged ETS", PI = F) + 
  autolayer(uscrude.fcast.nnet$mean, series = "Neural Net" ) +
  ggtitle("U.S. Petroleum Supply") +
  xlab("Year") + ylab("Thousands of barrels")
