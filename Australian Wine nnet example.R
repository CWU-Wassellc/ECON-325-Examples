library(fpp2)
library(astsa)
library(readxl)

### Data from DataMarket TSDL

AusWine <- read_excel("C:/Users/wassellc/Desktop/AusWine.xlsx")
View(AusWine)

wine <- ts ( AusWine[,2], start = c(1980,1), frequency = 12 )
wine.train <- head ( wine, (length(wine)-24))
wine.test <- tail (wine, 24)

autoplot(wine.train) + autolayer(wine.test)
ggsubseriesplot(wine)

## ETS
wine.lambda <- BoxCox.lambda ( wine.train )

ETS.wine <- ets ( wine.train, lambda = wine.lambda )
summary(ETS.wine)
ETS.wine.fcast <- forecast ( ETS.wine, h = 24 )
autoplot(ETS.wine.fcast)
checkresiduals(ETS.wine)
accuracy( ETS.wine.fcast, wine.test )

### ARIMA

ARIMA.wine <- auto.arima ( wine.train, lambda = wine.lambda,
             stepwise = FALSE, approximation = FALSE,
              parallel = TRUE )
summary(ARIMA.wine)
checkresiduals ( ARIMA.wine )
ARIMA.wine.fcast <- forecast ( ARIMA.wine, h = 24 )
autoplot(ARIMA.wine.fcast)
accuracy(ARIMA.wine.fcast, wine.test)

### Neural Net

nnet.wine <- nnetar ( wine.train, lambda = wine.lambda, p = 11 )
summary(nnet.wine)
checkresiduals ( nnet.wine )
nnet.wine.fcast <- forecast ( nnet.wine, h = 24 )
autoplot(nnet.wine.fcast)
accuracy ( nnet.wine.fcast, wine.test )

### TBATS

TBATS.wine <- tbats ( wine.train,  lambda = wine.lambda )
checkresiduals ( TBATS.wine )
TBATS.wine.fcast <- forecast ( TBATS.wine, h = 24 )
autoplot(TBATS.wine.fcast)
accuracy( TBATS.wine.fcast, wine.test )

### STL

STLF.wine.fcast <- stlf ( wine.train, s.window = 11, 
                          lambda = wine.lambda, h = 24)
autoplot(STLF.wine.fcast)
accuracy(STLF.wine.fcast, wine.test)
