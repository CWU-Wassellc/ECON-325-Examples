
### Q Similar to Ch 9 Q4

library(readxl)

# https://www.eia.gov/dnav/ng/hist/n9010us2m.htm


natgas_mcf <- read_excel("US Nat Gas Withdrawals.xlsx")
View(natgas_mcf)

natgas_bcf <- (natgas_mcf[,2] / 1000) # Convert to Billions Cubic Feet
natgas <- ts ( natgas_bcf, start = c(1980,1), frequency = 12 ) 

library(fpp2)
library(gridExtra)
library(astsa)

autoplot(natgas)
ggsubseriesplot(natgas)

### Lambda for Box Cox transformation, as needed

natgas_lambda <- BoxCox.lambda ( natgas )

###

library(strucchange)

t <- time(natgas)
plot(natgas ~ t, data = natgas, pch = 20)
breakpoints(natgas ~ t, data = natgas)  ## Identify the 4-segment breakpoints

### Breakpoints in 1986(10) 1993(9) 2005(8)

h <- 0

t <- time(natgas)
t.break1 <- (((1986*12)+10)/12)
t.break2 <- (((1993*12)+9)/12)
t.break3 <- (((2005*12)+8)/12)

tb1 <- ts(pmax(0, t - t.break1), start = 1980, frequency = 12)
tb2 <- ts(pmax(0, t - t.break2), start = 1980, frequency = 12)
tb3 <- ts(pmax(0, t - t.break3), start = 1980, frequency = 12)

fit.pw <- tslm(natgas ~ t + tb1 + tb2 + tb3)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] # + seq(h)
tb2.new <- tb2[length(tb2)] # + seq(h)
tb3.new <- tb3[length(tb3)] # + seq(h)
newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new, tb3=tb3.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(natgas ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3) + I(tb3^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)


autoplot(natgas) +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Natural Gas Production, Billions of Cubic Feet") +
  ggtitle("U.S. Natural Gas Gross Withdrawals") +
  guides(colour = guide_legend(title = " "))


### Try splinef function

natgas %>%
  splinef(lambda=natgas_lambda) %>%
  autoplot()

natgas %>%
  splinef(lambda=natgas_lambda) %>%
  checkresiduals()

### Now try adding Fourier series to the piecewise linear model

fit.pw.1 <- tslm(natgas ~ t + tb1 + tb2 + tb3 + fourier(natgas, K = 6))
fit.pw.2 <- tslm(natgas ~ t + tb1 + tb2 + tb3 + fourier(natgas, K = 5))
fit.pw.3 <- tslm(natgas ~ t + tb1 + tb2 + tb3 + fourier(natgas, K = 4))

CV(fit.pw.1) ## This one is best.
CV(fit.pw.2)
CV(fit.pw.3)

checkresiduals(fit.pw.1)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new, tb3=tb3.new, fourier(natgas, K = 6, h = 12)) %>%
  as.data.frame()
natgas.pw.fcast <- forecast(tslm (natgas ~ t + tb1 + tb2 + tb3 + fourier(natgas, K = 6)), 
                       newdata=newdata, h = 12)
autoplot(natgas.pw.fcast) + autolayer(fitted(natgas.pw.fcast))

### Fit model with ARIMA errors.

fit.arima <- auto.arima ( natgas, xreg = fourier(natgas,K=6))
fit.arima
checkresiduals(fit.arima)
autoplot(forecast(fit.arima, xreg=data.frame(fourier(natgas, K = 6, h = 12)), h = 12), PI=FALSE) + 
  autolayer(fitted(fit.arima),series="ARIMA Errors") + 
  autolayer(natgas.pw.fcast,series="Piecewise with Fourier", PI=F)


### Try TBATS

natgas.tbats <- tbats(natgas)
autoplot(forecast(natgas.tbats, h = 12))
accuracy(natgas.tbats)
checkresiduals(natgas.tbats)

#################################


### Ch 9 Q3


autoplot(motel[,"Roomnights"])

avg_price <- motel[,"Takings"]/motel[,"Roomnights"]
autoplot(avg_price)
ggsubseriesplot(avg_price)

## Interpolate monthly values from quarterly


library(tempdisagg)
cpimel_monthly <- predict(td(cpimel ~ 1, conversion = "average", to = "monthly", method = "denton-cholette"))
tail(cpimel_monthly)

###
autoplot(motel[,"Takings"])
autoplot(motel[,"Roomnights"])

### Arima

ggtsdisplay(avg_price)
ggtsdisplay(diff(avg_price,12))
ggtsdisplay(diff(diff(avg_price,12)))


model1 <- auto.arima(avg_price, lambda = 0, xreg = cpimel_monthly, stepwise = F, approximation = F, parallel = T)
model1
cpimel_monthly_fcast <- forecast ( splinef(cpimel_monthly, h = 12))
model1.fcast <- forecast ( model1, xreg = cpimel_monthly_fcast$mean )
autoplot(model1.fcast)

checkresiduals(model1)
