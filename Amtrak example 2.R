
### Load the libraries that we'll need

library(readxl)
library(fpp2)
library(zoo)

### Load Excel data

Amtrak <- read_excel("Amtrak Ridership.xlsx")
View(Amtrak)


### Convert to monthly time series

ridership.ts <- ts(Amtrak$Passengers / 1000, start = c(1991,1), end = c(2004, 12), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l")


### Decomposition

ridership.decompose <- decompose(ridership.ts, type = "multiplicative")
plot(ridership.decompose)

### Estimate and plot seasonal naive model.

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
train.ts <- window(ridership.ts, start = c(1991,1), end = c(1991, fixed.nTrain))
valid.ts <- window(ridership.ts, start = c(1991, fixed.nTrain + 1), end = c(1991, fixed.nTrain + fixed.nValid))
ridership.snaive <- snaive(train.ts, h = fixed.nValid)
plot(ridership.snaive)


### Trailing Moving Average

ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")
plot(ridership.ts, ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991, 2006), main = "")
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(ma.trailing, lwd = 2, lty = 2)

### STL

ridership.stl <- stl(train.ts, t.window = 6, s.window = 6, robust = TRUE)
plot(ridership.stl)

ridership.adj <- seasadj(ridership.stl)
plot(naive(ridership.adj))

ridership.stl.fcast <- forecast(ridership.stl, method = "naive", h=36)
plot(ridership.stl.fcast)

###

accuracy(ridership.snaive, valid.ts)
accuracy(ridership.stl.fcast, valid.ts)
