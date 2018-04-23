
library(fpp2)
library(Quandl)

## Civilian LF Participation Rate

civ.lf <- Quandl("FRED/LNU01300000")
civ.lf <- ts(civ.lf[,2], start = 1948, frequency = 12)
civ.lf <- window(civ.lf, start = 1980)

## Ages 20-24

civ.lf.young <- Quandl("FRED/LNU01300036")
civ.lf.young <- ts(civ.lf.young[,2], start = 1948, frequency = 12)
civ.lf.young <- window(civ.lf.young, start = 1980)

## Ages 25-54

civ.lf.mid <- Quandl("FRED/LNU01300060")
civ.lf.mid <- ts(civ.lf.mid[,2], start = 1948, frequency = 12)
civ.lf.mid <- window(civ.lf.mid, start = 1980)

## Ages 55+

civ.lf.old <- Quandl("FRED/LNU01324230")
civ.lf.old <- ts(civ.lf.old[,2], start = 1948, frequency = 12)
civ.lf.old <- window(civ.lf.old, start = 1980)

### Plots


p1 <- civ.lf %>% autoplot() + ggtitle("Civ Labor Force")
p2 <- civ.lf.young %>% autoplot() + ggtitle("Civ Labor Force, Young")
p3 <- civ.lf.mid %>% autoplot() + ggtitle("Civ Labor Force, Mid-age")
p4 <- civ.lf.old %>% autoplot() + ggtitle("Civ Labor Force, Old")

library(gridExtra)

gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

### Regressions

civ.lf.fit <- tslm(civ.lf ~ trend)
civ.lf.young.fit <- tslm(civ.lf.young ~ trend)
civ.lf.mid.fit <- tslm(civ.lf.mid ~ trend)
civ.lf.old.fit <- tslm(civ.lf.old ~ trend)

p12 <- p1 + geom_smooth(method = "lm", se = FALSE, color = "purple", lwd = 0.5)
p22 <- p2 + geom_smooth(method = "lm", se = FALSE, color = "purple", lwd = 0.5)
p32 <- p3 + geom_smooth(method = "lm", se = FALSE, color = "purple", lwd = 0.5)
p42 <- p4 + geom_smooth(method = "lm", se = FALSE, color = "purple", lwd = 0.5)

gridExtra::grid.arrange(p12, p22, p32, p42, nrow=2)


### Residuals

par(mfrow = c(2,2))

plot(residuals(civ.lf.fit))
  abline(0,0)
  abline(2,0, col = "red", lty = 2)
  abline(-2,0, col = "red", lty = 2)

plot(residuals(civ.lf.young.fit))
  abline(0,0)
  abline(2,0, col = "red", lty = 2)
  abline(-2,0, col = "red", lty = 2)
  
plot(residuals(civ.lf.mid.fit))
  abline(0,0)
  abline(2,0, col = "red", lty = 2)
  abline(-2,0, col = "red", lty = 2)
  
plot(residuals(civ.lf.old.fit))
  abline(0,0)
  abline(2,0, col = "red", lty = 2)
  abline(-2,0, col = "red", lty = 2)
  
par(mfrow = c(1,1))


### Seasonal naive

p13 <- civ.lf %>% snaive() %>% autoplot() + autolayer(fitted(civ.lf.fit), color = "green")
p13

