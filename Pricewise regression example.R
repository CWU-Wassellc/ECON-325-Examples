library(alr4)
library(fpp)
library(strucchange)

plot(C ~ Temp, data = segreg, pch = 20)
breakpoints(C ~ Temp, data = segreg)  ## Identify the 2-segment breakpoint
segreg[26,]  ## Breakpoint at observation 26

segreg$outlier <- rep(0, 39) 
segreg$outlier[15] <- 1

# Fit linear.

elecp <- pmax(segreg$Temp - 61.3, 0)
fit_piecewise <- lm(C ~ Temp + elecp + outlier, data = segreg)

Temp_knot <- predict(fit_piecewise, newdata = data.frame(Temp = 61.3, elecp = 0, outlier = 0))
x <- min(segreg$Temp) : max(segreg$Temp)
z <- pmax(x - 61.3, 0)

fcast_piecewise <- forecast(fit_piecewise, newdata = data.frame(Temp = x, elecp = z, outlier = 0))
lines(x, fcast_piecewise$mean, col="blue")

# Fit quadratic.

fit_quadratic <- lm(C ~ Temp + I(Temp^2) + outlier, data = segreg)
fcast_quadratic <- forecast(fit_quadratic, newdata = data.frame(Temp = x, elecp = z, outlier = 0))
lines(x, fcast_quadratic$mean, col = "red")

# Fit semi-log.

log_Temp <- log(segreg$Temp)
log_x <- log(x)

fit_log <- lm(C ~ log_Temp + outlier, data = segreg)
fcast_log <- forecast(fit_log, newdata = data.frame(log_Temp = log_x, elecp = z, outlier = 0))
lines(x, fcast_log$mean, col = "green")

# Fit cubic spline.

fit_cubic <- lm(C ~ Temp + I(Temp^2) + I(Temp^3) + I(elecp^3) + outlier, data = segreg)
fcast_cubic <- forecast(fit_cubic, newdata = data.frame(Temp = x, elecp = z, outlier = 0))
lines(x, fcast_cubic$mean, col = "purple")

legend("bottomright", legend = c("piecewise", "quadratic", "semi-log", "cubic"), lty = 1, col = c(4, 2, 3, "purple"))

# Determine the best fit using the CV() function.

CV(fit_piecewise)
CV(fit_quadratic)
CV(fit_log)
CV(fit_cubic)

# Create residual plots

par(mfrow = c(2,2))
plot(fcast_piecewise$resid ~ segreg$Temp, pch = 19, col = "blue"); abline(0, 0, lty = 2)
plot(fcast_quadratic$resid ~ segreg$Temp, pch = 19, col = "red"); abline(0, 0, lty = 2)
plot(fcast_log$resid ~ segreg$Temp, pch = 19, col = "green"); abline(0, 0, lty = 2)
plot(fcast_cubic$resid ~ segreg$Temp, pch = 19, col = "purple"); abline(0, 0, lty = 2)
par(mfrow = c(1,1))

# Calculate forecasts of electricity usage for a range of Temperatures, along with prediction intervals.

xx <- seq(10, 80, 10); zz <- pmax(xx - Temp_knot, 0)
predicted <- predict(fit_quadratic, newdata = data.frame(Temp = xx, elecp = zz, outlier =0), interval = "prediction")
matplot(xx, predicted, type="l", lty = 1, lwd = 1.5, col = c("navyblue","gold","gold"))
matpoints(xx, predicted, type="p", pch = 21, bg = "yellow")
matpoints(segreg$Temp, segreg$C, type = "p", pch = 19)

# Calculate correlation between Temp and Temp squared, then discuss.

cor(segreg$Temp, I(segreg$Temp^2))
 