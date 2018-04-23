
################################################
### Similar to question 1, Chapter 5 in FPP2 ###
###

library(readxl)
cherries <- read_excel("E:/ECON 325/ECON 325 -- Spring 2018/ECON-325-Examples/cherries.xlsx")
View(cherries)

library(fpp2)

cherries <- cherries[,2:3]
cherries <- ts ( cherries, freq = 1 )
autoplot(cherries)

cherries %>% as.data.frame() %>% ggplot(aes(y=Cherries_lb, x=Temp)) + geom_point() + 
  xlab("Temperature, in degrees Farenheit") + ylab("Cherry prices, $/lb") + geom_smooth(method="lm", se=FALSE)

cherries.tslm <- tslm ( Cherries_lb ~ Temp, data = cherries )
summary ( cherries.tslm )

# In this example, temperature is an indicator of season.
# Warmer temperatures might indicate optimal harvest times, increased supply,
# and lower prices. Conversely, lower temperatures might indicate winter months
# when supply of cherries is low and prices are adjusted accordingly.

<<<<<<< HEAD
residuals(cherries.tslm) %>% autoplot() + geom_abline(slope = 0, intercept = 0, color = "red" )
=======
cherries.fit <- tslm ( Cherries_lb ~ Temp, data = cherries )
residuals(cherries.fit) %>% autoplot() + geom_abline(slope = 0, intercept = 0, color = "red" )
>>>>>>> 7375807b1c34f135df14eec31a1e752051c98261

# The residual plot looks to be quite adequate, although there is an observation in the
# high 50 degree range that could be inspected as an outlier.

cherries.df <- as.data.frame(cherries)
cherries.df [ , "Residuals"] <- as.numeric ( residuals ( cherries.tslm ) )

cherries.resid.plot <- ggplot ( cherries.df, aes ( x = Cherries_lb, y = Residuals ) ) + geom_point()
cherries.resid.plot

cherries.tslm %>% checkresiduals()

<<<<<<< HEAD
# Now the forecast. You need to plug in the specific value for Temp that you want to forecast a cherry price for.
# This is NOT like the rwf(), etc., forecasts where you're extrapolating out through time.

=======
>>>>>>> 7375807b1c34f135df14eec31a1e752051c98261
cherries.fcast <- forecast ( cherries.tslm, newdata = data.frame ( Temp = 60 ) )

cherries.fcast.df <- data.frame ( Temp = 60, Cherries_lb = cherries.fcast$mean[1] )

cherries.df %>% ggplot(aes(y=Cherries_lb, x=Temp)) + geom_point(alpha = 0.5) + 
  xlab("Temperature, in degrees Farenheit") + ylab("Cherries Price $/lb") +
  geom_point(data = cherries.fcast.df, aes(x=60, y = cherries.fcast$mean[1]), color = "blue", size = 2.5) +
  geom_ribbon(aes(x=60, ymin = cherries.fcast$lower[1], ymax = cherries.fcast$upper[1]), color = "lightblue", 
                size = 1.25, alpha = 0.1) + 
  geom_abline(slope = 0, intercept = cherries.fcast$mean, linetype = 3, color = "red") +
<<<<<<< HEAD
  geom_abline(slope = cherries.tslm$coefficients[2], intercept = cherries.tslm$coefficients[1], linetype = 2, col = "green" )
=======
  geom_abline(slope = cherries.fit$coefficients[2], intercept = cherries.fit$coefficients[1], linetype = 2, col = "green" )
>>>>>>> 7375807b1c34f135df14eec31a1e752051c98261
  
# This gives a graphical representation of the regression line and a chosen temperature
# to forecast (in this case 60). R will highlight a point on the line and shade its confidence
# interval range. We can see that at a temp of 60, prices are estimated to be around $5.2/lb.



################################################
### Similar to question 2, Chapter 5 in FPP2 ###
###

# World Infant Mortality Rate /1000 births by Year

library(readxl)
InfantMor <- read_excel("E:/ECON 325/ECON 325 -- Spring 2018/ECON-325-Examples/InfantMor.xlsx")
View(InfantMor)


# Before we go much further, if the data isn't defined as time series, (you can look at the data
# type using the command "str(data)") we need to define it as such.

IMRts<-ts(InfantMor$IMR, start=1950,end=1970,frequency=1)

autoplot(IMRts) + geom_smooth(method = "lm", se = FALSE) + ylab ( "Infant Mortality Rate per 1000" )


# Use the tslm function to run a time series linear model and plot it along with a confidence interval.

IMR.tsfit <- tslm ( IMRts ~ trend )

<<<<<<< HEAD
IMR.tsf <- forecast ( IMR.tsfit, h = 10,level = c(80,95) )
=======
IMR.tsf <- forecast ( tsfit, h = 10,level = c(80,95) )
>>>>>>> 7375807b1c34f135df14eec31a1e752051c98261

autoplot(IMR.tsf) + ylab ("IMR per 1000")
     
summary(IMR.tsfit)
     
### Check residuals

IMRts.df <- as.data.frame(IMRts)
IMRts.df [ , "Residuals"] <- as.numeric ( residuals ( IMR.tsfit ) )

IMRts.resid.plot <- ggplot ( IMRts.df, aes ( x = IMRts, y = Residuals ) ) + geom_point() + 
  geom_abline(slope=0, intercept = 0, color = "red", linetype = 2)

IMRts.resid.plot

<<<<<<< HEAD

### To plot residuals vs. year

IMRts.resid.plot2 <- ggplot ( IMRts.df, aes ( x = InfantMor$year, y = Residuals ) ) + geom_point() + 
  geom_abline(slope=0, intercept = 0, color = "red", linetype = 2)

IMRts.resid.plot2

### OR

IMRts.resid.plot3 <- ggplot ( IMRts.df, aes ( x = seq(1950, 1970, by = 1), y = Residuals ) ) + geom_point() + 
  geom_abline(slope=0, intercept = 0, color = "red", linetype = 2)

IMRts.resid.plot3

#########################

IMR.tsf %>% residuals() %>% autoplot()

IMR.tsfit %>% checkresiduals()

# Although not extremely exaggerated, the residuals plot hints that some type on non-linear
# relationship might exist. Furthermore the ACF plot shows some autocorrelation in lag 1 and 2. 

     
# Calculate the infant mortality for 1980 and give confidence intervals
# Using the summary table above:
# Y= 146.1762-(3.1286*31)
# Y= 49.1896
# Notice how we plug in the period we are trying to estimate (1980-1949=30), not the year.
     
# Confidence Intervals
     
forecast ( IMR.tsfit, newdata = data.frame ( year = c(1980) ) )

# Assumptions: That is trend will continue in a linear fashion into the future
     

######################## Code suggestions for question 5

# You need to make a dummy variable that equals "1" in every March EXCEPT FOR the first one.
# One way that's easy to visualize, but not necessarily the most efficient, is to make a variable that is a 
# sequence of 2 zeroes, a 1, then 9 more series... and replicate that several time.  Then replace the very
# first zero with a one.  

dummy_surf <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0), length(fancy)/12)
dummy_surf[3] <- 0

# When you go to forecast your data, you need to create future values for the dummy variable.
# That is, you need a few more of this sequence.

dummy_surf_fcast <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0), 3)
future_data <- data.frame(dummy_surf = dummy_surf_fcast)
fcast <- forecast(fit, future_data, h=36)

# The above will create 3 more years of dummy variables, then you can plus those into your forecast.

# Once you get forecasts, you'll have to transform back from logs to levels.  You can do exp(foo) to change "foo"
# to levels from logs.  Alternatively, note that the Box Cox transformation with lambda = 0 is the natural log.

back.transform <- InvBoxCox(fcast$mean,lambda=0)
back.transform_upper <- InvBoxCox(fcast$upper[,2],lambda=0)
back.transform_lower <- InvBoxCox(fcast$lower[,2],lambda=0)



######

# Fourier term example

library(alr4)

windspeed <- ts(wm2$RSpd, start = c(2002,1,1), frequency = 1460)
autoplot(windspeed)

model1 <- tslm ( windspeed ~ trend + fourier(windspeed, K=12))
autoplot(windspeed) + autolayer(fitted(model1), series = "12 Fourier Harmonic Terms")

model2 <- tslm ( windspeed ~ trend + fourier(windspeed, K=24))
autoplot(windspeed) + autolayer(fitted(model2), series = "24 Fourier Harmonic Terms")

model3 <- tslm ( windspeed ~ trend + fourier(windspeed, K=48))
autoplot(windspeed) + autolayer(fitted(model3), series = "48 Fourier Harmonic Terms")

model4 <- tslm ( windspeed ~ trend + fourier(windspeed, K=64))
autoplot(windspeed) + autolayer(fitted(model4), series = "96 Fourier Harmonic Terms")

model5 <- tslm ( windspeed ~ trend + fourier(windspeed, K=96))
autoplot(windspeed) + autolayer(fitted(model5), series = "96 Fourier Harmonic Terms")

CV(model1)[3]
CV(model2)[3]
CV(model3)[3]
CV(model4)[3]
CV(model5)[3]

AIC.values <- as.numeric(c(CV(model1)[3], CV(model2)[3], CV(model3)[3], CV(model4)[3], CV(model5)[3]))
optimal.K <- NULL
optimal.K$AIC.values <- AIC.values
optimal.K$Fourier <- c (12, 24, 48, 64, 96)
plot(AIC.values ~ Fourier, data = optimal.K, pch = 16)

# Best one of this group is K = 48

checkresiduals(model3)
windspeed.train <- window(windspeed, end = c(2002.95))
windspeed.test <- window(windspeed, start = c(2002.951))
wind.fcast <- forecast(tslm (windspeed.train ~ trend + fourier(windspeed.train, K = 48)), 
                       newdata=data.frame(fourier(windspeed.train, K = 48, h = 100), h = 100))
autoplot(windspeed) + autolayer(fitted(wind.fcast))
