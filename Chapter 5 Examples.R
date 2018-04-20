
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

residuals(cherries.tslm) %>% autoplot() + geom_abline(slope = 0, intercept = 0, color = "red" )

# The residual plot looks to be quite adequate, although there is an observation in the
# high 50 degree range that could be inspected as an outlier.

cherries.df <- as.data.frame(cherries)
cherries.df [ , "Residuals"] <- as.numeric ( residuals ( cherries.tslm ) )

cherries.resid.plot <- ggplot ( cherries.df, aes ( x = Cherries_lb, y = Residuals ) ) + geom_point()
cherries.resid.plot

cherries.tslm %>% checkresiduals()

# Now the forecast. You need to plug in the specific value for Temp that you want to forecast a cherry price for.
# This is NOT like the rwf(), etc., forecasts where you're extrapolating out through time.

cherries.fcast <- forecast ( cherries.tslm, newdata = data.frame ( Temp = 60 ) )

cherries.fcast.df <- data.frame ( Temp = 60, Cherries_lb = cherries.fcast$mean[1] )

cherries.df %>% ggplot(aes(y=Cherries_lb, x=Temp)) + geom_point(alpha = 0.5) + 
  xlab("Temperature, in degrees Farenheit") + ylab("Cherries Price $/lb") +
  geom_point(data = cherries.fcast.df, aes(x=60, y = cherries.fcast$mean[1]), color = "blue", size = 2.5) +
  geom_ribbon(aes(x=60, ymin = cherries.fcast$lower[1], ymax = cherries.fcast$upper[1]), color = "lightblue", 
                size = 1.25, alpha = 0.1) + 
  geom_abline(slope = 0, intercept = cherries.fcast$mean, linetype = 3, color = "red") +
  geom_abline(slope = cherries.tslm$coefficients[2], intercept = cherries.tslm$coefficients[1], linetype = 2, col = "green" )
  
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

IMR.tsf <- forecast ( IMR.tsfit, h = 10,level = c(80,95) )

autoplot(IMR.tsf) + ylab ("IMR per 1000")
     
summary(IMR.tsfit)
     
### Check residuals

IMRts.df <- as.data.frame(IMRts)
IMRts.df [ , "Residuals"] <- as.numeric ( residuals ( IMR.tsfit ) )

IMRts.resid.plot <- ggplot ( IMRts.df, aes ( x = IMRts, y = Residuals ) ) + geom_point() + 
  geom_abline(slope=0, intercept = 0, color = "red", linetype = 2)

IMRts.resid.plot


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
     
