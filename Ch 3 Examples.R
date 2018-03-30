
### Plot example similar to 1st plot in Ch 3.1

library(readxl)
library(fpp2)

SeattleTemps <- read_excel("E:/ECON 325/ECON 325 -- Spring 2018/ECON-325-Examples/SeattleTemps.xlsx", 
                           col_names = FALSE)

Seattle <- ts(SeattleTemps, start=c(2000,1), frequency=12)

autoplot ( Seattle ) + 
  ggtitle ( "Daily Maximum Temperature, Seattle WA") + 
  xlab( "Year" ) + ylab ( "Temperature (Deg F)" )

ggseasonplot( Seattle )
ggsubseriesplot( Seattle )

### Set training data to exclude last 3 years

Seattle.train <- window ( Seattle, end = c ( 2014, 12 ))

autoplot(Seattle.train) +
  autolayer(meanf(Seattle.train, h=39), series="Mean", PI=FALSE) +
  autolayer(naive(Seattle.train, h=39), series="Naïve", PI=FALSE) +
  autolayer(snaive(Seattle.train, h=39), series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Monthly Daily Maximum Temperature, Seattle WA") +
  xlab("Year") + ylab("Daily Maximum Temperature") +
  guides(colour=guide_legend(title="Forecast"))

### Problem similar to Ch 3.7 #10

# Use the French Industry index.
# a.  Produce a time plot of the series.

autoplot(french) + ggtitle("French Industry Index") + ylab("French Industry Index")

# b.  Produce forecasts using the drift method and plot them.

autoplot(french) + ggtitle("Forecast French Industry Index -- Random Walk with Drift") + ylab("French Industry Index") + 
  autolayer( rwf ( french, drift = TRUE, h = 6 ), series = "RW with Drift", PI = FALSE )

# c.  Show that the graphed forecasts are identical to extending the line drawn between the first and last observations.
# This data set has 28 observations. I know this because...

length ( french )

# We will calculate the slope between the 1st and 28th, then plot the connecting line.

slope <- ( french [ 28 ] - french [ 1 ] ) / ( 3.25 - 1 )

df <- data.frame ( y1 = french [ 1 ], y2 = french [ 28 ], x1 = 1, x2 = 3.25 )

autoplot(french) + ggtitle("Forecast French Industry Index -- Random Walk with Drift") + ylab("French Industry Index") + 
  autolayer( rwf ( french, drift = TRUE, h = 6 ), series = "RW with Drift", PI = FALSE ) +
  geom_segment( aes ( x = x1, y = y1, xend = x2, yend = y2 ), color = "dodgerblue", data = df )
                 
# d.  Try some of the other benchmark functions to forecast the same data set.  Which do you think is best?  Why?

french2 <- window(french, end=2.5)

autoplot(french2) + ggtitle("Forecast French Industry Index") + ylab("French Industry Index") + 
  autolayer( rwf ( french2, drift = TRUE, h = 11 ), series = "RW with Drift", PI = FALSE ) +
  autolayer( rwf ( french2, h = 11 ), series = "Naive", PI = FALSE ) +
  autolayer( meanf ( french2, h = 11 ), series = "Mean Forecast", PI = FALSE )



