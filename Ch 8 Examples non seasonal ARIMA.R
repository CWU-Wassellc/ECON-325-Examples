library(fpp2)
library(astsa)

autoplot(austa)

# Q8
autoplot(forecast(auto.arima(austa), h = 10))
checkresiduals(auto.arima(austa))

# Q8b

Q8b1 <- sarima(austa, p = 0, d = 1, q = 1, no.constant = TRUE )
sarima.for(austa, p = 0, d = 1, q = 1, no.constant = TRUE, n.ahead = 8 ) 

Q8b2 <- sarima(austa, p = 0, d = 1, q = 0, no.constant = TRUE )
sarima.for(austa, p = 0, d = 1, q = 0, no.constant = TRUE, n.ahead = 8 ) 


# Q10 a, d, e

autoplot(austourists)

tsdisplay(diff(austourists,4))

auto.arima(austourists, parallel = T, approximation = F, stepwise = F)
