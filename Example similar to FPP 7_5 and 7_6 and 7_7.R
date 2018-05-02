library(fpp2)
library(gridExtra)

######## Questions 7.5 and 7.6

str(prison)
colnames(prison)

prison.sentenced <- ts ( prison[,c(28,32)], start = c(2005,1), frequency = 4 )
autoplot(prison.sentenced)

prison.VICM <- prison.sentenced[,1]
prison.WAM <- prison.sentenced[,2]

prison.VICM.ses <- ses(prison.VICM, h = 4, initial = c("optimal"), PI = FALSE)
prison.WAM.ses <- ses(prison.WAM, h = 4, initial = c("optimal"))

autoplot(prison.VICM.ses) + autolayer(prison.WAM.ses$mean) + autolayer(prison.WAM, color="red")

VICM.ses.RMSE <- sqrt(mean((prison.VICM.ses$residuals)^2))
VICM.ses.RMSE

WAM.ses.RMSE <- sqrt(mean((prison.WAM.ses$residuals)^2))
WAM.ses.RMSE

prison.VICM.holt <- holt(prison.VICM, h = 4, initial = c("optimal"), PI = FALSE)
prison.WAM.holt <- holt(prison.WAM, h = 4, initial = c("optimal"), PI = FALSE)

autoplot(prison.VICM.holt) + autolayer(prison.WAM.holt$mean) + autolayer(prison.WAM, color="red")

VICM.holt.RMSE <- sqrt(mean((prison.VICM.holt$residuals)^2))
VICM.holt.RMSE

WAM.holt.RMSE <- sqrt(mean((prison.WAM.holt$residuals)^2))
WAM.holt.RMSE


### Prediction Interval using RMSE, assuming normal errors

VICM.ses.PI.high <- prison.VICM.ses$mean[1] + 1.96 * VICM.ses.RMSE
VICM.ses.PI.low <- prison.VICM.ses$mean[1] - 1.96 * VICM.ses.RMSE
VICM.holt.PI.high <- prison.VICM.holt$mean[1] + 1.96 * VICM.holt.RMSE
VICM.holt.PI.low <- prison.VICM.holt$mean[1] - 1.96 * VICM.holt.RMSE

VICM.holt.PI.high; VICM.holt.PI.low
VICM.ses.PI.high; VICM.ses.PI.low

WAM.ses.PI.high <- prison.WAM.ses$mean[1] + 1.96 * WAM.ses.RMSE
WAM.ses.PI.low <- prison.WAM.ses$mean[1] - 1.96 * WAM.ses.RMSE
WAM.holt.PI.high <- prison.WAM.holt$mean[1] + 1.96 * WAM.holt.RMSE
WAM.holt.PI.low <- prison.WAM.holt$mean[1] - 1.96 * WAM.holt.RMSE

WAM.holt.PI.high; WAM.holt.PI.low
WAM.ses.PI.high; WAM.ses.PI.low


### Predict Intervals from R

prison.VICM %>% holt(.,initial=c("optimal"))%>%predict(h=1)
prison.VICM %>% ses(.,initial=c("optimal"))%>%predict(h=1)

prison.WAM %>% holt(.,initial=c("optimal"))%>%predict(h=1)
prison.WAM %>% ses(.,initial=c("optimal"))%>%predict(h=1)


#######################################
###### Question 7

chicken.holt.additive <- holt ( chicken, h = 50, exponential = FALSE)
## chicken.holt.exponential <- holt ( chicken, h = 50, exponential = TRUE)
chicken.holt.additive.damped <- holt ( chicken, h = 50, exponential = FALSE, damped = TRUE)
chicken.holt.exponential.damped <- holt ( chicken, h = 50, exponential = TRUE, damped = TRUE)

autoplot(chicken.holt.additive)
## autoplot(chicken.holt.exponential)
autoplot(chicken.holt.additive.damped)
autoplot(chicken.holt.exponential.damped)



chicken.lambda <- BoxCox.lambda(chicken)

chicken %>% holt(., h=50, exponential = FALSE, damped = TRUE) %>% autoplot()
chicken %>% holt(., h=50, exponential = TRUE, damped = TRUE) %>% autoplot()
chicken %>% holt(., h=50, lambda = chicken.lambda, exponential = FALSE, damped = TRUE) %>% autoplot()

chicken %>% holt(., h=50, exponential = FALSE, damped = TRUE) %>% accuracy()
chicken %>% holt(., h=50, exponential = TRUE, damped = TRUE) %>% accuracy()
chicken %>% holt(., h=50, lambda = chicken.lambda, exponential = FALSE, damped = TRUE) %>% accuracy()

