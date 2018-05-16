
library(fpp2)

fets <- function (y, h, lambda) {
  forecast ( ets ( y, lambda = lambda, additive.only = FALSE ), h = h )
}

ets.tsCV <- tsCV ( visitors, fets, h = 1 )

fets2 <- function ( y, h, lambda ) {
  forecast ( ets ( y, lambda = lambda, additive.only = TRUE ), h = h )
}

visitors.lambda <- BoxCox.lambda(visitors)

ets.additive.BC.tsCV <- tsCV ( visitors, fets2, lambda = visitors.lambda, h = 1 )

snaive.tsCV <- tsCV ( visitors, snaive, h = 1 )

fets3 <- function ( y, h, lambda, s.window ) {
  stlf ( y, s.window = s.window, h = h, lambda = visitors.lambda, etsmodel = "ZZZ" )
}

stlf.tsCV <- tsCV ( visitors, fets3, h = 1, lambda = visitors.lambda, s.window = 11 )


calc.RMSE.from.tsCV <- function (x) {
 sqrt ( mean ( x^2, na.rm = TRUE ) )
}

ets.RMSE <- calc.RMSE.from.tsCV ( ets.tsCV )
ets.additive.BC.RMSE <- calc.RMSE.from.tsCV ( ets.additive.BC.tsCV )
snaive.RMSE <- calc.RMSE.from.tsCV ( snaive.tsCV )
stlf.RMSE <- calc.RMSE.from.tsCV ( stlf.tsCV )

ets.RMSE; ets.additive.BC.RMSE; snaive.RMSE; stlf.RMSE
