#' Exponentially weighted moving average
#'
#' Estimates volatilities of a return series by means of an exponentially weighted moving average.
#'
#' @param x a numeric vector of asset returns.
#' @param lambda decay parameter for calculation of weights; default is 0.94.
#'
#'@export

ewma = function(x, lambda = 0.94){
  n = length(x)
  vol = rep(NA, n)
  vol[1] = var(x) #lambda * stats::var(x) + (1 - lambda) * stats::var(x)
  for(i in 2:n){
    vol[i] = lambda * vol[i - 1] + (1 - lambda) * x[i - 1]^2
  }
  drop(vol)
}
