#' Volatility weighted historical simulation
#'
#' Calculates Value at Risk and Expected Shortfall by means of volatility weighted historical simulation. Volatilities
#' are estimated with an exponentially weighed moving average.
#'
#' @param x a numeric vector of asset returns.
#' @param p confidence level for VaR calculation; default is 0.95%.
#' @param lambda decay parameter for calculation of weights; default is 0.94.
#'
#'@export

vwhsim <- function(x, p = 0.95, lambda = 0.94){
  n <- length(x)
  cvar <- ewma(x, lambda = lambda)
  csig <- sqrt(cvar)
  xz <- x  / csig
  loss <- -(xz * csig[n])
  VaR <- quantile(loss, p)
  ES <- mean(loss[loss >= VaR])
  result <- list(VaR = VaR, ES = ES)
}
