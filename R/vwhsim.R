vwhsim <- function(x, p = 0.95, lambda = 0.94){
  n <- length(x)
  cvar = ewma(x, lambda = lambda)
  csig = sqrt(cvar)
  xz = x  / csig
  loss <- -(xz * csig[n])
  VaR = quantile(loss, p)
  ES = mean(loss[loss >= VaR])
  result <- list(VaR = VaR, ES = ES)
}
