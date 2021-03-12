#' Filtered historical simulation
#'
#' Calculates univariate Value at Risk and Expected Shortfall (also called
#' Conditional Value at Risk) by means of filtered historical simulation.
#' Volatility is estimated with an exponentially weighted moving
#' average.
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.975
#' @param lambda decay factor for the calculation of weights; default is 0.94
#' @param arma fitting an ARMA-model to the return series before volatility
#' filtering
#' @param ... additional arguments of the arima function
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{Calculated Value at Risk}
#' \item{ES}{Calculated Expected Shortfall (Conditional Value at Risk)}
#' \item{armacoefs}{Estimated model parameters}
#' }
#' @examples
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' fhs(x = returns, p = 0.975, lambda = 0.94, arma = TRUE, order = c(1, 0, 1))


fhs_old <- function(x, p = 0.975, lambda = 0.94, arma = FALSE, ...) {
  n <- length(x)
  dots = list(...)
  if (arma == TRUE) {
    arima <- arima(x, ...)#do.call(what = stats::arima, args = dots)
    ret <- x
    x <- residuals(arima)
  }
  cvar <- ewma(x, lambda = lambda)
  one.ahead.cvar <-  lambda * cvar[n] + (1 - lambda) * x[n]^2
  csig <- sqrt(cvar)
  one.ahead.csig <- sqrt(one.ahead.cvar)
  xz <- x/csig
  boot.xz <- sample(xz, size = 100000, replace = TRUE)
  boot.loss <- -(boot.xz * one.ahead.csig)
  if (arma == TRUE) {
    ar <- dots$order[1]
    ma <- dots$order[3]
    phi <- arima$model$phi
    theta <- arima$model$theta
    if ("include.mean" %in% names(dots) && dots$include.mean == FALSE) {
      boot.loss <- sum(phi * ret[(n - 1):(n - ar)]) +
                   sum(theta * x[(n - 1):(n - ma)]) + boot.loss
    }
    else {
      boot.loss <- arima$coef[["intercept"]] + sum(phi * ret[(n - 1):(n - ar)])
                   + sum(theta * x[(n - 1):(n - ma)]) + boot.loss
    }

  }
  VaR <- stats::quantile(boot.loss, p)
  ES <- mean(boot.loss[boot.loss > VaR])
  results <- cbind(VaR = VaR, ES = ES)
  colnames(results) <- c("VaR", "ES")
  rownames(results) <- paste0(100 * p, "%")
  if (arma == TRUE) {
    results <- list(VaR_ES = results, model_arma = arima)
  }
  else {
    results <- list(VaR_ES = results)
  }
  results
}
