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
#' @param nboot size of bootstrap sample; default is 10000
#' @param nahead n-day ahead forecasts of VaR and ES; default is 1
#' @param arma fitting an ARMA-model to the return series before volatility
#' filtering; default is FALSE
#' @param ... additional arguments of the \emph{arima} function
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
#' fhs(x = returns, p = 0.975, lambda = 0.94, nahead = 5)


fhs <- function(x, p = 0.975, lambda = 0.94, nboot = 10000, nahead = 1,
                arma = FALSE, ...) {
  if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'x'.")
  }
  if (length(p) != 1 || is.na(p) || !is.numeric(p) || (p <= 0)) {
    stop("The argument 'p' must be a single non-NA double value with ",
         "0 < p < l.")
  }
  if (length(lambda) != 1 || is.na(lambda) || !is.numeric(lambda) ||
      lambda < 0 || lambda >= 1) {
    stop("The argument 'lambda' must be a single non-NA double value with ",
         "0 < lambda < 1.")
  }
  if (length(nboot) != 1 || is.na(nboot) || !is.numeric(nboot) ||
      nboot <= 0) {
    stop("The argument 'nboot' must be a single non-NA integer value with ",
         "nboot > 0.")
  }
  if (length(nahead) != 1 || is.na(nahead) || !is.numeric(nahead) ||
      nahead <= 0) {
    stop("The argument 'nahead' must be a single non-NA integer value with ",
         "nahead > 0.")
  }
  if (length(arma) != 1 || !arma %in% c(TRUE, FALSE)) {
    stop("The argument 'arma' must be TRUE or FALSE.")
  }
  n <- length(x)
  dots = list(...)
  if (arma == TRUE) {
    arima <- stats::arima(x, ...)
    ret <- x
    x <- stats::residuals(arima)
  }
  cvar <- ewma(x, lambda = lambda)
  one.ahead.cvar <-  lambda * cvar[n] + (1 - lambda) * x[n]^2
  csig <- sqrt(cvar)
  one.ahead.csig <- sqrt(one.ahead.cvar)
  xz <- x/csig


  boot.loss <- matrix(0, nboot, nahead)
  VaR <- rep(0, nahead)
  ES <- rep(0, nahead)

  if (arma == FALSE) {
    for (i in 1:nahead) {
      boot.xz <- sample(xz, size = nboot, replace = TRUE)
      boot.loss[ , i] <- -(boot.xz * one.ahead.csig)
      one.ahead.cvar <- lambda * one.ahead.cvar + (1 - lambda) *
        boot.loss[, i]^2
      one.ahead.csig <- sqrt(one.ahead.cvar)
      VaR[i] <- stats::quantile(boot.loss[, i], p)
      ES[i] <- mean(boot.loss[, i][boot.loss[, i] > VaR[i]])
    }
  }

  if (arma == TRUE) {
    ar <- dots$order[1]
    ma <- dots$order[3]
    phi <- arima$model$phi
    theta <- arima$model$theta
    if ("include.mean" %in% names(dots) && dots$include.mean == FALSE) {
      mu <- 0
    }
    else {
      mu <- arima$coef[["intercept"]]
    }
    for (i in 1:nahead) {
      boot.xz <- sample(xz, size = nboot, replace = TRUE)
      boot.loss[, i] <- -(boot.xz * one.ahead.csig)

      boot.loss <- -(mu + sum(phi * ret[n:(n - ar + 1)]) +
                         sum(theta * x[n:(n - ma + 1)])) + boot.loss

      one.ahead.cvar <- lambda * one.ahead.cvar + (1 - lambda) *
        boot.loss[, i]^2
      one.ahead.csig <- sqrt(one.ahead.cvar)
      VaR[i] <- stats::quantile(boot.loss[, i], p)
      ES[i] <- mean(boot.loss[, i][boot.loss[, i] > VaR[i]])
    }
  }

  results <- cbind(VaR = VaR, ES = ES)
  colnames(results) <- c(paste0(100 * p, "% VaR"), "ES")
  if (arma == TRUE) {
    results <- list(VaR_ES = results, model_arma = arima)
  }
  else {
    results <- list(VaR_ES = results)
  }
  results
}
