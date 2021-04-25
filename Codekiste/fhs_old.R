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

  VaR <- rep(0, nahead)
  ES <- rep(0, nahead)

  if (arma == FALSE) {
    for (i in 1:nahead) {
      boot.xz <- sample(xz, size = nboot, replace = TRUE)
      boot.loss <- -(boot.xz * one.ahead.csig)
      one.ahead.cvar <- lambda * one.ahead.cvar + (1 - lambda) *
        boot.loss^2
      one.ahead.csig <- sqrt(one.ahead.cvar)
      VaR[i] <- stats::quantile(boot.loss, p)
      ES[i] <- mean(boot.loss[boot.loss > VaR[i]])
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
      #boot.loss <- boot.loss.sc <- (1:nboot) * 0
      # if (i == 1) {
      #   boot.loss <- ret
      # }
      #nret <- length(ret)
      # for (j in 1:nboot) {
      #
      # boot.xz <- sample(xz, size = 1, replace = TRUE)
      # boot.loss.sc[j] <- -(boot.xz * one.ahead.csig)
      #
      #
      # boot.loss[j] <- -(mu + sum(phi * boot.loss[j]) +
      #                     sum(theta * zstar)) + boot.loss.sc[j]
      #
      # zstar <- boot.loss.sc[j]
      # one.ahead.cvar <- lambda * one.ahead.cvar + (1 - lambda) * boot.loss[j]^2
      # one.ahead.csig <- sqrt(one.ahead.cvar)
      # }
      nret <- length(ret)
      boot.xz <- sample(xz, size = nboot, replace = TRUE)
      boot.loss.sc <- -(boot.xz * one.ahead.csig)
      boot.loss <- -(mu + sum(phi * ret[nret:(nret - ar + 1)]) +
                         sum(theta * x[n:(nret - ma + 1)])) + boot.loss.sc

      if (nahead >= 2) {
        for (j in 2:nboot) {
          retboot <- c(ret, boot.loss[j])
          xboot <- c(x, boot.loss.sc[j])
        }
      }
      ret <- retboot
      x <- xboot

      ret <- c(ret, sample(boot.loss, 1))
      x <- c(x, sample(boot.loss.sc, 1))
      one.ahead.cvar <- lambda * one.ahead.cvar + (1 - lambda) * boot.loss^2
      one.ahead.csig <- sqrt(one.ahead.cvar)
      VaR[i] <- stats::quantile(boot.loss, p)
      ES[i] <- mean(boot.loss[boot.loss > VaR[i]])
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
