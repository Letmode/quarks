#' Rolling one-step forecasts of Value at Risk and Expected Shortfall
#'
#' Computes rolling one-step forecasts of Value at Risk and Expected Shortfall
#' (also called Conditional Value at Risk) by means of plain historical
#' simulation age- and volatility-weighted historical simulation as well as
#' filtered historical simulation.
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.975
#' @param method method to be used for calculation; default is 'plain'
#' @param lambda decay factor for the calculation of weights; default is 0.98
#' for \emph{method = 'age'} and 0.94 for \emph{method = 'vwhs'} or
#' \emph{method = 'fhs'}
#' @param nout number of out-of-sample observations; default is NULL
#' @param nwin window size for rolling one-step forecasting; default is NULL
#' @param nboot size of bootstrap sample. Only for \emph{method = 'fhs'};
#' default is 10000
#' @param arma fitting an ARMA-model to the return series before volatility
#' filtering. Only for \emph{method = 'fhs'}; default is FALSE
#' @param ... additional arguments of the \emph{arima} function
#'
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{numerical vector containing out-of-sample forecasts of Value at
#' Risk}
#' \item{ES}{numerical vector containing out-of-sample forecasts of Expected
#' Shortfall (Conditional Value at Risk)}
#' \item{xout}{numerical vector containing out-of-sample returns}
#' }
#' @examples
#'
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' n <- length(returns)
#' nout <- 250 # number of obs. for out-of-sample forecasting
#' nwin <- 500 # window size for rolling forecasts
#'
#'
#' ### Example 1 - plain historical simulation
#' results1 <- rollcast(x = returns, p = 0.975, method = 'plain', nout = nout,
#'                      nwin = nwin)
#' matplot(1:nout, cbind(-results1$xout, results1$VaR, results1$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Plain HS - 97.5% VaR and ES for the DAX30 return series')
#'
#' ### Example 2 - age weighted historical simulation
#' results2 <- rollcast(x = returns, p = 0.975, method = 'age', nout = nout,
#'                      nwin = nwin)
#' matplot(1:nout, cbind(-results2$xout, results2$VaR, results2$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Age weighted HS - 97.5% VaR and ES for the DAX30 return series')
#'
#' ### Example 3 - volatility weighted historical simulation
#' results3 <- rollcast(x = returns, p = 0.975, method = 'vwhs', nout = nout,
#'                      nwin = nwin)
#' matplot(1:nout, cbind(-results3$xout, results3$VaR, results3$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Vol. weighted HS - 97.5% VaR and ES for the DAX30 return series')
#'
#' ### Example 4 - filtered historical simulation
#' results4 <- rollcast(x = returns, p = 0.975, method = 'fhs', nout = nout,
#'                      nwin = nwin)
#' matplot(1:nout, cbind(-results4$xout, results4$VaR, results4$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Filtered HS - 97.5% VaR and ES for the DAX30 return series')

rollcast <- function(x, p = 0.975, method = c("plain", "age", "vwhs", "fhs"),
                     lambda = c(0.94, 0.98), nout = NULL, nwin = NULL,
                     nboot = 10000, arma = FALSE, ...) {
  if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'x'.")
  }
  if (length(p) != 1 || is.na(p) || !is.numeric(p) || (p <= 0)) {
    stop("The argument 'p' must be a single non-NA double value with ",
         "0 < p < l.")
  }
  if (!(length(method) %in% c(1, 4)) || !all(!is.na(method)) ||
      !is.character(method) || !method %in% c("plain", "age", "vwhs", "fhs")) {
    stop("A single character value must be passed to 'method'.",
         "Valid choices are 'plain', 'age', 'vwhs' or 'fhs'.")
  }
  if (!(length(lambda) %in% c(1, 2)) || !all(!is.na(lambda)) ||
      !is.numeric(lambda) || all(lambda < 0) || all(lambda >= 1)) {
    stop("The argument 'lambda' must be a single non-NA double value with ",
         "0 < lambda < 1.")
  }
  if (length(nout) != 1 || is.na(nout) || !is.numeric(nout) || nout < 0 ||
      is.null(nout)) {
    stop("The argument 'nout' must be a single non-NA integer value.")
  }
  if (length(nwin) != 1 || is.na(nwin) || !is.numeric(nwin) || nwin <= 1 ||
      is.null(nwin)) {
    stop("The argument 'nwin' must be a single non-NA integer value
             with nwin > 1.")
  }
  if (nwin > (length(x) - nout) || nout > (length(x) - nwin) ||
      (nwin + nout) > length(x)) {
    stop("Window size and (or) out-of-sample size are too large.")
  }
  if (length(arma) != 1 || !arma %in% c(TRUE, FALSE)) {
    stop("The argument 'arma' must be TRUE or FALSE.")
  }

  if (all(method == c("plain", "age", "vwhs", "fhs")))
    method <- "plain"
  if (all(lambda == c(0.94, 0.98)) && method == "age")
    lambda <- 0.98
  if (all(lambda == c(0.94, 0.98)) && method == "vwhs" || method == "fhs")
    lambda <- 0.94


  n <- length(x)
  nin <- n - nout
  xin <- x[1:nin]
  if (nout == 0){
    xout <- NA
  }
  else{
    xout <- x[(nin + 1):n]
  }
  xstart <- xin[(nin - nwin + 1):nin]
  fcasts <- matrix(NA, max(nout, 1), 2, dimnames = list(c(), c("VaR", "ES")))
  if (method == "plain") {
    fcasts[1, ] <- hs(xstart, p = p, method = method)[[1]]
    if (nout > 1) {
      for (i in 2:nout) {
        if (i <= nwin) {
          fcasts[i, ] <- hs(c(xstart[i:nwin], xout[1:(i - 1)]),
                            p = p, method = method)[[1]]
        }
        else{
          fcasts[i, ] <- hs(xout[(i - nwin):(i - 1)], p = p,
                            method = method)[[1]]
        }
      }
    }
  }
  if (method == "age") {
    fcasts[1, ] <- hs(xstart, p = p, method = method, lambda = lambda)[[1]]
    if (nout > 1) {
      for (i in 2:nout) {
        if (i <= nwin) {
          fcasts[i, ] <- hs(c(xstart[i:nwin], xout[1:(i - 1)]),
                            p = p, method = method,
                            lambda = lambda)[[1]]
        }
        else{
          fcasts[i, ] <- hs(xout[(i - nwin):(i - 1)], p = p,
                            method = method, lambda = lambda)[[1]]
        }
      }
    }
  }
  if (method == "vwhs") {
    fcasts[1, ] <- vwhs(xstart, p = p, lambda = lambda)[[1]]
    if (nout > 1) {
      for (i in 2:nout) {
        if (i <= nwin) {
          fcasts[i, ] <- vwhs(c(xstart[i:nwin],
                                xout[1:(i - 1)]), p = p,
                              lambda = lambda)[[1]]
        }
        else{
          fcasts[i, ] <- vwhs(xout[(i - nwin):(i - 1)], p = p,
                              lambda = lambda)[[1]]
        }
      }
    }
  }
  if (method == "fhs") {
    fcasts[1, ] <- fhs(xstart, p = p, lambda = lambda, nboot = nboot,
                       arma = arma, ...)[[1]]
    if (nout > 1) {
      for (i in 2:nout) {
        if (i <= nwin) {
          fcasts[i, ] <- fhs(c(xstart[i:nwin],
                               xout[1:(i - 1)]), p = p,
                             lambda = lambda, nboot = nboot,
                             arma = arma, ...)[[1]]
        }
        else{
          fcasts[i, ] <- fhs(xout[(i - nwin):(i - 1)], p = p,
                             lambda = lambda, nboot = nboot,
                             arma = arma, ...)[[1]]
        }
      }
    }
  }
  VaR <- fcasts[, 1]
  ES <- fcasts[, 2]
  results <- list(VaR = VaR, ES = ES, xout = xout)

  class(results) <- "quarks"
  attr(results, "function") <- "rollcast"

  results
}
