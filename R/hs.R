#' Nonparametric calculation of univariate Value at Risk and Expected Shortfall
#'
#' Computes Value at Risk and Expected Shortfall by means of plain and age weighted historical simulation
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.95\%
#' @param method method to be used for calculation; default is "plain"
#' @param lambda decay factor for the calculation of weights; default is 0.98
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{Calculated Value at Risk}
#' \item{ES}{Calculated Expected Shortfall}
#' }
#' @examples
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' hs(x = returns, p = 0.95, method = "plain") # plain historical simulation
#' hs(x = returns, p = 0.95, method = "age", lambda = 0.98) # age weighted historical simulation
hs <- function(x, p = 0.95, method = c("age", "plain"), lambda = 0.98) {
  if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
    stop(
      "A numeric vector of length > 1 and without NAs must be passed to",
      " 'x'."
    )
  }
  if (length(p) != 1 || is.na(p) || !is.numeric(p) ||
    (p <= 0)) {
    stop(
      "The argument 'p' must be a single non-NA double value with ",
      "0 < p < l."
    )
  }
  if (length(lambda) != 1 || is.na(lambda) || !is.numeric(lambda) ||
    (lambda < 0)) {
    stop(
      "The argument 'lambda' must be a single non-NA double value with ",
      "0 < lambda < 1."
    )
  }

  if (all(method == c("age", "plain"))) method <- "plain"

  if (method == "plain") {
    l <- sort(-x)
    VaR <- stats::quantile(l, p, na.rm = TRUE)
    ES <- mean(l[l >= VaR[1]])
  }

  if (method == "age") {
    n <- length(x)
    w <- lambda^((1:n) - 1) * (1 - lambda) / (1 - lambda^n)
    l <- sort(-x)
    l.ind <- order(-x)
    pcum <- cumsum(w[l.ind])
    VaR.ind <- which(pcum > p)[1]
    VaR <- l[VaR.ind]
    ES <- mean(l[l >= VaR])
  }
  results <- cbind(VaR = VaR, ES = ES)
  colnames(results) <- c("VaR", "ES")
  rownames(results) <- paste0(100 * p, "%")
  results
}
