#' Unconditional and Conditional Coverage Tests, Independence Test
#'
#' The conditional (Kupiec, 1995), the unconditional coverage
#' test (Christoffersen, 1998) and the independence test (Christoffersen, 1998)
#' of the Value-at-Risk (VaR) are applied.
#'
#' @param obj a list that contains the following elements:
#' \describe{
#' \item{\code{loss}}{a numeric vector that contains the values of a loss series
#' ordered from past to present; is set to \code{NULL} by default.}
#' \item{\code{VaR}}{a numeric vector that contains the estimated values of the
#' VaR for the same time points of the loss series \code{loss};
#' is set to \code{NULL} by default.}
#' \item{\code{p}}{a numeric vector with one element; defines the probability p
#' stated in the null hypotheses of the coverage tests (see the section
#' \code{Details} for more information); is set to \code{NULL} by default.}
#' }
#' @param conflvl a numeric vector with one element; the significance
#' level at which the Null Hypothesis is evaluated; is set to \code{0.95} by
#' default.
#' Please note that a list returned by the \code{rollcast} function can be directly
#' passed to \code{cvgtest}.
#'
#' @export
#'
#' @importFrom stats 'pchisq'
#'
#' @details
#' The function needs three inputs: the out-of-sample loss series \code{obj$loss}, the
#' corresponding estimated \code{obj$VaR} series and the coverage level \code{obj$p},
#' for which the VaR has been calculated. If an object returned by this function
#' is entered into the R console, a detailed overview of the test
#' results is printed.
#'
#' @return
#' A list of class \code{quarks} with the following four elements:
#' \describe{
#' \item{p}{probability p stated in the null hypotheses of the coverage tests}
#' \item{p.uc}{the p-value of the unconditional coverage test}
#' \item{p.cc}{the p-value of the conditional coverage test}
#' \item{p.ind}{the p-value of the independence test}
#' \item{conflvl}{the significance level at which the Null Hypothesis is
#' evaluated}
#' \item{model}{selected model for estimation; only available if a list
#' returned by the \code{rollcast} function is passed to \code{cvgtest}}
#' \item{method}{selected method for estimation; only available if a list
#' returned by the \code{rollcast}) function is passed to \code{cvgtest}}
#' }
#'
#' @references
#' Christoffersen, P. F. (1998). Evaluating interval forecasts. International
#' economic review, pp. 841-862.
#'
#' Kupiec, P. (1995). Techniques for verifying the accuracy of risk measurement
#' models. The J. of Derivatives, 3(2).
#'
#'
#' @examples
#'
#' prices <- DAX$price.close
#' returns <- diff(log(prices))
#' n <- length(returns)
#' nout <- 250 # number of obs. for out-of-sample forecasting
#' nwin <- 500 # window size for rolling forecasts
#' results <- rollcast(x = returns, p = 0.975, method = 'age', nout = nout,
#'                      nwin = nwin)
#' cvgtest(results)
#'

cvgtest <- function(obj = list(loss = NULL, VaR = NULL, p = NULL), conflvl = 0.95) {
  if (!is.list(obj) && !is.data.frame(obj)) {
    stop("A list or data frame containing two vectors with equal
         length and without NAs as well as a single numeric value
         must be passed to", " 'obj'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["loss"]]) <= 1 ||
                                   !all(!is.na(obj[["loss"]])) ||
                                   !is.numeric(obj[["loss"]]))) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'obj[['loss']]'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["VaR"]]) <= 1 ||
                                   !all(!is.na(obj[["VaR"]])) ||
                                   !is.numeric(obj[["VaR"]]))) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'obj[['VaR']]'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["p"]]) != 1 ||
                                   is.na(obj[["p"]]) ||
                                   !is.numeric(obj[["p"]]) ||
                                   obj[["p"]] <= 0 || obj[["p"]] >= 1)) {
    stop("A single numeric value that satisfies >0 and <1 must be passed to",
         " 'obj[['p']]'")
  }

  if(inherits(obj, "quarks")) {
    loss <- -obj[["xout"]]
    VaR <- obj[["VaR"]]
    p <- obj[["p"]]
    model <- obj[["model"]]
    method <- obj[["method"]]
  }
  else {
    loss <- obj[["loss"]]
    VaR <- obj[["VaR"]]
    if (is.null(model)) model <- NA
    if (is.null(method)) method <- NA
  }

  n.out <- length(loss)
  It <- loss > VaR
  n0 <- sum(1 - It[1:n.out])
  n1 <- sum(It[1:n.out])
  if (n1 == 0) {
    stop("No VaR violations found. Tests are not applicable.")
  }
  Itf <- It[1:(n.out - 1)]
  Its <- It[2:n.out]
  diff.It <- Itf - Its
  diff.It.0 <- diff.It == 0
  n00 <- sum(diff.It.0[Itf == 0])
  n01 <- sum(diff.It == -1)
  n10 <- sum(diff.It == 1)
  n11 <- sum(diff.It.0[Itf == 1])

  puc <- n1 / n.out
  p01 <- n01 / (n00 + n01)
  p11 <- n11 / (n10 + n11)

  Tp <- p^n0 * (1 - p)^n1
  T1 <- (1 - puc)^n0 * puc^n1

  if (n11 == 0) {
    T2 <- (1 - p01)^n00 * p01^n01
  } else {
    T2 <- (1 - p01)^n00 * p01^n10 * (1 - p11)^n10 * p11^n11
  }

  LRuc <- -2 * log(Tp / T1)
  LRind <- -2 * log(T1 / T2)
  LRcc <- -2 * log(Tp / T2)
  p.uc <- 1 - pchisq(LRuc, 1)
  p.ind <- 1 - pchisq(LRind, 1)
  p.cc <- 1 - pchisq(LRcc, 2)

  result <- list(p = p,
                 p.uc = p.uc,
                 p.ind = p.ind,
                 p.cc = p.cc,
                 conflvl = conflvl,
                 method = method,
                 model = model)
  class(result) <- "quarks"
  attr(result, "function") <- "cvgtest"
  result
}
