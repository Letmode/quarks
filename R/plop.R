#' Profit & Loss operator function
#'
#' Calculates portfolio returns or losses by assigning weights
#'
#' @param x a numeric matrix of asset returns or losses
#' @param wts standardized portfolio weights (should add up to 1); by default
#' the portfolio is equally weighted
#' @param approxim controls if a first-order approximation for the calculation
#' of returns or losses is used; default is \code{'lin'} (first-order
#' approximation is employed)
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{pl}{Weighted portfolio returns or losses}
#' \item{wts}{Portfolio weights}
#' }
#' @examples
#' # creating portfolio
#' portfol <- cbind(SP500$price.close, DJI$price.close)
#' returns <- apply(portfol, 2, function(x) diff(log(x)))
#' # defining weights and applying the P&L operator function
#' wts <- c(0.4, 0.6)
#' portret <- plop(returns, wts = wts, approxim = 1)
#' portloss <- plop(-returns, wts = wts, approxim = 1)
#' plot.ts(cbind(portret$pl, portloss$pl))
#'
plop <-  function(x, wts = NULL, approxim = c(0, 1)) {

  if (!length(approxim) %in% c(1, 2) || any(is.na(approxim)) ||
      !is.numeric(approxim) || !all(approxim %in% c(0, 1))) {
    stop("A single integer value must be passed to 'approxim'.",
         "Valid choices are 0 or 1.")
  }
  if (all(approxim == c(0, 1))) approxim = 1
  x <- as.matrix(x)
  ncol <- dim(x)[2]
  if (is.null(wts)) wts <- rep(1 / ncol, ncol)

  switch(as.character(approxim),
         "0" =  {
           pl <- apply(x, 1, FUN = function(x, wts) exp(x) %*% wts - 1, wts)
         },
         "1" = {
           pl <- apply(x, 1, FUN = function(x, wts) x %*% wts, wts)
         })
  return(list(pl = pl,
              wts = wts))
}


