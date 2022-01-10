kama <- function(x, neff = 10, nfast = 2, nslow = 30) {
  n <- length(x)
  vol <- rep(NA, n)
  ch <- c(rep(0, neff), abs(diff(x, neff)))
  vol[1] <- x[1]
  for (i in 2:n) {
    if (i > neff) {
      er <- ch[i] / sum(abs(diff(x[(i - neff):i])))
    }
    else {
      er <- 0
    }
    sc <- (er * (2 / (nfast + 1) - 2 / (nslow + 1)) + 2 / (nslow + 1))^2
    vol[i] <- vol[i - 1] + sc * (x[i] - vol[i - 1])
  }
  vol
}

# prices <- SP500$price.close
# returns <- diff(log(prices))
# date <- SP500$ref.date#[-1]
# cvar <- kama(x = prices)
# csig <- cvar
# matplot(date, cbind(prices, csig), type = 'll',
# main = 'conditional standard deviations for the DAX30 return series')
