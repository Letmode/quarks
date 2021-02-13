hsim_switch <- function(x, p = 0.95, method = c("age", "basic"), lambda = 0.98){
  results <- switch(method,
                  basic = {
                    l <- sort(-x)
                    VaR <- stats::quantile(l, p, na.rm = TRUE)
                    ES <- mean(l[l >= VaR[1]])
                    list(VaR = VaR, ES = ES)
                  },
                  age = {
                    n <- length(x)
                    w <- lambda ^ ((1:n) - 1) * (1 - lambda) / (1 - lambda ^ n)
                    l <- sort(-x)
                    l.ind <- order(-x)
                    pcum <- cumsum(w[l.ind])
                    VaR.ind <- which(pcum > p)[1]
                    VaR <- l[VaR.ind]
                    ES <- mean(l[l >= VaR])
                    list(VaR = VaR, ES = ES)
                  })
}
