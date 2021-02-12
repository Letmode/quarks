rollcast <- function(x, p = 0.95, method = c("hsim", "agesim", "vwhsim"), lambda = c(0.94, 0.98),
                     nout = 250, nwin = 250){
  if (all(lambda == c(0.94, 0.98)) && (method == "hsim" || method == "agesim")) lambda = 0.98
  if (all(lambda == c(0.94, 0.98)) && method == "vwhsim") lambda = 0.94
  n = length(x)
  nin = n - nout
  xin <- x[1:nin]
  xout = x[(nin + 1) : nout]
  xstart = xin[(nin - nwin):nin]
  fcasts = matrix(NA, nout, 2, dimnames = list(c(), c("VaR", "ES")))
  if (method == "hsim"){
    fcasts[1, ] = hsim(xstart, p = p, method = "basic")
    for (i in 2:nout){
      fcasts[i, ] = hsim(c(xstart[i:nwin], xout[1:(i - 1)]), p = p, method = "basic")
    }
  }
  if (method == "agesim"){
    fcasts[1, ] = hsim(xstart, p = p, method = "age", lambda = lambda)
    for (i in 2:nout){
      fcasts[i, ] = hsim(c(xstart[i:nwin], xout[1:(i - 1)]), p = p, method = "age", lambda = lambda)
    }
  }
  if (method == "vwhsim"){
    fcasts[1, ] = vwhsim(xstart, p = p, lambda = lambda)
    for (i in 2:nout){
      fcasts[i, ] = vwhsim(c(xstart[i:nwin], xout[1:(i - 1)]), p = p, lambda = lambda)
    }
  }
  VaR <- fcasts[, 1]
  ES <- fcasts[, 2]
  results <- list(VaR = VaR, ES = ES)
  results
}
