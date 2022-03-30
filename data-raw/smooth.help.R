smooth.help <- function(x, smoothscale, smoothopts) {
  meanxc <- mean(x)
  xc <- x - meanxc
  xsmooth <- log(xc^2)
  smoothopts <- c(list(y = x), smoothopts)
  if (smoothscale == "auto") {
    np.est <- do.call(what = smoots::msmooth, args = smoothopts)
  }
  if (smoothscale == "lpr") {
    np.est <- do.call(what = smoots::gsmooth, args = smoothopts)
  }
  mule <- -log(mean(exp(np.est[["res"]])))
  sxt <- exp(0.5 * (np.est[["ye"]] - mule))
  xstd <- x / sxt
  nxstd <- length(xstd)
  return(list(xstd = xstd,
              sfc = sxt[nxstd]))
}
