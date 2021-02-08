#'@export

ewma = function(x, lambda = 0.94){
  n = length(x)
  vol = rep(NA, n)
  vol[1] = lambda * stats::var(x) + (1 - lambda) * stats::var(x)
  for(i in 2:n){
    vol[i] = lambda * vol[i-1] + (1 - lambda) * x[i-1]^2
  }
  drop(vol)
}
