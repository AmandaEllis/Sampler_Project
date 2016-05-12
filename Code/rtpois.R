rtpois <- function(n, lambda,tol=1e-10){
  ## Simulate from zero-truncated Poisson
  
  ## Initialize output
  x <- rep(NA,n)
  
  ## Identify lambda values below tolerance
  low <- which(lambda < tol)
  nlow <- length(low)
  
  if(nlow > 0){
    x[low] <- 1
    
    if(nlow < n)
      x[-low] <- qpois(runif(n-nlow, dpois(0, lambda[-low]), 1), lambda[-low])
  }
  else
    x <- qpois(runif(n-nlow, dpois(0, lambda), 1), lambda)
  
  return(x)
}