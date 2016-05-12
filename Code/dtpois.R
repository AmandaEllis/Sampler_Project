dtpois <- function(X, lambda,return.log='FALSE'){
  ## density zero-truncated Poisson
  
  ## Initialize output
  density <- NA

  if(return.log=='FALSE'){
  density<-(lambda^X)/((exp(lambda)-1)*factorial(X))
  }else{
    density<-log((lambda^X)/((exp(lambda)-1)*factorial(X)))
    }
  
  return(density)
}

