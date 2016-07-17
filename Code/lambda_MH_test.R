##################
#simulate the data
##################
  n=2000
  lambda=2
  tol=1e-10
  ## Initialize output
  x <- rep(NA,n)

  ## Identify lambda values below tolerance
  low <- which(lambda < tol)
  nlow <- length(low)

  if(nlow > 0){
  x[low] <- 1
  
  if(nlow < n)
    x[-low] <- qpois(runif(n-nlow, dpois(0, lambda[-low]), 1), lambda[-low])
  }else
      x <- qpois(runif(n-nlow, dpois(0, lambda), 1), lambda)
  
######
#MCMC
#####
  iterations<-1000
  burn.in<-100
  lambda.MH<-rep(NA, length=iterations+burn.in)
  
  lambda.MH[1]<-lambda
  
  for(i in 2:(iterations+burn.in)){  
    
    #sample candidiate lambda 
    candidate.lambda=runif(1,min=0, max=10) 
    
    FC.candidiate.lambda<-sum(log(candidate.lambda^(x-.5)/(exp(candidate.lambda)-1)))
    FC.current.lambda<-sum(log(lambda.MH[i-1]^(x-.5)/(exp(lambda.MH[i-1])-1)))
    
    #Next we compute the acceptance probability 
    ratio<-exp(FC.candidiate.lambda-FC.current.lambda)
    acceptance.prob=min(ratio,1)
    u.lambda<-runif(1)
    
    if (u.lambda<acceptance.prob){
      lambda.MH[i]<-candidate.lambda
    }else{
      lambda.MH[i]<-lambda.MH[i-1]
    }
  }
  
############
#Chain check
############
  
  (posterior.mean.lambda<-mean(as.vector(lambda.MH),na.rm=TRUE))
  plot(as.vector(lambda.MH),type='l')
  