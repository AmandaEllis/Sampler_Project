
#[W|N,p] We consider W to be the capture histories generated under model M0
# Again we need only consider the individuals that are affected by the photograph that is moved.

d.W.given.N.p<-function(current.Y,candidate.Y,N,p,cap.occasions){

  #In order to compute the acceptance probablity we need to compute W
  #For each individual and each time occasion and if a photo was taken sets capture equal to 1
  candidate.W<-candidate.Y
  candidate.W[candidate.Y>0]=1
  
  current.W<-current.Y
  current.W[current.Y>0]=1
  
  #calcualtes the number of occasions in W
  if(is.matrix(candidate.W)==TRUE){
  n.occasions<-length(candidate.Y[1,])
  }else{n.occasions<-1}
  
  #Initialize density
  d.W.candidate.given.Np<-0
  d.W.current.given.Np<-0
  
  if(is.matrix(candidate.W)==TRUE){
  for(j in 1:n.occasions){
    d.W.candidate.given.Np.temp<-dbinom(sum(candidate.W[,j]),N,p[cap.occasions[j]],log=TRUE)
    d.W.current.given.Np.temp<-dbinom(sum(current.W[,j]),N,p[cap.occasions[j]],log=TRUE)
    
    d.W.candidate.given.Np<-d.W.candidate.given.Np+d.W.candidate.given.Np.temp
    d.W.current.given.Np<-d.W.current.given.Np+d.W.current.given.Np.temp
  }
  }else{
    d.W.candidate.given.Np<-dbinom(sum(candidate.W),N,p[cap.occasions],log=TRUE)
    d.W.current.given.Np<-dbinom(sum(current.W),N,p[cap.occasions],log=TRUE)
  }
  
  return(list("d.W.candidate.given.Np"=d.W.candidate.given.Np,"d.W.current.given.Np"=d.W.current.given.Np))
  
}