
#[W|N,p] We consider W to be the capture histories generated under model M0
# Again we need only consider the individuals that are affected by the photograph that is moved.

d.W.given.N.p<-function(current.Y,candidate.Y,N,p){

  #In order to compute the acceptance probablity we need to compute W
  #For each individual and each time occasion and if a photo was taken sets capture equal to 1
  candidate.W<-candidate.Y
  candidate.W[candidate.Y>0]=1

  current.W<-current.Y
  current.W[current.Y>0]=1

  d.W.candidate.given.Np<-dbinom(sum(candidate.W),N,p,log=TRUE)
  d.W.current.given.Np<-dbinom(sum(current.W),N,p,log=TRUE)
  return(list("d.W.candidate.given.Np"=d.W.candidate.given.Np,"d.W.current.given.Np"=d.W.current.given.Np))
  
}