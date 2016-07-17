#This function computes the propsal density of X when a new indidivual is created
#The given X goes second in the function call

#The candidate X is created using the following steps:
#Choose to delete an individual
#Select the individual
#reallocate the photos

prop.den.delete.fun<-function(X,given.X,q,t,deleted.Id){
  
  #Select individual to delete
  #We selct from the indidivuals not seem on all occasions
  Y<-X_to_Y(given.X,t)
  W<-Y
  W[Y>0]=1
  
  individuals.seen.not.all<-which(rowSums(W)<t)
  current.n.ind<-length(individuals.seen.not.all)
  
  delete.prob<-log(1-q)                        #Choose to add new individual
  n.ind.prob <- log(1/current.n.ind)           #Select the individual
  
  #total number of photos lost ID has
  total.photos<-length(which(given.X[,3]==deleted.Id))
  photo.prob<-log((1/(current.n.ind-1))^total.photos)
  
  density<- delete.prob+n.ind.prob+photo.prob
  
  return(list('density'=density)) 
}