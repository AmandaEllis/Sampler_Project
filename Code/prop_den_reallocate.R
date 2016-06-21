prop.den.reallocate.fun<-function(X,occasion){
  
  n.photos.occasion<-
  
  
  occasion.prob<-log(1/t)                            #Choose an occasion
  n.occasions.prob <- log(1/t)                 #Select number of occasions
  occasions.prob<-  log(1/(choose(t,t.star)))  #Select the occasions
  ID.prob<-  log(1/(max(given.X[,3]) +1))            #Select the ID for the individual
  

  density<- add.prob+n.occasions.prob+occasions.prob+ID.prob+photo.prob+lambda.prob
  
  return(list('density'=density)) 
}