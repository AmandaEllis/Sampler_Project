#Function that generates a new X by reallocating photos


new.X.reallocate<-function(X,t){
  current.n.ind<-max(X[,3])  #Computes the number of individuals in current X
  new.X<-X
  delete='FALSE'
  
  #select a capture occasion
  occasion<-sample(1:t,size=1)
  
  #choose removal location for photo and set individual for photo equal to NA
  remove.location<-sample(X[which(X[,2]==occasion),1],1)
  new.X[remove.location,3]<-NA
  #Photo that is moved, used when calculating the full conditional
  photos<-remove.location
  
  new.individual<-sample(1:current.n.ind,1)
  new.X[remove.location,3]<-new.individual
  
  if(length(unique(new.X[,3])) != (current.n.ind)) {
    new.X=X
    delete='TRUE'
  }

  return(list("new.X"=new.X,"photos"=photos,'delete'=delete,'occasion'=occasion))
  
  
}