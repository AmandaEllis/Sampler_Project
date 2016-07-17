#Function that generates a new X by deleting an individual
#The candidate X is created using the following steps:
#Choose to delete an individual
#Select the individual
#reallocate the photos


new.X.delete<-function(X,t){
  
  new.X<-X
  
  #Select individual to delete
  #We select from the indidivuals not seen on all occasions
  Y<-X_to_Y(X,t)
  W<-Y
  W[Y>0]=1
  
  individuals.seen.not.all<-which(rowSums(W)<t)
  
  lost.ID<-sample(individuals.seen.not.all,size=1)
  #photos that will need to be moved
  photos.allocate<-X[which(X[,3]==lost.ID),1]
  
  #Set the ID for the photos to NA
  new.X[photos.allocate,3]<-NA
  
  new.ID<-sort(unique(new.X[,3]))  #Current IDs in new X
  
  #Since an ID was lost we renumber the individuals    
  temp<-c(1,lost.ID,(max(new.ID)+1))
  for(j in 1:2){
    new.X[which(new.X[,3]>=temp[j]&new.X[,3]<temp[j+1]),3]<-new.X[which(new.X[,3]>=temp[j]&new.X[,3]<temp[j+1]),3]-(j-1)
  }
  
  new.ind<-max(new.X[,3],na.rm=TRUE) #Computes the number of individuals in new X
  
  #Reallocate photos
  for(i in 1:length(photos.allocate)){
    new.X[photos.allocate[i],3]<-sample(1:new.ind,size=1)
  }
  
  return(list("new.X"=new.X,'photos'=photos.allocate,'deleted.Id'=lost.ID,'delete'=FALSE))
  
}