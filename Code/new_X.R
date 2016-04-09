#Function that generates a new X
#New X is chosen by first selecting k photos from the first capture occasion.  
#These photos are then randomly assigned to a different individual
#This is repeated for each capture occasion

new.X<-function(X,k,t){
prev.ind<-max(X[,3])  #Computes the number of individuals in current X
new.X<-X
#For each capture occasion choose k photos and randomly assign to a different individual
for(j in 1:t){
    #choose removal location for k photos and remove those photos
    remove.location<-sample(X[which(X[,2]==j),1],k)
    remove.photos<-X[remove.location,1]
    
    #Relocate the k photos
    new.temp<-1
    for(l in 1:k){
    #sample individual and place photo 
    #Individual is sampled from current individuals plus allowance for new individual
    new.individual<-sample(1:(prev.ind+new.temp),1)
    new.X[remove.location[l],3]<-new.individual
      if(new.individual==prev.ind+new.temp){new.temp=new.temp+1}
    } 
  }

#If we remove all of the photographs for an individual we need to renumber the individuals
#Otherwise we will have zero rows in W and favor moving photographs to a new individuals 
#when selecting new X.
  new.ID<-sort(unique(new.X[,3]))  #Current IDs in new X
  old.ID<-c(1:max(new.ID))          #IDs in previous X
  lost.ID<-old.ID[-which(old.ID%in% new.ID)]  #IDs lost when generating new X
  num.lost.ID<-length(lost.ID)

  if(num.lost.ID>0){
  lost.ID<-c(1,lost.ID,(max(new.ID)+1))
  for(j in 1:(num.lost.ID+1)){
    new.X[which(new.X[,3]>=lost.ID[j]&new.X[,3]<lost.ID[j+1]),3]<-new.X[which(new.X[,3]>=lost.ID[j]&new.X[,3]<lost.ID[j+1]),3]-(j-1)
    }
  }
  return(new.X)

}