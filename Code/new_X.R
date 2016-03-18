#Function that generates a new X
#New X is chosen by first selecting k photos from the first capture occasion.  
#These photos are then randomly assigned to a different individual
#This is repeated for each capture occasion

new.X<-function(X,k){
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
  return(new.X)

}