#Function that generates a new X
#New X is chosen by first selecting k photos from the first capture occasion.  
#These photos are then randomly assigned to a different individual
#This is repeated for each capture occasion

new_X<-function(X,k){

  
  prev.ind<-nrow(X.MH[[i-1]])  #Computes the number of individuals in current X
  
  #Computes the maximum number of photos per an individual on an occasion
  if(is.matrix(X.MH[[i-1]])==TRUE){max.photo<-1
  }else{max.photo<-length(X.MH[[i-1]][1,1,])}
  
  #Augment current X array by appending empty space for k photos for each individual on each occasion
  canidate.X.tmp<-abind(X.MH[[i-1]],array(NA,dim=c(prev.ind,t,k)))
  
  #Augment current X array by appending empty space for t*k individuals on each occasion
  canidate.X<-array(NA,dim=c(prev.ind+t*k,t,max.photo+k))
  new.indiv<-matrix(NA,ncol=t,nrow=t*k)
  for(j in 1:(max.photo+k)){
    canidate.X[,,j]<-rbind(canidate.X.tmp[,,j],new.indiv)
  }
  
  #For each capture occasion choose k photos and randomly assign to a different individual
  for(j in 1:t){
    #choose removal location for k photos and remove those photos
    remove.location<-as.vector(sample(which(!is.na(canidate.X[,j,])==FALSE),k))
    remove.photos<-canidate.X[,j,][remove.location]
    canidate.X[,j,][remove.location]<-NA
    #Relocate the k photos
    for(l in 1:k){
      #sample individual and place photo
      exit='F'
      tmp.indiv<-sample(1:prev.ind+t*k, 1)
      for(m in 1:(max.photo+k)){
        if(is.na(canidate.X[tmp.indiv,j,m])==TRUE && exit=='F'){
          canidate.X[tmp.indiv,j,m]<-remove.photos[l]
          exit='T'
        }
      }
    } 
  }
  
  #Remove individuals from array that are all NA, ie have no photos
  keep<-c(1:nrow(canidate.X))
  for(j in 1:nrow(canidate.X)){
    if(sum(canidate.X[j,,]!='NA',na.rm = TRUE)==0){keep[j]=NA}
  }
  keep<-keep[-which(is.na(keep))]
  canidate.X=canidate.X[keep,,]
  
  #Remove matrices from 3rd dimension of array that are all NA
  keep<-c(1:(max.photo+k))
  for (j in 1:(max.photo+k)){
    if(sum(canidate.X[,,j]!='NA',na.rm = TRUE)==0){keep[j]=NA}
  }
  keep<-keep[-which(is.na(keep))]
  canidate.X=canidate.X[,,keep]
  
}