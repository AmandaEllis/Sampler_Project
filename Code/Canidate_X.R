candidate.X<-function(current.X){
  
  #Canidate X is chosen by first selecting k photos from the first capture occasion.  
  #These photos are then randomly assigned to a different individuals
  #This is repeated for each capture occasion
  
  #Set candidate.X equal to current X
  candidate.X<-current.X
  #Add space for k new potential individuals
  for(l in 1:k){
    candidate.X[[(prev.ind+l)]]<-list(NULL)
  }
  
  prev.ind<-length(current.X)  #Computes the number of individuals in current X
  
  #For each capture occasion choose k photos and randomly assign to a different individual
  for(j in 1:t){
    
    #First find how many photos were taken of each individual on occasion j
    photos.per.indiv<-sapply(current.X,function(ind) if(length(ind)<j) 0 else length(ind[[j]]))
    cum.photo.indiv<-cumsum(photos.per.indiv)
    
    #Sample k photos from the capture occasion
    #Sample location
    location.tmp<-sample(sum(photos.per.indiv),k)
    #Find individual
    individual.tmp<-sapply(location.tmp,function(x) min(which(cum.photo.indiv >=x)))
    #find the photograph for each individual
    photo.tmp<-location.tmp-sapply(individual.tmp, function(x) cum.photo.indiv[x])
    
    #find photo ID and Remove those photos
    photo.ID.tmp<-rep(NA,length=k)
    for(l in 1:k){
      photo.ID.tmp[l]<-candidate.X[[individual.tmp[l]]][[j]][[photo.tmp[l]]]
      candidate.X[[individual.tmp[l]]][[j]][[photo.tmp[l]]]<-NULL
    }
    
    #Place photos
    #Number of photos each individual currently has
    photos.per.indiv.temp<-sapply(candidate.X,function(ind) if(length(ind)<j) 0 else length(ind[[j]]))
    #Select k individuals, change later to allow for same individual to be chosen more than once
    new.individuals<-sample(1:(prev.ind+k),k,replace=FALSE)
    #Number of photos the k inidivudals have
    new.photos.per.indiv<-photos.per.indiv.temp[new.individuals]
    
    #Place photos on the new individuals
    for(l in 1:k){
        candidate.X[[new.individuals[l]]][[j]][[new.photos.per.indiv[l]+1]]<-photo.ID.tmp[l]
    }
    
    
  }
    
    
    
    
    lapply(current.X, function(x) x!=26)
    remove.location<-as.vector(sample(which(!is.na(current.X[,j,])==FALSE),k))
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