#[X|Y] X is considered uniform over the space defined by Y and the observed occasions of the photographs
# We only need to consider the columns of Y which corresponds to the capture occasions the selected photos were taken in
# Further we need only consider individuals that the photographs moves to and from.
# Code currently works if lambda is the same across individuals and occasions


d.X.given.Y<-function(current.X,candidate.X,t,photos){
  
  #Find the occasions which the photos were taken in 
  cap.occasions<-sort(unique(current.X[photos,2]))

  #Compute Y for the candidate and current X, we only need the column of Y corresponding to the capture occasion the selected photos were taken in
  candidate.Y<-as.matrix(X_to_Y(candidate.X,t)[,cap.occasions])
  current.Y<-as.matrix(X_to_Y(current.X,t)[,cap.occasions])

  #Remove the 0's
  #Given that Y is zero we know that individual was not photographed so we do not need to consider it
  candidate.Y.n0<-list()
  current.Y.n0<-list()
  #For each of the capture occasions we create a list of the number of photographs taken for those individuals that were photographed
  for(i in 1:length(cap.occasions)){
    candidate.Y.n0[[i]]<-candidate.Y[which(candidate.Y[,i]!=0),i]
    current.Y.n0[[i]]<-current.Y[which(current.Y[,i]!=0),i]
  }  
  
  #We need to account for the possibility that there was no change in Y
  # We do this by comparing the elements in each column of the 2 Y's
  same.col<-NA

  for(i in 1:length(cap.occasions)){
    if(length(current.Y.n0[[i]])==length(candidate.Y.n0[[i]])){              #First check to see if length is the same, if not then the column differs
    if(sum(sort(candidate.Y.n0[[i]])!=sort(current.Y.n0[[i]]))==0){          #Sort each list and compare elements, is sum is not zero then column differs
    same.col<-c(same.col,i)                       #records the columns that are the same between the current and candidate
    }
    }
  }
  
  same.col<-sort(same.col[!is.na(same.col)])
  
  
  #If the number of same occasions is the same as the number of cap.occasions in which the photos were taken there is no change in Y
  #Since X is defined uniform over Y, if the Ys are the same then the densities are the same
  if(length(same.col)==length(cap.occasions)){
  return(list("d.X.given.candidate.Y"=0,"d.X.given.current.Y"=0,
              "current.Y.ind"=NA,"candidate.Y.ind"=NA,"candidate.Y"=candidate.Y,"current.Y"=current.Y,same=TRUE)) 
  }
  
  #Remove the same columns from the candidate Y and current Y and cap.occasions
  if(length(same.col)>0){
  candidate.Y<-candidate.Y[,-same.col]
  current.Y<-current.Y[,-same.col]
  
  candidate.Y.n0<-candidate.Y.n0[-same.col]
  current.Y.n0<-current.Y.n0[-same.col]
  
  cap.occasions<-cap.occasions[-same.col]
  }
  #Next we compute the density of X given Y
  #To do this we need to calculate the cardinality of X given Y since X is uniform over Y
  #We only need the portion of the capture occasion that differs between the candidate and the current
  #For example consider there are 8 photographs.  Candidate.Y<-c(3,2,2,1) Current.Y<-c(3,3,2)
  #The cardinatlity of the candidiate can be written as choose(8,3)*choose(5,2)*choose(3,2)*choose(1,1)
  #The cardinatlity of the current can be written as choose(8,3)*choose(5,2)*choose(3,3)
  #Using cancellation we only need choose(3,2) for the candidiate
  
  #initialize density value, we are working with log density
  d.X.given.candidate.Y<-0     
  d.X.given.current.Y<-0
  
  #Keeps tracks of the elements of the current Y and candidate Y that differ within a capture occasion
  #This is used when calculating [Y|W,lambda] and is output
  current.Y.ind.keep<-NA  
  candidate.Y.ind.keep<-NA
  
  #loop over the capture occasions
  for(i in 1: length(cap.occasions)){
  current.Y.ind<-NA      #initialize  the individuals that differ for the current capture occasion
  candidate.Y.ind<-NA     
  
  candidate.Y.n0.temp<-unlist(candidate.Y.n0[[i]])  #Unlist the number of photographs taken in the current occasion
  current.Y.n0.temp<-unlist(current.Y.n0[[i]])
  
  max<-max(c(candidate.Y.n0.temp,current.Y.n0.temp))  #maximum number of photographs taken of an individual in the current occasion
  
  #Find the individuals whose number of photographs differs between the candidate and current
  for(j in 1:max){
    current.Y.temp<-length(which(current.Y.n0.temp==j))     #How many individuals had thier photo taken i times in the current Y
    candidate.Y.temp<-length(which(candidate.Y.n0.temp==j)) #How many individuals had thier photo taken i times in the candidiate Y
    
    if(candidate.Y.temp>current.Y.temp){
      #If there are more individuals in the candidate Y with thier photo taken i times, we add those individuals to candidate list
      candidate.Y.ind<-c(candidate.Y.ind,rep(j,candidate.Y.temp-current.Y.temp))  
    }else if (candidate.Y.temp<current.Y.temp){
       #If there are more individuals in the current Y with thier photo taken i times, we add those individuals to candidate list
        current.Y.ind<-c(current.Y.ind,rep(j,current.Y.temp-candidate.Y.temp))
    }
  }
  
  current.Y.ind<-sort(current.Y.ind[!is.na(current.Y.ind)])           #Remove NA from current and candidate elements that differ
  candidate.Y.ind<-sort(candidate.Y.ind[!is.na(candidate.Y.ind)])
  
  current.Y.ind.keep<-c(current.Y.ind.keep,current.Y.ind)             #Add elements that differ for output
  candidate.Y.ind.keep<-c(candidate.Y.ind.keep,candidate.Y.ind)
  
  #Calculates the sum of the similar elements of Y for the candidate and current Y (Code could have replaced Current.Y.no with same results)
  sum.similar<-sum(candidate.Y.n0.temp)-sum(candidate.Y.ind) 
  #Calculates the number of photos taken on selected occasion (Code could have replaced Current.Y.no with same results)
  sum.photos.occasion<-sum(candidate.Y.n0.temp)
  
  #number of photos available to to distribute
  total.available.current<-sum.photos.occasion-sum.similar
  total.available.candidate<-sum.photos.occasion-sum.similar
  
  #Iteratively calculate the density for the elements of the current capture occasion for the current Y
  if(length(current.Y.ind)>=2){
    for(j in 1:(length(current.Y.ind)-1)){
     d.X.given.current.Y<-d.X.given.current.Y+log(1/(choose(total.available.current,current.Y.ind[j]))) 
     #Update the number of photos available
     total.available.current<-total.available.current-current.Y.ind[j]
    }
  }else{d.X.given.current.Y=d.X.given.current.Y+0}
  
  #Iteratively calculate the density for the elements of the current capture occasion for the candidate Y
  if(length(candidate.Y.ind)>=2){
    for(j in 1:(length(candidate.Y.ind)-1)){
      d.X.given.candidate.Y<-d.X.given.candidate.Y+log(1/(choose(total.available.candidate,candidate.Y.ind[j]))) 
      #Update the number of photos available
      total.available.candidate<-total.available.candidate-candidate.Y.ind[j]
    }
  }else{d.X.given.candidate.Y=d.X.given.candidate.Y+0}
  
  }
  
  current.Y.ind.keep<-current.Y.ind.keep[!is.na(current.Y.ind.keep)]
  candidate.Y.ind.keep<-candidate.Y.ind.keep[!is.na(candidate.Y.ind.keep)]
  
  return(list("d.X.given.candidate.Y"=d.X.given.candidate.Y,"d.X.given.current.Y"=d.X.given.current.Y,
              "current.Y.ind"=current.Y.ind.keep,"candidate.Y.ind"=candidate.Y.ind.keep,
              "candidate.Y"=candidate.Y,"current.Y"=current.Y,same=FALSE,"cap.occasions"=cap.occasions))
  
}
    