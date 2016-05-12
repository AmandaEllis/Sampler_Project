#[X|Y] X is considered uniform over the space defined by Y and the observed occasion of each photograph
# We only need to consider the column of Y which corresponds to the capture occasion the selected photo was taken in
# Further we need only consider the individual that the photograph moves to and from.


d.X.given.Y<-function(current.X,candidate.X,t,cap.occasion){

  #Compute Y for the candidate and current X, we only need the column of Y corresponding to the capture occasion the selected photo was taken in
  candidate.Y<-X_to_Y(candidate.X,t)[,cap.occasion]
  current.Y<-X_to_Y(current.X,t)[,cap.occasion]

  #Remove the 0's, Given that Y is zero we know that individual was not photographed so we do not need to consider it
  candidate.Y.n0<-candidate.Y[which(candidate.Y!=0)]
  current.Y.n0<-current.Y[which(current.Y!=0)]

  #We need to account for the possibility that there was no change in Y
  if(length(candidate.Y.n0)==length(current.Y.n0)){ 
    if(sum(sort(candidate.Y.n0)!=sort(current.Y.n0))==0){
      return(list("d.X.given.candidate.Y"=0,"d.X.given.current.Y"=0))
    }
  }
    
  #Find the elelments that differ
  current.Y.ind<-list()       #initialize the list for the indidivuals that differ
  candidate.Y.ind<-list()     
  
    
  max<-max(c(candidate.Y.n0,current.Y.n0))  #maximum number of photographs taken of an individual
  
  current.Y.temp<-0
  candidate.Y.temp<-0
  
  #Find the individuals whose number of photographs differs between the candidate and current
  for(i in 1:max){
    current.Y.temp<-length(which(current.Y.n0==i))     #How many individuals had thier photo taken i times in the current Y
    candidate.Y.temp<-length(which(candidate.Y.n0==i)) #How many individuals had thier photo taken i times in the candidiate Y
    
    if(candidate.Y.temp>current.Y.temp){
      #If there are more individuals in the candidate Y with thier photo taken i times, we add those individuals to candidate list
      candidate.Y.ind<-c(candidate.Y.ind,rep(i,candidate.Y.temp-current.Y.temp))  
    }else if (candidate.Y.temp<current.Y.temp)
       #If there are more individuals in the current Y with thier photo taken i times, we add those individuals to candidate list
        current.Y.ind<-c(current.Y.ind,rep(i,current.Y.temp-candidate.Y.temp))
  }
  
  current.Y.ind<-unlist(current.Y.ind)
  candidate.Y.ind<-unlist(candidate.Y.ind)
  
  #Calculates the sum of the similar elements of Y for the candidate and current Y (Code could have replaced Current.Y.no with same results)
  sum.similar<-sum(candidate.Y.n0)-sum(candidate.Y.ind) 
  #Calculates the number of photos taken on selected occasion (Code could have replaced Current.Y.no with same results)
  sum.photos.occasion<-sum(candidate.Y.n0)
  
  #At most current.Y.ind and candidate.Y.ind will have 2 elements since we only move 1 photo, so we only need to calculate the first one
  d.X.given.candidate.Y<-log(1/(choose((sum.photos.occasion-sum.similar),candidate.Y.ind[1])))
  d.X.given.current.Y<-log(1/(choose((sum.photos.occasion-sum.similar),current.Y.ind[1])))
  
  return(list("d.X.given.candidate.Y"=d.X.given.candidate.Y,"d.X.given.current.Y"=d.X.given.current.Y,
              "current.Y.ind"=current.Y.ind,"candidate.Y.ind"=candidate.Y.ind,"candidate.Y"=candidate.Y,"current.Y"=current.Y))
  
}
    