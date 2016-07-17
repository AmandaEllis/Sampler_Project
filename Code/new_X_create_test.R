#Function that generates a new X by creating an individual

#The candidate X is created using the following steps:
#Choose to add new individual
#Simulate the number of occasions the individual was seen on using the current value of p
#Select the ID for the individual
#Simulate the number of photos that will be placed at each occasion
#Select photos

new.X.create<-function(X,t,lambda,p){

for(j in 1:100){
  #Number of indidivduals in current X
  current.n.ind<-max(X[,3]) 
  new.X<-X
  
  #delete keeps track of if we delete an individual
  #If an individual is deleted then we do not accept the candidate X
  delete='FALSE'  
  
  #Simulate capture history for new individual
  #Select number of occasions individual is seen on and which occasions using the current value of p
  #The individual must be observed on at least one occasion and not seen on all occasions
  
  #Start with the capture occasion being all zero.
  all.zero<-TRUE
  all.one<-TRUE
  #Simulate until capture occasion has at least 1 capture
  while(all.zero==TRUE || all.one==TRUE){
    w<-rep(NA,length=t)
    for (j in 1:t){
      w[j]<-rbinom(1,size=1,prob=p[j])
    }
    n.occasion<-sum(w)
    occasions<-which(w==1)
    if(n.occasion >0){all.zero=FALSE}
    if(n.occasion <t){all.one=FALSE}
  }
  
  #Next we select the ID for the individual
  new.individual<-sample(seq(from=.5,to=(current.n.ind+.5),by=1),1)
  
  #Renumber the individuals
  new.X[which(new.X[,3]>new.individual),3]=new.X[which(new.X[,3]>new.individual),3]+1
  
  #For each of the occasions the indidivual is seen on we pull photos from other individuals 
  #that were seen on the occasions the new ind. was not observed on
  #For each occasion we simulate the number of photos using the current value of lambda
  
  #First we find the individuals that were seen on the occasion the new indiv. was not seen on
  non.occasions<-which(w==0)
  individuals.seen<-unique(new.X[which(new.X[,2]%in%non.occasions),3])
  remove.location<-NA
  
  for(i in 1:n.occasion){
      #Select number of photos to remove
    n.photos<-rtpois(1,lambda)
    
    if(n.photos<=length(which(new.X[,2]==occasions[i] & new.X[,3]%in%individuals.seen))){
        ##Select the location of photos to be removed
        if(length(new.X[which(new.X[,2]==occasions[i] & new.X[,3]%in%individuals.seen),1])>1){
          remove.location<-sample(new.X[which(new.X[,2]==occasions[i] & new.X[,3]%in%individuals.seen),1],n.photos)
          }else{remove.location<-new.X[which(new.X[,2]==occasions[i] & new.X[,3]%in%individuals.seen),1]}
        
    #Sets the location of the photos to the new indidivual
    new.X[remove.location,3]<-new.individual+.5
    }else{      
          delete='TRUE'
          }
  }
 
  if(delete=='TRUE'){new.X<-X}
  
}
  return(list("new.X"=new.X,'photos'=remove.location,'new.ID'=(new.individual+.5),'delete'=delete))
  
}