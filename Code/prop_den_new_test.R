#This function computes the propsal density of X when a new indidivual is created
#The given X goes second in the function call

#The candidate X is created using the following steps:
#Choose to add new individual
#Simulate the number of occasions the individual was seen on using the current value of p
#Select the ID for the individual
#Simulate the number of photos that will be placed at each occasion
#Select photos

prop.den.new.fun<-function(X,given.X,q,t,new.ID,lambda,p){
  
  occasions<-sort(unique(X[which(X[,3]==new.ID),2])) #Occasions new individual was photographed in
  non.occasions<-c(1:t)[!(c(1:t)%in%occasions)]
 
  add.prob<-log(q)                            #Choose to add new individual
  
  #initialize the probabily for the occasions the individual was seen on
  n.occasions.prob <-0
  #Rereate the capture history
  cap.history<-rep(0,length=t)
  cap.history[occasions]<-1
  #The probability that an individual as a capture history of all zeros.
  all.zero.prob<-prod(1-p)
  all.one.prob<-prod(p)
  for(j in 1:t){
    occasion.prob<-log(dbinom(cap.history[j],size=1,prob=p[j]))
    n.occasions.prob<-n.occasions.prob+occasion.prob
  }
  n.occasions.prob<-n.occasions.prob-log(1-all.zero.prob-all.one.prob)
  
  ID.prob<-  log(1/(max(given.X[,3]) +1))            #Select the ID for the individual
  
  #initialize the probability of the photos selected and the number of photos
  photo.prob<-0
  lambda.prob<-0
  
  individuals.seen<-unique(given.X[which(given.X[,2]%in%non.occasions),3])
  
  for(i in 1:length(occasions)){
    #number of photos in the selected occasion
    n.photos.occasion<-length(which(given.X[,2]==occasions[i] & given.X[,3]%in%individuals.seen))
    #number of photos for the new indidivual taken in the occasion
    n.photos.occasion.new<-length(which(X[which(X[,3]==new.ID),2]==occasions[i]))

    photo.prob<-log(1/((choose(n.photos.occasion,n.photos.occasion.new))))+photo.prob
    lambda.prob<-dtpois(n.photos.occasion.new,lambda,return.log='TRUE')+lambda.prob
  }
  
  density<- add.prob+n.occasions.prob+ID.prob+photo.prob+lambda.prob
  
  return(list('density'=density)) 
}
