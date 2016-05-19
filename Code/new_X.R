#Function that generates a new X
#New X is chosen by selecting a single photograph from a random ocassion 
#These photos are then randomly assigned to a different individual

new.X.fun<-function(X,t){
prev.ind<-max(X[,3])  #Computes the number of individuals in current X
new.X<-X

#choose removal location for photo and set individual for photo equal to NA
remove.location<-sample(X[which(X[,2]==t),1],1)
new.X[remove.location,3]<-NA
photo<-remove.location

#If we remove all of the photographs for an individual we need to renumber the individuals
#Otherwise we will have zero rows in W and favor moving photographs to a new individuals 
#when selecting new X.
new.ID<-sort(unique(new.X[,3]))  #Current IDs in new X
old.ID<-c(1:prev.ind)            #IDs in previous X
lost.ID<-old.ID[-which(old.ID%in% new.ID)]  #IDs lost when generating new X
num.lost.ID<-length(lost.ID)  #length is 1 or zero.

#if an ID was lost we renumber the individuals    
if(num.lost.ID>0){
      temp<-c(1,lost.ID,(max(new.ID)+1))
      for(j in 1:2){
      new.X[which(new.X[,3]>=temp[j]&new.X[,3]<temp[j+1]),3]<-new.X[which(new.X[,3]>=temp[j]&new.X[,3]<temp[j+1]),3]-(j-1)
      }
}

new.ind<-max(new.X[,3],na.rm=TRUE) #Computes the number of individuals in new X

#Relocate the photo
#Select a number from .5,1,1.5,...,new.ind, new.ind +.5
#where a whole number indicates that the photograph should be assigned to an existing individual and a half 
#number indicates that the photograph should be assigned to a new individual between two existing individuals 
new.individual<-sample(seq(from=.5,to=(new.ind+.5),by=.5),1)

if(floor(new.individual)==new.individual){
  new.X[remove.location,3]<-new.individual
}else{
  new.X[which(new.X[,3]>new.individual),3]=new.X[which(new.X[,3]>new.individual),3]+1
  new.X[remove.location,3]<-new.individual+.5
  }

return(list("new.X"=new.X,"n.ID.cand.X"=new.ind,"n.ID.curr.X"=prev.ind,"photo"=photo))

}