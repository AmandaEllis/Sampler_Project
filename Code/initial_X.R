#Create intial X array based off of observed scores
#This is done by using the estimates of alpha.match and beta.match
#Any observed score greater than the mean of the distribution of the matches
#will be considered a match for the initial value.
#In order to preserve transitivity, closure will be taken over the pairs.  

#####################
####Packages Used####
#####################
library(igraph)

initial.X<-function(S,photo.occasion.true,parameters, N.photos){

alpha<-parameters$alpha.match
beta<-parameters$beta.match

photo.individual<-rep(NA,length=N.photos)            #Vector that gives the individual for each photo ID
X<-data.frame(photo.occasion.true,photo.individual)
  
match.mean<-(alpha)/(alpha+beta) #Mean of the beta distribution for match pairs
match.var<-(alpha*beta)/((alpha+beta)^2+(alpha+beta+1))
C.observed<-S                                                                       #Set C.observed equal to S. 
C.observed[which(C.observed>=(match.mean+match.var/4))]=1  #Set values greater than the mean equal to 1
C.observed[which(C.observed<(match.mean+match.var/4))]=0   #Set values less than the mean equal to 0

#Transitive Closure over the pairs
i <- clusters(graph.adjacency(C.observed))$membership
C.observed[] <- i[row(C.observed)] == i[col(C.observed)]
#Matrix.Check(C.observed)

#Build X Array from observed C matrix.
#We only need the unique rows fom C to build the X array
#Notice that each unique row of C denotes a unique individual
unique.C.observed<-unique(C.observed)

#Number of observed individuals
n.obs.indiv<-length(unique.C.observed[,1])

#Photo IDs for each individual
obs.photo.indiv<-vector("list",n.obs.indiv)

for(i in 1:n.obs.indiv){
  obs.photo.indiv[[i]]<-which(unique.C.observed[i,]==1)
}

#Finds for each individual the occasions that individual was seen on
for(i in 1:n.obs.indiv){
  X[obs.photo.indiv[[i]],3]<-i
}


return(list('X.observed'=X,'n.obs.indiv'=n.obs.indiv))

}