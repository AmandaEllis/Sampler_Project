#Create intial X array based off of observed scores
#This is done by using the estimates of alpha.match and beta.match
#Any observed score greater than the mean of the distribution of the matches
#will be considered a match for the initial value.
#In order to preserve transitivity, closure will be taken over the pairs.  

#####################
####Packages Used####
#####################
library(abind)
library(igraph)

initial.X<-function(S,photo.occasion.true,parameters){
  
match.mean<-(parameters$alpha.match)/(parameters$alpha.match+parameters$beta.match)
C.observed<-S
C.observed[which(C.observed>=match.mean)]=1  #Set values greater than the mean equal to 1
C.observed[which(C.observed<match.mean)]=0   #Set values less than the mean equal to 0

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
obs.occasions<-vector("list",n.obs.indiv)

for(i in 1:n.obs.indiv){
  for(j in 1:length(obs.photo.indiv[[i]]))
  obs.occasions[[i]][j]<-photo.occasion.true[obs.photo.indiv[[i]][j],2]
}

#Finds the number of photos for each individual per occasion
n.photos.occasion<-vector("list",n.obs.indiv)
for(i in 1:n.obs.indiv){
  n.photos.occasion[[i]]<-as.data.frame(table(obs.occasions[[i]]))[,2]
}

#Finds the maximum number of photos per individual per occasion
max.photo<-max(sapply(n.photos.occasion, max)) 

#Define observed X array using the photos for each individual and occasion photos were taken
X.observed<-list()

for(i in 1:n.obs.indiv){
  X.observed[[i]]<-list()
  for(j in 1:t){
            temp<-obs.photo.indiv[[i]][which(photo.occasion.true[obs.photo.indiv[[i]],2]==j)]
            X.observed[[i]][[j]]<-as.list(temp)
  }
}

return(list('X.observed'=X.observed,'n.obs.indiv'=n.obs.indiv))

}