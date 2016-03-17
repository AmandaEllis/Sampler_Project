#Used to Simulate the X array from a give Y matrix
#Simulate X, where X is the array of photos for each capture history.
#Outputs both the X array in dataframe form and the capture occasion for each photo.

Simulate.X<-function(Y,N.photo,N.obs.photographed){

photo.id<-c(1:N.photo)                              #List of photo IDs
photo.occasion<-rep(NA,length=N.photo)              #Vector that gives the capture occasion for each photo ID
photo.individual<-rep(NA,length=N.photo)            #Vector that gives the capture occasion for each photo ID
X<-data.frame(photo.id,photo.occasion,photo.individual)

sample.photo.id<-photo.id                                 #Vector of Photo IDs used in the construction of X

for(i in 1:N.obs.photographed){
  for(j in 1:t){
    if(Y[i,j]>0){
      #Sample function does not work as expected when =1 this is a work around
      if(length(sample.photo.id)>1){                    
        current.sample<-sample(sample.photo.id,size=Y[i,j])
      }else{current.sample<-sample.photo.id}  
            X[current.sample,3]<-i
      X[current.sample,2]<-j
      sample.photo.id<-sample.photo.id[-which(sample.photo.id%in%current.sample)]
    }
  }
}

return(X)
}
