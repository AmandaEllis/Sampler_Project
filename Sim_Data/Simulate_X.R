#Used to Simulate the X array from a give Y matrix
#Simulate X, where X is the array of photos for each capture history.
#Outputs both the X array in list form and the capture occasion for each photo.

Simulate.X<-function(Y,N.photo,N.obs.photographed){
photo.id<-c(1:N.photo)                          #List of photo IDs
max.photo<-max(Y)                               #Maximum number of photos for an individual in a capture occasion

X<-list()                         #X is a list of list of list, by individual/capture occasion/photo ID

photo.occasion<-matrix(NA,nrow=N.photo,ncol=2)            #Matrix that gives the capture occasion for each photo ID
photo.occasion[,1]<-photo.id

sample.photo.id<-photo.id                                 #Vector of Photo IDs used in the construction of X

for(i in 1:N.obs.photographed){
  X[[i]]<-list()
  for(j in 1:t){
    if(Y[i,j]>0){
        #Sample function does not work as expected when =1 this is a work around
        if(length(sample.photo.id)>1){                    
          current.sample<-sample(sample.photo.id,size=Y[i,j])
        }else{current.sample<-sample.photo.id}  
      
        X[[i]][[j]]<-as.list(current.sample)
        photo.occasion[current.sample,2]<-j
        sample.photo.id<-sample.photo.id[-which(sample.photo.id%in%current.sample)]
    }
  }
}
return(list('X'=X,'photo.occasion'=photo.occasion))
}
