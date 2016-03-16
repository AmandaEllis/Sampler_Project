#Takes the X array and transforms to the C matrix
#C matrix is the binary matrix which denotes the photgraph pairs
X_to_C<-function(X,N.photo,N.obs.photographed){
  
#List photos for each individual
indiv.photos<-vector("list",N.obs.photographed)           
for(i in 1:N.obs.photographed){
    indiv.photos[[i]]<-unlist(unlist(X[[i]]))
    }

C<-matrix(0,nrow=N.photo,ncol=N.photo)

for(i in 1:N.photo){                     #Sets the diagonal of C equal to 1
  C[i,i]<-1
}

for(i in 1:N.obs.photographed){                           #Looks at the photos for each individual sets all possible pairs per individual to 1
  current.indiv<-as.vector(indiv.photos[[i]])
  if(length(current.indiv)>1){
    pairs<-combn(current.indiv,m=2)
    for(j in 1:length(pairs[1,])){
      C[pairs[1,j],pairs[2,j]]<-1
      C[pairs[2,j],pairs[1,j]]<-1
    }
  }
}
 return('C'=C)
}