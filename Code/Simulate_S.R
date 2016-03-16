#Takes the C matrix and simulates the matrix of Scores

Sim.S<-function(C,alpha.match,beta.match,alpha.non.match,beta.non.match,N.photo){
  #First we change the upper diagonal of S
  S<-matrix(NA,nrow=N.photo,ncol=N.photo)
  for(i in 1:(N.photo-1)){
    for(j in (i+1):N.photo){
      if(C[i,j]==1){S[i,j]=rbeta(n=1,shape1=alpha.match,shape2=beta.match) 
      }else{
        S[i,j]=rbeta(n=1,shape1=alpha.non.match,shape2=beta.non.match)
      }
    }
  }
  #Set the diagonal of S equal to 1
  for(i in 1:N.photo){                     
    S[i,i]<-1
  }
  #Enforce the symmetry
  for(i in 1:(N.photo-1)){
    for(j in (i+1):N.photo){
      S[j,i]<-S[i,j]
    }
  }
  
  return('S'=S)
}