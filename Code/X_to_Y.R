X_to_Y<-function(X,t){
  #List photos for each individual
  N.obs.photographed<-max(X[,3])
  Y<-matrix(NA,nrow=N.obs.photographed,ncol=t)
  
  #Looks at each individual and each time occasion and if a photo was taken sets capture equal to 1
  for(j in 1:t){
      individual.counts<-as.data.frame(table(X[which(X[,2]==j),3]))
      Y[as.numeric(as.vector(individual.counts[[1]])),j]<-as.numeric(as.vector(individual.counts[[2]]))
    }
  
  
  #Changes the rest of W to zeros
  Y[is.na(Y)]<-0
  
  return(Y)
  
  
}