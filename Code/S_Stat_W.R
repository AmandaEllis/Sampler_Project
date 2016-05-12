S.Stat.W<-function(X,t){
#Convert Current X to to Current W
      Y<-X_to_Y(X,t)
      W<-Y
      W[Y>0]=1
      
      n.obs.ind<-length(W[,1])              #Number of observed individuals
      
      captures<-rep(NA,length=t)  #Calculate the number of captures per capture occasion
      for(i in 1:t){
        captures[i]<-sum(W[,i])
      }
      
      total.captures<-sum(captures)
      
      first<-rep(NA,length=n.obs.ind) 
      W.no.first<-W
      for(i in 1:n.obs.ind){
        first[i]<-min(which(W[i,]==1))        #Calculate when individual first observed
        W.no.first[i,first[i]]<-0             #Sets the first occasion to 0
      }
      
      recaptures<-rep(NA,length=t)  #Calculate the number of recaptures per capture occasion
      for(i in 1:t){
        recaptures[i]<-sum(W.no.first[,i])
      }
      
      return(list("n.obs.ind"=n.obs.ind,"total.captures"=total.captures))
  
}