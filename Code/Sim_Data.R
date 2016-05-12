# function for simulating data for model M_0 with t capture occassions
# paramters is a list of parameters need to generate the data described below
# output is the desired data to output, can return W, Wp C or S
# 4/08/2016
source("Simulate_X.R")
source("X_to_C.R")
source("Simulate_S.R")
source("rtpois.R")


sim.data.M0<-function(parameters,output){
  t=parameters$t
  p=parameters$p                   
  N=parameters$N                     
  lambda=parameters$lambda                
  alpha.match=parameters$alpha.match            
  beta.match=parameters$beta.match 
  alpha.non.match=parameters$alpha.non.match       
  beta.non.match=parameters$beta.non.match 
  
  #Simulate W, where W is the observed capture history matrix
  #First simulate W with both observed and unobserved individuals. W is simulated under model M0
  W<-matrix(rbinom(n=(N*t),size=1,prob=p),nrow=N,ncol=t) 
  remove<-apply(W,1,sum)==0  #Removed unobserved individuals
  W<-W[!remove,]
  
  if(output=='W'){return(W)} #Returns observed capture history matrix
  
  #Model the additional photographs at each occasion using a poisson
  #Independent poissons are generated for each possible capture and 1 is added to each
  #Then multiplied by W, those animals animals not captured are multipled by 0 and 
  #those captured are multiplied by 1
  
  N.obs<-length(W[,1])      #Number of observed individuals
  
  Y<-matrix((rtpois(n=(N.obs*t),lambda=lambda)),nrow=N.obs,ncol=t)*W
  
  #Remove individuals that were not photographed
  Y<-Y[rowSums(Y)>0,]
   
  #Returns matrix with number of photos per ind per capture occasion
  if(output=='Y'){return(Y)} 
  
  #Simulate X, where X is the array of photos for each capture history.
  #X is simulated as a list of list
  #Simulate photo.occasion, which gives the capture occasion for each photo
  N.photo<-sum(Y) 
  N.obs.photographed<-nrow(Y)
  X<-Simulate.X(Y,N.photo,N.obs.photographed,t)


  #Returns X
  if(output=='X'){return(X)}
  
  #Computes matrix based off of X matrix
  C<-X_to_C(X,N.photo)
  
  #Returns C
  if(output=='c'){return(c)}
  
  #Simulate the score values
  S<-Sim.S(C,alpha.match,beta.match,alpha.non.match,beta.non.match,N.photo)

  if(output=='S'){return(S)}
  if(output=='ALL'){return(list('S'=S,'photo.occasion'=X[,1:2],'W'=W,'Y'=Y,'X'=X,'C'=C))}
}