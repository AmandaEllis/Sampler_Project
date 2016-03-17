
#####################
####Packages Used####
#####################
library(abind)
library(igraph)

#####################
######Functions######
#####################
source("Sim_Data.R")                           #Source Data Simulation function
#source("Matrix_Check.R")                       #Souce Matrix Check function 
source("initial_X.R") 

#####################
####Simulate Data####
#####################
parameters<-list(t=3,                     # t denotes number of capute occasions
                 p=.5,                    # p denotes probability of capture at each occasion
                 N=20,                    # N is the total population size
                 lambda=2,                # lambda: The number of photos per individual is modeled using a poisson dist
                 alpha.match=6,           # alpha.match and beta.match are the parameters in the beta distribution for true matches
                 beta.match=2,
                 alpha.non.match=2,       # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                 beta.non.match=6)

data<-sim.data.M0(parameters,'ALL')  #Simulates Data
S<-data$S                       #Matrix of observed scores
photo.occasion.true<-data$photo.occasion      #Gives the occasion each photo was taken
W.true<-data$W                   #Gives the true capture occasion matrix
Y.true<-data$Y                   #Gives the true Y, where Y tells how many times an animal was photographed per occasion
X.true<-data$X                    #Gives the true X, where X tells the photo IDs for each individual and time point
C.true<-data$C                   #Gives the true C, where C is the binary matirx denoting which photgraphs match
t<-parameters$t                      #Gives the number of capture occasions
N.photos<-length(C.true[1,])         #Gives the number of photos that were taken

#Tuning parameters
k<-3  #Used for canidate X

#Save output to txt for debugging
#sink("C:/Users/Amand/Dropbox/A_Ellis/Code/Sampler_Photographs/Sampler_Project/output.txt", append=FALSE, split=FALSE)

################
#### MCMC  #####
################
iterations<-10000
burn.in<-0

#Define Chains
N.gibbs<-rep(NA,length=iterations+burn.in)
p.gibbs<-rep(NA,length=iterations+burn.in)
X.MH<-list(NA,length=iterations+burn.in)
lambda.MH<-rep(NA, length=iterations+burn.in)


######################
####intial values#####
######################
p.gibbs[1] <-parameters$p
lambda.MH[1]<-parameters$lambda
X.MH[[1]]<-X.true
N.gibbs[[1]]<-parameters$N

# initial_X_N<-initial.X(S,photo.occasion.true,parameters)
# X.MH[[1]]<-initial_X_N$X.observed
# #Initial value for N is the number of observed individuals in initial X array
# N.gibbs[1]<-initial_X_N$n.obs.indiv



##################
#### Sampler #####
##################

for(i in 2:(iterations+burn.in)){                             #Gibbs sampler

#Sample N  using gibbs step
#   a=n.obs.ind+alpha.N
#   b=(1+beta.N-((1-p.gibbs[i-1])^n.occasions))/((1-p.gibbs[i-1])^n.occasions)
#   lambda<-rgamma(1,a,b)
#   U<-rpois(1,lambda)
#   N.gibbs[i]<- n.obs.ind + U 
    N.gibbs[i]<-parameters$N
  
#Sample p using gibbs step
#   alpha.p.gibbs<-alpha.p+total.captures
#   beta.p.gibbs<-n.occasions*N.gibbs[i]-total.captures+beta.p
#   p.gibbs[i]<-rbeta(1,alpha.p.gibbs,beta.p.gibbs)
    p.gibbs[i]<-parameters$p

#Sample lambda using MH step
    lambda.MH[i]<-parameters$lambda

#Sample X using MH step

#Generate a Canidate X

print(canidate.X)

#In order to compute the acceptance probablity we need to compute C
# Compute C
canidate.C<-matrix(NA,nrow=N.photos,ncol=N.photos)

for(j in 1:N.photos){                     #Sets the diagonal of C equal to 1
  canidate.C[j,j]<-1
}

#Computes the number of individuals in canidate X
n.cand.ind<-length(canidate.X[,1,1])

#List photos for each individual
indiv.photos.canidate<-vector("list",n.cand.ind)          
for(j in 1:n.cand.ind){
  indiv.photos.canidate[[j]]<-as.vector(canidate.X[j,,])[!is.na(as.vector(canidate.X[j,,]))]
}

#Looks at the photos for each individual sets all possible pairs per individual to 1
for(l in 1:n.cand.ind){                           
  current.indiv<-as.vector(indiv.photos.canidate[[l]])
  if(length(current.indiv)>1){
    pairs<-combn(current.indiv,m=2)
    for(j in 1:length(pairs[1,])){
      canidate.C[pairs[1,j],pairs[2,j]]<-1
      canidate.C[pairs[2,j],pairs[1,j]]<-1
    }
  }
}

#Changes the rest of c to zeros
canidate.C[is.na(canidate.C)]<-0                           
#Matrix.Check(canidate.C)  
print(canidate.C)

#In order to compute the acceptance probablity we need to compute Y
canidate.Y<-matrix(NA,nrow=M,ncol=t)

#Looks at each individual and each time occasion and if a photo was taken sets capture equal to 1
for(j in 1:nrow(canidate.X)){
  for(l in 1:t){
    if(sum(canidate.X[j,l,]!='NA',na.rm = TRUE)!=0){canidate.Y[j,l]=sum(canidate.X[j,l,]!='NA',na.rm = TRUE)} 
  }
}

#Changes the rest of W to zeros
canidate.Y[is.na(canidate.Y)]<-0

#In order to compute the acceptance probablity we need to compute W
canidate.W<-matrix(NA,nrow=M,ncol=t)

#For each individual and each time occasion and if a photo was taken sets capture equal to 1
canidate.W[canidate.Y>0]=1

print(canidate.W)



#Changes the rest of W to zeros
canidate.W[is.na(canidate.W)]<-0

print(canidate.W)
#Delete
X.MH[[i]]<-canidate.X


}

