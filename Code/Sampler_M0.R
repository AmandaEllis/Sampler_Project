#####################
####Packages Used####
#####################


#####################
######Functions######
#####################

source("Sim_Data.R")                           #Source Data Simulation function
#source("Matrix_Check.R")                      #Source Matrix Check function 
source("initial_X.R")                         #Source function to create intial X value (Needs to be changed to data frame)
source("new_X_create_test.R")                       #Source function to create new X from current X
source("new_X_delete_test.R")                       #Source function to delete X from current X 
source("new_X_reallocate.R")                   #Source function to reallocate photos from current X
source("prop_den_new_test.R")                       #Source function to calcuate proposal density
source("prop_den_delete_test.R")                    #Source function to calcuate proposal density
source("X_to_C.R")                             #Source function to calcuate C from given X
source("X_to_Y.R")                             #Source function to calcuate Y from given X
source("FC_X.R")                               #Source function to calcuate Full Conditional from given X
source("S_Stat_W.R")                           #Source function to calcuate sufficient statistics from a given W

#####################
####Simulate Data####
#####################
sampler.M0<-function(parameters,k1,k2,priors,iterations,burn.in,q){


data<-sim.data.M0(parameters,'ALL')       #Simulates Data

#Extract Data
S<-data$S                                 #Matrix of observed scores
photo.occasion.true<-data$photo.occasion  #Gives the occasion each photo was taken
W.true<-data$W                            #Gives the true capture occasion matrix
Y.true<-data$Y                            #Gives the true Y, where Y tells how many times an animal was photographed per occasion
X.true<-data$X                            #Gives the true X, where X tells the photo IDs for each individual and time point
C.true<-data$C                            #Gives the true C, where C is the binary matirx denoting which photgraphs match
t<-parameters$t                           #Gives the number of capture occasions
N.photos<-length(C.true[1,])              #Gives the number of photos that were taken

alpha.match=parameters$alpha.match        # alpha.match and beta.match are the parameters in the beta distribution for true matches
beta.match=parameters$beta.match 
alpha.non.match=parameters$alpha.non.match   # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
beta.non.match=parameters$ beta.non.match


################
#### Priors ####
################

alpha.p<-priors$alpha.p   #p has a beta prior
beta.p <-priors$beta.p 

alpha.N<-priors$alpha.N    #N has negative binomial prior
beta.N <-priors$beta.N 

################
#### MCMC  #####
################

#Define Chains
N.gibbs<-rep(NA,length=iterations+burn.in)
p.gibbs<-matrix(NA,nrow=iterations+burn.in,ncol=t)
#X.MH<-list(NA,length=iterations+burn.in)
lambda.MH<-rep(NA, length=iterations+burn.in)


######################
####intial values#####
######################

initial<-list(p.gibbs =parameters$p,
              lambda.MH=parameters$lambda)

p.gibbs[1,] <-initial$p.gibbs
lambda.MH[1]<-initial$lambda.MH

initial.X<-initial.X(S,photo.occasion.true,parameters)
current.X<-initial.X$X.observed
#Initial value for N is the number of observed individuals in initial X array
N.gibbs[1]<-parameters$N
#current.X<-X.true



##################
#### Sampler #####
##################
# Start the clock!
#ptm <- proc.time()
#options(warn=2)

for(i in 2:(iterations+burn.in)){                             #Gibbs sampler with Metropolis Hasting Steps

  #Get the sufficient Statistics from W to sample N and p
  #stat.output<-S.Stat.W(X.MH[[i-1]],t)
  stat.output<-S.Stat.W(current.X,t)
  
  ###############################
  ## Sample N  using gibbs step##
  ###############################
    
  #See link and Barker pg 205
  a<-stat.output$n.obs.ind+alpha.N
  
  pi.0<-1
  for(j in 1:t){
  pi.0<-(1-p.gibbs[i-1,j])*pi.0
  }
  
  b<-(1+beta.N-pi.0)/pi.0
  l<-rgamma(1,a,b)
  U<-rpois(1,l)
  N.gibbs[i]<- stat.output$n.obs.ind + U 
  #N.gibbs[i]<-parameters$N
  
  ###############################
  ## Sample p  using gibbs step##
  ###############################
  captures<-stat.output$captures
  alpha.p.gibbs<-alpha.p+captures
  
  beta.p.gibbs<-N.gibbs[i]-captures+beta.p
  
  for(j in 1:t){
  p.gibbs[i,j]<-rbeta(1,alpha.p.gibbs[j],beta.p.gibbs[j])
  }
  #p.gibbs[i,]<-parameters$p

  ####################################
  ## Sample lambda  using gibbs step##
  ####################################
#   candidate.lambda=runif(1,min=0, max=10) 
#   current.Y<-as.vector(X_to_Y(current.X,t))
#   #remove zeros
#   current.Y<-current.Y[which(current.Y>0)]
#   FC.candidiate.lambda<-sum(log(candidate.lambda^(current.Y-.5)/(exp(candidate.lambda)-1)))
#   FC.current.lambda<-sum(log(lambda.MH[i-1]^(current.Y-.5)/(exp(lambda.MH[i-1])-1)))
#   
#   #Next we compute the acceptance probability 
#   ratio<-exp(FC.candidiate.lambda-FC.current.lambda)
#   acceptance.prob=min(ratio,1)
#   u.lambda<-runif(1)
#   
#   if (u.lambda<acceptance.prob){
#     lambda.MH[i]<-candidate.lambda
#   }else{
#     lambda.MH[i]<-lambda.MH[i-1]
#   }
  lambda.MH[i]<-parameters$lambda

  ###########################
  ##Sample X using MH step##
  ##########################
  
  
  # Generate a Canidate X and calcualte the proposal densities
  # Candidate X is generated in 2 steps
  # Step 1: Create or delete an individual
  # Step 2 move photos
  # After each step the change move is accepted or rejected using a MH acceptance probability
  
  
  ########
  #STEP 1#
  ########
  for(j in 1:k1){
  # First we choose which action to take delete or create an individual
  action<-runif(1)
  
  if(action <=q){  #We are adding an individual
    output<-new.X.create(current.X,t,lambda.MH[i],p.gibbs[i,])          #Creates a new X by adding a new individual
    candidate.X<-output$new.X
    photos=output$photos                                      #Photos are that are affected by change
    new.ID=output$new.ID
    #delete is TRUE when indidivual is deleted, FALSE when not
    delete=output$delete
    
    if(length(unique(candidate.X[,3]))!=max(candidate.X[,3])){
      print("step 1 candidate")
      print(action)
    }
    
    if(delete=='FALSE'){
    #Current X given Candidiate X proposal density 
    log.X.current.proposal.density.step1<-prop.den.delete.fun(X=current.X,given.X=candidate.X,q=q,t=t,deleted.Id=new.ID)$density
    #Candidate X given Current X proposal density 
    log.X.candidate.proposal.density.step1<-prop.den.new.fun(X=candidate.X,given.X=current.X,q=q,t=t,new.ID=new.ID,lambda=lambda.MH[i],p=p.gibbs[i,])$density
    }else{
      log.X.current.proposal.density.step1<-0
      log.X.candidate.proposal.density.step1<-0 
    }
  
    }else{
    output<-new.X.delete(current.X,t)
    candidate.X<-output$new.X
    photos=output$photos
    deleted.ID=output$deleted.Id
    #delete will always output false, this delete stands for a deleted individual when should have added
    delete=output$delete
    #Current X given Candidiate X proposal density 
    log.X.current.proposal.density.step1<-prop.den.new.fun(X=current.X,given.X=candidate.X,q=q,t=t,new.ID=deleted.ID,lambda=lambda.MH[i],p=p.gibbs[i,])$density
    #Candidate X given Current X proposal density 
    log.X.candidate.proposal.density.step1<-prop.den.delete.fun(X=candidate.X,given.X=current.X,q=q,t=t,deleted.Id=deleted.ID)$density
    
    }
  

    #The full conditional of X given data and unknowns
    FC.input<-list(candidate.X=candidate.X,                 #Candidate X
                   current.X=current.X,                     #Current X
                   N.photos=N.photos,                       #Total number of photos
                   t=t,                                     #Number of capture occasions
                   photos=photos,                             #Photo that was agitated
                   S=S,                                     #Matrix of pairwise scores -observed data
                   beta.match=beta.match,                   # alpha.match and beta.match are the parameters in the beta distribution for true matches
                   alpha.match=alpha.match,
                   alpha.non.match=alpha.non.match,         # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                   beta.non.match =beta.non.match,               #Capture occasion that photo was selected from
                   p=p.gibbs[i,],
                   N=N.gibbs[i],
                   lambda=lambda.MH[i])
    
    #Calculate the full conditional for the candidate and the current X, only the portions that differ are outputed.  
    #Computes log density
    if(delete=='FALSE'){
    FC.output.step1<-FC.X(FC.input)
    #Next we compute the acceptance probability 
    ratio<-exp((FC.output.step1$candidate.FC-log.X.candidate.proposal.density.step1)+(log.X.current.proposal.density.step1-FC.output.step1$current.FC))

    acceptance.prob.step1<-min(1,ratio)
    }else{acceptance.prob.step1=0}
    
    u1=runif(1)
    
    if (u1<acceptance.prob.step1){current.X<-candidate.X}
      
    }
    

    ########
    #STEP 2#
    ########  
    
    for(j in 1:k2){
    #Reallocate k photos
    output<-new.X.reallocate(current.X,t)          #Creates a new X by reallocating photos
    candidate.X<-output$new.X
    photos=output$photos                                      #Photos are that are affected by change
    delete=output$delete
    
    #The proposal density is the same
    log.X.current.proposal.density.step2<-0
    log.X.candidate.proposal.density.step2<-0 
    
    
    #The full conditional of X given data and unknowns
    FC.input.step2<-list(candidate.X=candidate.X,                 #Candidate X
                         current.X=current.X,                     #Current X
                         N.photos=N.photos,                       #Total number of photos
                         t=t,                                     #Number of capture occasions
                         photos=photos,                             #Photo that was agitated
                         S=S,                                     #Matrix of pairwise scores -observed data
                         beta.match=beta.match,                   # alpha.match and beta.match are the parameters in the beta distribution for true matches
                         alpha.match=alpha.match,
                         alpha.non.match=alpha.non.match,         # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                         beta.non.match =beta.non.match,               #Capture occasion that photo was selected from
                         p=p.gibbs[i,],
                         N=N.gibbs[i],
                         lambda=lambda.MH[i])
    
    #Calculate the full conditional for te candidate and the current X, only the portions that differ are outputed.  
    #Computes log density
    FC.output.step2<-FC.X(FC.input.step2)
    
    #Next we compute the acceptance probability 
    ratio<-exp((FC.output.step2$candidate.FC-log.X.candidate.proposal.density.step2)+(log.X.current.proposal.density.step2-FC.output.step2$current.FC))
    
    if(delete=='FALSE'){
      acceptance.prob.step2<-min(1,ratio)
    }else{acceptance.prob.step2=0}
    
    u2=runif(1)
    if (u2<acceptance.prob.step2){current.X<-candidate.X}
    
    } 


}
# # Stop the clock
# #proc.time() - ptm

return(list('N.gibbs'=N.gibbs,'p.gibbs'=p.gibbs,'lambda.MH'=lambda.MH,"current.X"=current.X))
}



