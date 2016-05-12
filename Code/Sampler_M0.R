#####################
####Packages Used####
#####################


#####################
######Functions######
#####################

source("Sim_Data.R")                           #Source Data Simulation function
#source("Matrix_Check.R")                      #Source Matrix Check function 
#source("initial_X.R")                         #Source function to create intial X value (Needs to be changed to data frame)
source("new_X.R")                              #Source function to create new X from current X  
source("X_to_C.R")                             #Source function to calcuate C from given X
source("X_to_Y.R")                             #Source function to calcuate Y from given X
source("FC_X.R")                               #Source function to calcuate Full Conditional from given X
source("S_Stat_W.R")                           #Source function to calcuate sufficient statistics from a given W

#####################
####Simulate Data####
#####################
parameters<-list(t=3,                     # t denotes number of capute occasions
                 p=.8,                    # p denotes probability of capture at each occasion
                 N=20,                   # N is the total population size
                 lambda=1,                # lambda: The number of photos per individual is modeled using a poisson dist
                                          # lambda is the mean of a Poisson before truncation the mean of the truncated poisson is E(X) = T/(1 - exp(-T))
                 alpha.match=6,           # alpha.match and beta.match are the parameters in the beta distribution for true matches
                 beta.match=2,
                 alpha.non.match=2,       # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                 beta.non.match=6)

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

#########################
####Tuning Parameters####
#########################
k<-8                                    #Used for canidate X, tells the number of photos perturbed in an iteration of the chain

################
#### Priors ####
################

alpha.p<-.5   #p has a beta prior
beta.p <-.5

alpha.N<-0    #N has negative binomial prior
beta.N <-0

################
#### MCMC  #####
################
iterations<-10000
burn.in<-1000

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
# Start the clock!
ptm <- proc.time()

for(i in 2:(iterations+burn.in)){                             #Gibbs sampler with Metropolis Hasting Steps
  options(warn=2)
  #Get the sufficient Statistics from W to sample N and p
    stat.output<-S.Stat.W(X.MH[[i-1]],t)

  ###############################
  ## Sample N  using gibbs step##
  ###############################
  
    a=stat.output$n.obs.ind+alpha.N
    b=(1+beta.N-((1-p.gibbs[i-1])^t))/((1-p.gibbs[i-1])^t)
    l<-rgamma(1,a,b)
    U<-rpois(1,l)
    N.gibbs[i]<- stat.output$n.obs.ind + U 
    #N.gibbs[i]<-parameters$N
  
  ###############################
  ## Sample p  using gibbs step##
  ###############################
  #   alpha.p.gibbs<-alpha.p+stat.output$total.captures
  #   beta.p.gibbs<-t*N.gibbs[i]-stat.output$total.captures+beta.p
  #   p.gibbs[i]<-rbeta(1,alpha.p.gibbs,beta.p.gibbs)
  p.gibbs[i]<-parameters$p

  ####################################
  ## Sample lambda  using gibbs step##
  ####################################
  lambda.MH[i]<-parameters$lambda

  ###########################
  ##Sample X using MH step##
  ##########################

  #Generate a Canidate X
  #Candidate X is generated by moving one photo at a time
  #After the removal of a photo the move is accepted or rejected using a MH acceptance probability
  #This is repeated k times during an iteration of the chain

  current.X<-X.MH[[i-1]]
  
  for(j in 1:k){
    #randomly select a capture occasion
    cap.occasion<-sample(1:t,1)
    
    #Outputs a candidate X and the ID of the photo that was removed
    #Also outputs the number of individuals in the candidate and current X
    output<-new.X.fun(X.MH[[i-1]],cap.occasion)
    candidate.X<-output$new.X
    photo<-output$photo

    #Next we compute the acceptance probability 
    #This is completed in 2 steps, we compute the densities of the proposal distributions and the FC
    
    #The proposal density of X conditional on another X
    #The density is ... g(X.candidate|X.Current)=(1/T)(1/Np_t)(1/(n*+1))
    #T is the number of capture occasions
    #Np_t is the number of photos taken on the t occasion
    #n* is the number of available individuals in the given X 
    #Further notice that T and Np_t are the same for both the candidate X and the current X
    #When computing the dentsity we need only worry about 1/(n*+1)
    #We will work with the log density
    #Correction the densities are the same
    
    log.X.current.proposal.density<-0
    log.X.candidate.proposal.density<-0

    
    #The full conditional of X given data and unknowns
    FC.input<-list(candidate.X=candidate.X,                 #Candidate X
                   current.X=current.X,                     #Current X
                   N.photos=N.photos,                       #Total number of photos
                   t=t,                                     #Number of capture occasions
                   photo=photo,                             #Photo that was agitated
                   S=S,                                     #Matrix of pairwise scores -observed data
                   beta.match=beta.match,                   # alpha.match and beta.match are the parameters in the beta distribution for true matches
                   alpha.match=alpha.match,
                   alpha.non.match=alpha.non.match,         # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                   beta.non.match =beta.non.match,
                   cap.occasion=cap.occasion,               #Capture occasion that photo was selected from
                   p=p.gibbs[i-1],
                   N=N.gibbs[i-1],
                   lambda=lambda.MH[i-1])
    
    #Calculate the full conditional for te candidate and the current X, only the portions that differ are outputed.  
    #Computes log density
    FC.output<-FC.X(FC.input)
    
    ratio<-exp((FC.output$candidate.FC-log.X.candidate.proposal.density)+(log.X.current.proposal.density-FC.output$current.FC))
    
    acceptance.prob<-min(1,ratio)
    
    u=runif(1)
    if (u<acceptance.prob){
      print(1)
      current.X<-candidate.X
    }else print(0)
  }

  X.MH[[i]]<-current.X
  #X.MH[[i]]<-X.true

}

# Stop the clock
proc.time() - ptm

#hist(N.gibbs, freq=FALSE, xlab="N", col="gray", border="white", main="")
(posterior.mean.N<-mean(as.vector(N.gibbs),na.rm=TRUE))
plot(as.vector(N.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.N<-quantile(N.gibbs,c(lb,ub),names=FALSE))

(posterior.mean.p<-mean(as.vector(p.gibbs),na.rm=TRUE))
plot(as.vector(p.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.p<-quantile(p.gibbs,c(lb,ub),names=FALSE))

