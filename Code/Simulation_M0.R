
#####################
######Functions######
#####################

source("Sampler_M0.R")                           #Source Data Simulation function


################################
####TParameters for Sim Data####
################################
t=3
parameters<-list(t=t,                     # t denotes number of capute occasions
                 p=rep(.25,t),             # p denotes probability of capture at each occasion
                 N=100,                    # N is the total population size
                 lambda=1,                # lambda: The number of photos per individual is modeled using a poisson dist
                 # lambda is the mean of a Poisson before truncation the mean of the truncated poisson is E(X) = T/(1 - exp(-T))
                 alpha.match=6,           # alpha.match and beta.match are the parameters in the beta distribution for true matches
                 beta.match=2,
                 alpha.non.match=2,       # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
                 beta.non.match=6)

#########################
####Tuning Parameters####
#########################
k1<-1                                  #Used for canidate X, tells the number of photos perturbed in an iteration of the chain
k2<-5
q<-.5


################
#### Priors ####
################

priors<-list(alpha.p=.5,  #p has a beta prior
             beta.p =.5,
             
             alpha.N=1,    #N has negative binomial prior
             beta.N =0)

################
#### MCMC  #####
################
iterations<-20000
burn.in<-1000

## Read simulation number from the command line
simnum <- as.integer(commandArgs(trailingOnly=T)[1])

output<-sampler.M0(parameters,k1,k2,priors,iterations,burn.in,q)

outfile <- file.path("Output",paste0("output_",simnum,".Rdata"))
save(output,file=outfile)
