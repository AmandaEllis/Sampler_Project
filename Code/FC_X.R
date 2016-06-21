#This function computes the density of the full conditional of  X
#We only need the portion of the FC that is affected by the photos that move
#The full conditional is given by
# [X|*] is proportional to [S^obs|X][X|Y][Y|W,lambda][W|N,p]
#The function looks at each piece seperately. 
#5/11/16


#####################
######Functions######
#####################
source("dtpois.R")      #Function that computes the desity of zero truncated poisson
source("d_S_given_X.R") #Function that computes the density of S given X
source("d_X_given_Y.R") #Function that computes the density of X given Y
source("d_W_given_N_p.R") #Function that computes the density of X given Y


FC.X<-function(FC.input){
  
  candidate.X=FC.input$candidate.X                 #Candidate X
  current.X=FC.input$current.X                     #Current X
  N.photos=FC.input$N.photos                       #Total number of photos
  t=FC.input$t                                     #Number of capture occasions
  photos=FC.input$photos                           #Photos that were agitated
  S=FC.input$S                                     #Matrix of pairwise scores -observed data
  beta.match=FC.input$beta.match                   # alpha.match and beta.match are the parameters in the beta distribution for true matches
  alpha.match=FC.input$alpha.match
  alpha.non.match=FC.input$alpha.non.match         # alpha.non.match and beta.non.match are the parameters in the beta distribution for true non-matches
  beta.non.match =FC.input$beta.non.match
  cap.occasion=FC.input$cap.occasion               #Capture occasion that photo was selected from
  p=FC.input$p
  N=FC.input$N
  lambda=FC.input$lambda

  ###############
  ## [S^obs|X] ##
  ###############
  d.S.X.output<-d.S.given.X(current.X,candidate.X,S,N.photos,photos,alpha.match,beta.match,alpha.non.match,beta.non.match)
  d.S.given.current.X<-d.S.X.output$d.S.given.current.X
  d.S.given.candidate.X<-d.S.X.output$d.S.given.candidate.X
#   d.S.given.current.X<-0
#   d.S.given.candidate.X<-0
  
  ###########
  ## [X|Y] ##
  ###########
  d.X.Y.output<-d.X.given.Y(current.X,candidate.X,t,photos)
  d.X.given.current.Y<-d.X.Y.output$d.X.given.current.Y
  d.X.given.candidate.Y<-d.X.Y.output$d.X.given.candidate.Y
  
  current.Y.ind<-d.X.Y.output$current.Y.ind           #Used in [Y|W,lambda] 
  candidate.Y.ind<-d.X.Y.output$candidate.Y.ind
  
  current.Y<-d.X.Y.output$current.Y                 #Used in [W|N,p]
  candidate.Y<-d.X.Y.output$candidate.Y
  
  cap.occasions<-d.X.Y.output$cap.occasions                      #Used in [W|N,p]         
  
  ##################
  ## [Y|W,lambda] ##
  ##################
  
  # [Y|W,lambda] We consider each element of Y to be an independent draw from a truncated Poisson
  # Again we need only consider the individuals that are affected by the photograph that is moved. 
  # We know that since W is deterministic of Y, that each Y >0 will have a corresponding W of 1
  # Also each Y equal to 0 has a corresponding W of 0.  Using independence we need only consider 
  # The candidate and current Y above.  
  
  if(d.X.Y.output$same==FALSE){
  d.Y.given.current.W<-sum(dtpois(current.Y.ind,lambda,return.log='TRUE'))
  d.Y.given.candidate.W<-sum(dtpois(candidate.Y.ind,lambda,return.log='TRUE'))
  }else{
    #If the Y for the current and the candidate are the same then the densities will cancel.
    d.Y.given.current.W<-0
    d.Y.given.candidate.W<-0
  }
  
  ##############
  ## [W|N,p] ##
  #############
  
  #We use the candidate Y and current Y from the output above
  #The candidiate Y and current Y only contain the columns that differ
  
  if(d.X.Y.output$same==FALSE){
  d.W.N.p.output<-d.W.given.N.p(current.Y,candidate.Y,N,p,cap.occasions)
  d.W.current.given.Np<-d.W.N.p.output$d.W.current.given.Np
  d.W.candidate.given.Np<-d.W.N.p.output$d.W.candidate.given.Np
  }else{
    #If the Y for the current and the candidate are the same then the W are the same and the densities will cancel.
    d.W.current.given.Np<-0
    d.W.candidate.given.Np<-0
  }
  
  ######################
  ## Full Conditional ##
  ######################
  
  #Now we calculate the full conditional for the current and the candidate
  current.FC<-d.S.given.current.X+d.X.given.current.Y+d.Y.given.current.W+d.W.current.given.Np
  candidate.FC<-d.S.given.candidate.X+d.X.given.candidate.Y+d.Y.given.candidate.W+d.W.candidate.given.Np

#   current.FC<-d.X.given.current.Y+d.Y.given.current.W+d.W.current.given.Np
#   candidate.FC<-d.X.given.candidate.Y+d.Y.given.candidate.W+d.W.candidate.given.Np
  
  return(list("current.FC"=current.FC,"candidate.FC"=candidate.FC))
    
  }
 
  
    