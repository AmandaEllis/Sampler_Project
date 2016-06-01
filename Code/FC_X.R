#This function computes the density of the full conditional of  X
#Recall that for each candidate X we are only changing 1 photo
#We only need the portion of the FC that is affected by the photo moving
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
  photo=FC.input$photo                             #Photo that was agitated
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
  d.S.X.output<-d.S.given.X(current.X,candidate.X,S,N.photos,photo,alpha.match,beta.match,alpha.non.match,beta.non.match)
  d.S.given.current.X<-d.S.X.output$d.S.given.current.X
  d.S.given.candidate.X<-d.S.X.output$d.S.given.candidate.X
  
  ###########
  ## [X|Y] ##
  ###########
  d.X.Y.output<-d.X.given.Y(current.X,candidate.X,t,cap.occasion)
  d.X.given.current.Y<-d.X.Y.output$d.X.given.current.Y
  d.X.given.candidate.Y<-d.X.Y.output$d.X.given.candidate.Y
  
  current.Y.ind<-d.X.Y.output$current.Y.ind           #Used in [Y|W,lambda] 
  candidate.Y.ind<-d.X.Y.output$candidate.Y.ind
  
  current.Y<-d.X.Y.output$current.Y                 #Used in [W|N,p]
  candidate.Y<-d.X.Y.output$candidate.Y
  
  ##################
  ## [Y|W,lambda] ##
  ##################
  
  # [Y|W,lambda] We consider each element of Y to be an independent draw from a truncated Poisson
  # Again we need only consider the individuals that are affected by the photograph that is moved. 
  # We know that since W is deterministic of Y, that each Y >0 will have a corresponding W of 1
  # Also each Y equal to 0 has a corresponding W of 0.  Using independence we need only consider 
  # The candidate and current Y above.  
  
  d.Y.given.current.W<-sum(dtpois(current.Y.ind,lambda,return.log='TRUE'))
  d.Y.given.candidate.W<-sum(dtpois(candidate.Y.ind,lambda,return.log='TRUE'))

  ##############
  ## [W|N,p] ##
  #############
  
  d.W.N.p.output<-d.W.given.N.p(current.Y,candidate.Y,N,p)
  
  d.W.current.given.Np<-d.W.N.p.output$d.W.current.given.Np
  d.W.candidate.given.Np<-d.W.N.p.output$d.W.candidate.given.Np
  
  ######################
  ## Full Conditional ##
  ######################
  
  #Now we calculate the full conditional for the current and the candidate
  current.FC<-d.S.given.current.X+d.X.given.current.Y+d.Y.given.current.W+d.W.current.given.Np
  candidate.FC<-d.S.given.candidate.X+d.X.given.candidate.Y+d.Y.given.candidate.W+d.W.candidate.given.Np

  return(list("current.FC"=current.FC,"candidate.FC"=candidate.FC))
    
  }
 
  
    