#[S^obs|X], since there is a 1-to-1 correspondance between X an C we work with [S^obs|C]
#When the ith photo is moved we only need the rows of the current C and candidate C that are
#affected by the move.

d.S.given.X<-function(current.X,candidate.X,S,N.photos,photo){

  #In order to compute the acceptance probablity we need to compute C
  #Compute C for the candidate and current X
  current.C<-X_to_C(current.X,N.photos)
  candidate.C<-X_to_C(candidate.X,N.photos)

  rows.current<-which(current.C[,photo]==1)     #Locates the rows in the current X that are affected by the removed photo
  rows.candidate<-which(candidate.C[,photo]==1) #Locates the rows in the candidate X that are affected by the removed photo
  comb.rows<-sort(unique(c(rows.candidate,rows.current))) #These rows will be used in Full conditional

  n.rows<-length(comb.rows)  #number of rows that are affected by the removed photo

  sub.current.C<-current.C[comb.rows,comb.rows]     #Creates a sub current C which only contains elelements that differ from the candidate C
  sub.candidate.C<-candidate.C[comb.rows,comb.rows] #Creates a sub candidate C which only contains elelements that differ from the current C
  sub.S<-S[comb.rows,comb.rows]                     #Extracts the elements of S that correspond to the sub C

  
  #When generating a new X we allowed for the possibility that there was no change in X
  # We need to account for when there is no difference in C
  if(length(sub.candidate.C)<2){
    return(list("d.S.given.candidate.X"=0,"d.S.given.current.X"=0))
  }
  
  #Next consider when the candidate C and current C differ
  #Notice that the sub Cs are still symmetric we need only compute the upper triangular portion
  
  #Loop over the upper triagular portion of the current C
  #Densities are calculated using log
  
  #Initialize the value of the density  
  d.S.given.current.X<-0
  temp<-0
  for(i in 1:(nrow(sub.current.C)-1)){
    for(j in (i+1):nrow(sub.current.C)){
      temp<-sub.current.C[i,j]*dbeta(sub.S[i,j],alpha.match,beta.match,log=TRUE)+
        (1-sub.current.C[i,j])*dbeta(sub.S[i,j],alpha.non.match,beta.non.match,log=TRUE)
      d.S.given.current.X<-d.S.given.current.X+temp
    }
  }
  
  #mMltiply by 2 since only the upper triangle was calculated. 
  #Notice that the diagaonal is the same for both the current and the candidiate so we do not need to caculate
  d.S.given.current.X<-d.S.given.current.X*2
    
  #Loop over the upper triagular portion of the candidiate C
  #Densities are calculated using log
  d.S.given.candidate.X<-0
  temp<-0
  for(i in 1:(nrow(sub.candidate.C)-1)){
      for(j in (i+1):nrow(sub.candidate.C)){
        temp<-sub.candidate.C[i,j]*dbeta(sub.S[i,j],alpha.match, beta.match,log=TRUE)+
          (1-sub.candidate.C[i,j])*dbeta(sub.S[i,j],alpha.non.match,beta.non.match,log=TRUE)
        d.S.given.candidate.X<-d.S.given.candidate.X+temp
       }
  }
  
  #Multiply by 2 since only the upper triangle was calculated. 
  #Notice that the diagaonal is the same for both the current and the candidiate so we do not need to caculate
  d.S.given.candidate.X<-d.S.given.candidate.X*2
  
  return(list("d.S.given.candidate.X"=d.S.given.candidate.X,"d.S.given.current.X"=d.S.given.current.X))


}
