#This function computes the propsal density of X
#Recall that for each candidate X we are only changing 1 photo
#We only need the portion of the FC that is affected by the photo moving
#The full conditional is given by
# [X|*] is proportional to [S^obs|X][X|Y][Y|W,lambda][W|N,p]
#The function looks at each piece seperately. 
#5/11/16
