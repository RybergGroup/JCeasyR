# For the distances A-B, A-C, A-D, B-C, B-D, C-D
D<-matrix(c(0.3, 0.4, 0.4, 0.3, 0.3, 0.2),ncol=1)

#On the tree ((A,B),(C,D)) [unrooted] the branches between taxa can be represented by
X<-t(matrix(c(1,1,0,0,0, 1,0,1,0,1, 1,0,0,1,1, 0,1,1,0,1, 0,1,0,1,1, 0,0,1,1,0), ncol=6))

#The branches of the tree are
solve((t(X)%*%X)) %*% t(X) %*% D

#Can you fit the branch lengths to the tree?

#And the ME score
sum(solve((t(X)%*%X)) %*% t(X) %*% D)

#Try calculating the ME score for the two other topologies ((A,C),(B,D)) and ((A,D),(B,C))
