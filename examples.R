#For the tree ((A,B),(C,D)) let the branch to A be 1 to B 2 to (A,B) 3 to C 4 to D 5 and to (C,D) 6
branches<-c(0.1,0.1,0.1,0.1,0.1,0.1)

#Let the bases be
T <- c(1,0,0,0)
C <- c(0,1,0,0)
A <-c(0,0,1,0)
G <-c(0,0,0,1)
N <- c(1,1,1,1)

#The likelihood for a character that is the same for all and given the vector of branches is

sum(JCbranchPchars(branches[3],JCbranchPchars(branches[1],A)*JCbranchPchars(branches[2],A))*JCbranchPchars(branches[6],JCbranchPchars(branches[4],A)*JCbranchPchars(branches[5],A))*c(0.25,0.25,0.25,0.25))

#For the data
DataMatrix <- list()
DataMatrix$A <- list(A,A,A,C,A,A)
DataMatrix$B <- list(A,T,A,A,A,A)
DataMatrix$C <- list(A,A,A,A,G,A)
DataMatrix$D <- list(A,A,C,A,A,A)

#The likelihood will be

logLH<-0
for (i in 1:length(DataMatrix$A))
    logLH <- logLH+log(sum((JCbranchPchars(branches[3],JCbranchPchars(branches[1],DataMatrix$A[[i]])*JCbranchPchars(branches[2],DataMatrix$B[[i]]))*JCbranchPchars(branches[6],JCbranchPchars(branches[4],DataMatrix$C[[i]])*JCbranchPchars(branches[5],DataMatrix$D[[i]])))*c(0.25,0.25,0.25,0.25)))

logLH

#Try setting branch length to

branches<-c(0.188,0.188,0,0.188,0.188,0)

#Try to find better combination of branch lengths

branches<-c(0.19984, 0.19985, 0.00007, 0.18268, 0.19985 ,0)

#Change char four for taxon A to be uncertain N
DataMatrix$A[[4]]<-N

#What hapens to the likelihood for the given branch lengths?
#Can you find a better branch length leading to A?

#Reset char 4 to C and set car 1 to N in A
DataMatrix$A[[4]]<-C
DataMatrix$A[[1]]<-N

#What happens to the likelihood and how can you improve the likelihood by changing the branch leading to A?
