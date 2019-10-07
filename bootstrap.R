#This is an example of how bootstrap replicates of a data matrix can be generated

#Let the bases be
T <- c(1,0,0,0)
C <- c(0,1,0,0)
A <-c(0,0,1,0)
G <-c(0,0,0,1)
N <- c(1,1,1,1)

#The data matrix
dataMatrix <- list()
dataMatrix$A <- list(A,C,G,T,A,T,G,G,A,T)
dataMatrix$B <- list(A,C,G,G,G,T,G,C,A,T)
dataMatrix$C <- list(A,A,C,G,G,T,G,G,A,T)
dataMatrix$D <- list(A,A,C,T,G,T,G,C,A,T)

#Variables for bootstrap
bootstrapSamples <- list(); #list for bootstrap replicates
nSamples <- 100 #Number of replicates to get
columns <- c() #Variable to select columns

for (n in 1:nSamples) { #For every sample
    columns <- sample(1:length(dataMatrix[[1]]),length(dataMatrix[[1]]),replace=TRUE) #Select columns with replacement
    bootstrapSamples[[n]] <- list()
    for (i in 1:length(dataMatrix)) { #For each taxon in data matrix
	bootstrapSamples[[n]][[i]]<-list()
	for (j in 1:length(columns)) { #Get the value at each site (i.e. column) in sequence
	    bootstrapSamples[[n]][[i]][[j]] <- c()
	    bootstrapSamples[[n]][[i]][[j]] <- dataMatrix[[i]][[columns[j]]]
	}
    }
}

#The function nDiff gives you the number of differences between two sequences
nDiff <- function ( seq1, seq2 ) {
    diff<-c()
    for (i in 1:length(seq1)) {
	if (sum(seq1[[i]]*seq2[[i]])) diff[i] <- 0
	else diff[i] <- 1
    }
    return (sum(diff))
}

#Can you calculate the bootstrap support for the different topologies for the for taxa using the ME criterion?
