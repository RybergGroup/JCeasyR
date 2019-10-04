#Give probability of observing nDiff differences in a seqLength long sequence given branchLength
JCbrachPseq <- function(branchLength,seqLength,nDiff) {
    return((((1/4)-(1/4)*exp(-(4/3)*branchLength))^nDiff)*((1/4)+(3/4)*exp(-(4/3)*branchLength))^(seqLength-nDiff))
}

#Give Jukes Cantor distance given observed distance X
JCdistance <- function(x) { return((-3/4)*log(1-((4/3)*x))) }

#Give probability of for the four different bases for one site given branch length branchLength and the probability at start/end of branch (basePs)
JCbranchPchars <- function(branchLength, basePs) {
    basePsEnd <- c()
    for (i in 1:length(basePs)) {
	basePsEnd[i]<-0
	for (j in 1:length(basePs)) {
	    if (i==j) basePsEnd[i] <- basePsEnd[i]+basePs[j]*((1/4)+(3/4)*exp(-(4/3)*branchLength))
	    else basePsEnd[i] <- basePsEnd[i]+basePs[j]*((1/4)-(1/4)*exp(-(4/3)*branchLength))
	}
    }
    return(basePsEnd)
}

