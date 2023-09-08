rm(list=ls())
IDCandidates <- function(Dataset, Threshold){
  #first determine our candidates.  If number of levels = 1 then all entries are the same, so no information is passed.
  #if number of levels equals the number of loans then all entries are different and no information is passed.
  Candidates <- c()
  for (k in 1:ncol(Dataset)){
    #Dataset[is.na(Dataset[,k]),k]=""
    print(colnames(Dataset)[k])
    U <- unique(Dataset[,k])
    print(length(U))
    if (length(U)==1||length(U)==nrow(Dataset)||length(U)>(nrow(Dataset)-Threshold)){
      
    }else{
      Candidates <- append(Candidates, colnames(Dataset)[k])
    }
  }
  
  return(Candidates)
}

#for debugging
  #Dataset <- LEAD[,"CURRENT_LOAN_BALANCE"]
  #Segments <- 10000

#define function Segment which buckets the dataset "Dataset" into a maximum of "Segments" buckets
Segment <- function(Dataset, Segments){
  
  #What should be the size of each bucket?  Set this value to N
  N <- length(Dataset)/Segments
  
  #Create dataset D, set it equal to Dataset
  D <- Dataset
  #create a data set that is a sorted version of D
  Ds <- sort(D)
  
  #create the first bucket.  Set all values less than D[N,] equal to D[N,] 
  #All rows in D with values less than Ds[N], which aren't NA get set equal to DS[Ceiling(N)].  
  D[D<= Ds[ceiling(N)]&is.na(D)==F] <- Ds[ceiling(N)]
  
  
  
  for (i in 2:Segments){
    D[D>Ds[ceiling(N*(i-1))]&D<=Ds[ceiling(i*N)]&is.na(D)==F]=Ds[ceiling(i*N)]
  }
  
  return(D)  
}


# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

addNoAnswer <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Answer")))
  return(x)
}


'%notin%' <- Negate('%in%')
