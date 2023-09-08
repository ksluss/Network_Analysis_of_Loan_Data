
library(igraph)
options(scipen=999)

Dataset <- LEAD
Dataset <- as.data.frame(lapply(Dataset, addNoAnswer))


TotPoss <- choose(nrow(Dataset), 2)
TotPoss

dim(Dataset)
Candidates <- c()

Delinq <- LEAD2[is.na(LEAD$CURRENT_DELINQUENCY_STATUS), ]
nrow(Delinq)/nrow(LEAD2)


rownames(Delinq) <- Delinq[,"UNQ_ID_IN_SRC_SYST"]
#for testing, trim the data set to a small size
#samplesize <- 50000
#Dataset <- Dataset[1:samplesize,]

#put all loan ids into an array called vertices and create an empty datset named "Edges".  This data set will contain a row for each of our variables and will house the number of edges
Vertices <- c(Dataset[,'UNQ_ID_IN_SRC_SYST'])
Edges <- data.frame(row.names=Candidates)

CompleteFrom <- data.frame()
CompleteTo <- data.frame()

#because our graph is undirected, and the size of the data set is immense, we rely on the commutative principal.  Therefore, the maximum size of any graph will be equal to the number of data elements
TotPoss <- nrow(Dataset)

#Which columns in our dataset do we want to analyze?  
Candidates <- colnames(Dataset)
#ignore the loan ID field
Candidates <- Candidates[2:length(Candidates)]

#create matrices for later use
LoanIDs <- matrix(nrow=TotPoss, ncol=length(Candidates))
Values <- matrix(nrow=TotPoss, ncol=length(Candidates))
Count <- matrix()
g<- data.frame()

#for debugging
k<-5

#for each column in our data set, create a list of edges
for (k in 1:length(Candidates)){
  print(Candidates[k])
  colnum <- which (colnames(Dataset)==Candidates[k])
  
  
  
  #set the na values equal to blank
  Dataset[is.na(Dataset[,colnum]),colnum] <-"No Answer"
  
  
  From <- data.frame()
  To <- data.frame()
  
  #put our loan IDs and the column we'll be checking into a data frame
  TempData <- data.frame(Dataset[,'UNQ_ID_IN_SRC_SYST'],Dataset[,colnum])
  colnames(TempData) <- c('UNQ_ID_IN_SRC_SYST',Candidates[k])
  while (nrow(TempData)>0){
    #print(nrow(TempData))
    print(length(unique(TempData[,2])))
    
    #determine all the loan IDs for which the value of our variable matches the first value
    Temp <- TempData[compareNA(TempData[,2],TempData[1,2]),'UNQ_ID_IN_SRC_SYST']
    
    #if there was at least one match (which there always should be) add the edge to our from and to vectors
    N <- length(Temp)
    if (N>0){
      To <- rbind(To, t(t(Temp)))
      From <- rbind(From, t(t(rep(Temp[1],N))))
    }
    #check the dimensions (for debugging)
    #dim(To)
    #dim(From)
    
    #determine all of the rows for which the value of the variable did not agree with entry in row 1.  Adjust TempData to only include these rows
    Temp <-TempData[TempData[,2]!=TempData[1,2],]
    TempData <- Temp
    dim(TempData)
  }
  
  Edges[k,1]=0
  #print(ecount(graph_from_data_frame(data.frame(From,To),directed=F,vertices=Vertices)))
  df <- data.frame(From,To)
  if (nrow(df)>0){
    ColName <- colnames(Dataset)[k]
    g <- simplify(graph_from_data_frame(df,directed=F,vertices=Vertices),remove.loops=T)
    CompleteFrom[1:nrow(From),k]<- From
    CompleteTo[1:nrow(To),k]<- To
    colnames(CompleteFrom)[k] <- Candidates[k]
    colnames(CompleteTo)[k] <- Candidates[k]
    Edges[k,1] <- ecount(g)
    Edges[k,2] <- Edges[k,1]/TotPoss
  }
}
Edges

#Determine the importance of each Variable
i <- 1
GoodLoans <- Dataset[Dataset$UNQ_ID_IN_SRC_SYST %notin% Delinq$UNQ_ID_IN_SRC_SYST,]

CompleteGood <- data.frame()
CompleteBad <- data.frame()
CompleteSize <- data.frame()

for (i in 1:length(Candidates)){
  print(Candidates[i])
  g <- simplify(graph_from_data_frame(data.frame(CompleteFrom[,i],CompleteTo[,i]),directed=F,vertices=Vertices),remove.loops=T)
  C <- components(g)
  Membership <- C$membership  
  
  BadCount <- data.frame(rep(0,C$no))
  GoodCount <- data.frame(rep(0,C$no))

  for (j in 1:nrow(Delinq)){
    BadCount[Membership[Delinq[j,"UNQ_ID_IN_SRC_SYST"]],1] <-BadCount[Membership[Delinq[j,"UNQ_ID_IN_SRC_SYST"]],1]+1
  }

  for (j in 1:nrow(GoodLoans)){
    GoodCount[Membership[GoodLoans[j,1]],1] <-GoodCount[Membership[GoodLoans[j,1]],1]+1
  }
  
  Size <- data.frame(C$csize)
  Pct <- data.frame(BadCount/Size, GoodCount/Size, Size/nrow(Delinq))
  colnames(Pct) <- c("Percent Bad", "Percent Good", "Percent of Bad")
 
  CompleteGood[1:nrow(GoodCount),i]<- GoodCount
  CompleteBad[1:nrow(BadCount),i]<- BadCount
  CompleteSize[1:nrow(Size),i] <- Size
}

colnames(CompleteGood) <- Candidates
colnames(CompleteBad) <- Candidates
colnames(CompleteSize) <- Candidates


i <- which(Candidates=="ORIGINATION_DATE")


#score each variable based on a given Threshold
Threshold <- 1
Score <- data.frame()
for (i in 1:ncol(CompleteBad)){
  Bad <- CompleteBad[,i]
  Good <- CompleteGood[,i]
  Size <- CompleteSize[,i]
  df <- data.frame(Bad/Size, Good/Size, Size, Size/nrow(Delinq))
  DF <- df[df[,1]>=Threshold & is.na(df[,1])==F,]
  Score[i,1] <- sum(DF[,3])
  Score[i,2] <- sum(DF[,1]*DF[,4])
}
df[1:20,]
colnames(df) <- c("Percent Bad", "Percent Good","Size", "Percent of Bad")
rownames(Score) <- Candidates
Score

