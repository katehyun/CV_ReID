library(pnn)

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")

sigma <- 1
numoffeatures <- 100
cat <- list()
prob <- list()

for (i in 1: length(Downheader_new[,1]) ){

  if ( ( length(candidate[[i]][1]) == 0)) {
    cat[[i]] <- NA 
    prob[[i]] <- NA 
  }
  
  else{
  seqcandidate <- seq( from = 1, to = length(candidate[[i]]) )
   
  output <- matrix(unlist( candidate[[i]]), ncol = length(candidate[[i]]))
  output2 <-t( output[seq(1, length(output[,1]), 
                        by = round (1000 / numoffeatures)),] )
  pnn_md <- data.frame(seqcandidate, output2)
#   pnn_md <- cbind( seqcandidate, output2 )
  nn <- learn( pnn_md, category.column=1)
  nn <- smooth(nn, sigma)
  
  Down <- Downobjout[i,]
  Down <- Down[seq(1, length(Down), by = round (1000 / numoffeatures) )]
  candi_guess <- list(guess(nn, Down)) 
  cat[[i]] <- candi_guess[[1]][1]
  prob[[i]] <-  candi_guess[[1]][2]

  }
}
# prob[[888]] <- NA

max_pnn_prob<-vector()
for (i in 1: length(prob)){
  max_pnn_prob[i] <- max(unlist ( prob[[i]]) )
  if (is.na(max_pnn_prob[i])) {
    max_pnn_prob[i] <- c(0)
  }
}



pnn_Upid <- c()
pnn_Upid_after <- c()

j <- 1
for (i in 1:length(Downheader_new[,1])){
  
  a <- as.numeric( unlist(cat[i]) )
  if (length(Upsiglist[[j]]) == 0 ){
    
    pnn_Upid[i] <-999
    j <- j+1}
  
  else {   
    pnn_Upid[i] <- Upsiglist[[j]][a]
    j <- j+1
  } 
}

#matching loading# 
#     matching_SOLC=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/MatchingIDSOLC.txt")
#     colnames(matching_SOLC) <- c("SO", "LC")
#     matching_SOLC <- format(matching_SOLC, scientific=FALSE)
#     
#     matching <- subset(matching_SOLC , substr(SO, 3, 13) <  substr(LC, 3, 13))



threshold_PNN<- seq(from = 0.9, to = 0.1, by = -0.1) 
Result_PNN <-data.frame()

p <- 6
rm(Target_pnnanalysis_Jan0910_table, Result_PNN, Result, TargetTable)
Target_pnnanalysis_Jan0910_obj2 <- rep(999, length(pnn_Upid))

Downheader_ID <- Downheader_new$sigid
Downtarget <- Downheader_ID

# SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/LCJan_v1.txt", fill=T)
# SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 
SOLCFHWAClass <-Downheader_new[,14]
Target_pnnanalysis_Jan0910_obj  <- (matching$SO[ match ( Downtarget,  matching$LC )])
Target_pnnanalysis_Jan0910_obj2 <- Target_pnnanalysis_Jan0910_obj

Target_pnnanalysis_Jan0910_obj2[is.na ( Target_pnnanalysis_Jan0910_obj2)]  <- c(999)

Target_pnnanalysis_Jan0910_table <- cbind(SOLCFHWAClass,max_pnn_prob, 
                                           Downtarget,  Target_pnnanalysis_Jan0910_obj, 
                                           Target_pnnanalysis_Jan0910_obj2, pnn_Upid )

mode(Target_pnnanalysis_Jan0910_table) <- "numeric"

Class <- sort(unique(Target_pnnanalysis_Jan0910_table[,1]))
p <- 7
Result_PNN <-list()
TargetTable_PNN <-list()

for (z in 1: length(Class)){
  
  setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result/") 
  TargetTable <- subset(Target_pnnanalysis_Jan0910_table, Target_pnnanalysis_Jan0910_table[,1] == Class[z])
  classresult <- f.ResultPNN (threshold_PNN,  TargetTable, p )
  Result_PNN[[z]] <- classresult$resultpnn
  TargetTable_PNN[[z]] <- classresult$tt
#   assign(paste("Result_PNN",Class[z],sep=""), classresult$resultpnn)
#   assign(paste("TargetTable",Class[z], sep=""),classresult$tt )
  #write.table(classresult$tt, paste("TargetTable",Class[z], (".txt"), sep=""), sep="\t",row.names=FALSE)
  #write.table(classresult$resultnn, paste("Result_NN",Class[z], (".txt"), sep=""), sep="\t",row.names=FALSE)
  
}

options(scipen=999)

save(prob, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/prob.RData")
save(Result_PNN, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_PNN.RData")
save(TargetTable_PNN, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_PNN.RData")

rm(TargetTable, output, output2, Down, a, a_Upid, a_Upid_after, b, base_Upid_after, candi_guess, classresult,
   i,j, p, pnn_Upid, pnn_Upid_after, seqcandidate, z)


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData") 
