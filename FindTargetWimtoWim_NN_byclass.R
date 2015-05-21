# utils:::menuInstallPkgs() 
rm(list=ls())
# load functonbook2
# library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
options(scipen=999) 
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/shiftandstretch_Jan0910_04272015.RData")

# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_04272015.RData ")
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif_04272015.RData ")
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_04272015.RData")
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")

rm (rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8 )
rm(swift, stret, ss, splineDown, splineUp, minstretmagdif, minswiftmagdif, magdif, candi_magdif)
############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
# min magdif
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}

min_a_basemagdif<-vector()
for (i in 1: length(a_basemagdif)){
  min_a_basemagdif[i] <- min(a_basemagdif[[i]])
}


idx_basemagdif <- lapply(a_basemagdif,which.min)
idx_magdif <- lapply(a_magdif,which.min)

base_Upid <- c()
base_Upid_after <- c()

j=1

for (i in 1:length(idx_basemagdif)){
  a <- unlist(idx_basemagdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    base_Upid[i] <- Upsiglist[[j]][a]
    j <- j+1
  }
  
}

a_Upid <- c()
base_Upid <- c()
a_Upid_after <- c()

j <- 1
for (i in 1:length(a_magdif)){
  
  a <- unlist(idx_magdif[i])
  b <- unlist(idx_magdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    a_Upid[i] <-999
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    a_Upid[i] <- Upsiglist[[j]][a]
    base_Upid[i] <- Upsiglist[[j]][b]
    j <- j+1
  }
  
}


#matching loading# 
#     matching_SOLC=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/MatchingIDSOLC.txt")
#     colnames(matching_SOLC) <- c("SO", "LC")
#     matching_SOLC <- format(matching_SOLC, scientific=FALSE)
#     
#     matching <- subset(matching_SOLC , substr(SO, 3, 13) <  substr(LC, 3, 13))


# Jan 0910
p <- 7
rm(Target_baseanalysis_Jan0910_table, Result_NN, Result)
Target_baseanalysis_Jan0910_obj2 <- rep(999, length(a_Upid))

Downtarget <- vector()
Downtarget <- Downheader_new$sigid

# library( RPostgreSQL)
# drv <- dbDriver("PostgreSQL")
# con <- f.dbinfo (drv)

# LCGTML = Leucadia data
# LCGTML <- dbGetQuery(con, 
#             "SELECT  groundtruthmasterlist.vehid, wimlink.wimid, wimsignaturelink.wimsigid, 
#             groundtruthmasterlist.station, groundtruthmasterlist.lane,
#             veh_class, wimrecords.ts,  veh_len, gross_weight, axle_1_2_spacing, axle_2_3_spacing,
#             axle_3_4_spacing, axle_4_5_spacing, 
#             axle_1_rt_weight, axle_1_lt_weight, axle_2_rt_weight, axle_2_lt_weight, axle_3_rt_weight,
#             axle_3_lt_weight, axle_4_rt_weight, 
#             axle_4_lt_weight, axle_5_rt_weight, axle_5_lt_weight
#             FROM  gtsystem.wimlink, gtsystem.wimsignaturelink, gtsystem.wimrecords,gtsystem.groundtruthmasterlist
#             where groundtruthmasterlist.vehid = wimlink.vehid 
#             and groundtruthmasterlist.vehid = wimsignaturelink.vehid
#             and wimlink.wimid = wimrecords.wim_id
#             and station = 84
#             order by wimrecords.ts")


SOLCFHWAClass <-Downheader_new[,14]
# dbDisconnect(con)
# SOLCAllFHWAClass <- read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/LCJan/LCJan_v1.txt", fill=T)
# SOLCFHWAClass <- SOLCAllFHWAClass[,6] [match (Downtarget, SOLCAllFHWAClass[,3])] 



Target_baseanalysis_Jan0910_obj  <- (matching$SO[ match (  Downtarget , matching$LC  )])
Target_baseanalysis_Jan0910_obj2 <- Target_baseanalysis_Jan0910_obj

Target_baseanalysis_Jan0910_obj2[is.na ( Target_baseanalysis_Jan0910_obj2)]  <- c(999)


Target_baseanalysis_Jan0910_table <- cbind(SOLCFHWAClass,min_a_basemagdif,min_a_magdif, 
                                            Downtarget,  Target_baseanalysis_Jan0910_obj, 
                                           Target_baseanalysis_Jan0910_obj2, base_Upid, a_Upid )

mode(Target_baseanalysis_Jan0910_table) <- "numeric"

# start from here  - by Class
threshold_NN<- seq(from = 0, to = 10, by = 0.5) 
# Result_NN <-list()


Class <- sort(unique(Target_baseanalysis_Jan0910_table[,1]))
p <- 9

# by class
options(scipen=999)
Result_tmp_NN <- list()
Result_NN <- list()
TargetTable_tmp_NN <- list()
TargetTable_NN <- list()
# 
# # first module (very bad performance)
# duplicateindex_archive <- vector()
# duplicateindex <- vector()
# 
# for (z in 2: length(Target_baseanalysis_Jan0910_table[,1])){
#   
#  
#   duplicatenumbers_temp <-  unique(Target_baseanalysis_Jan0910_table[1:z,8] [ duplicated(Target_baseanalysis_Jan0910_table[1:z,8] )])
#   remove <- c(999)
#   duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
#   duplicateindex <- t( which( Target_baseanalysis_Jan0910_table[1:z,8]  %in%   duplicatenumbers ) )
#   
#   if (length(duplicateindex) > 0) {
#     duplicateindex_archive <- cbind(duplicatenumbers, duplicateindex_archive  )
#     duplicateindex_archive_table <- as.data.frame ( table ( duplicateindex_archive ) )
#   }
#                            
#   
#   if (length(duplicateindex) > 0) {
#   
#           while ( length(duplicateindex) != 0 ){
#             
#             
#             if (Target_baseanalysis_Jan0910_table[duplicateindex[1] ,3] < Target_baseanalysis_Jan0910_table[duplicateindex[2] ,3] )
#             {
#               frequency <- duplicateindex_archive_table [ which ( duplicateindex_archive_table$duplicateindex_archive == 
#                                                                     Target_baseanalysis_Jan0910_table[duplicateindex[2] ,8] ) , 2] + 1
#               
#               if ( frequency < length (a_magdif[[ duplicateindex[2] ]] )) {
#                   secondbigidx <- which (a_magdif[[ duplicateindex[2] ]] == sort( a_magdif[[ duplicateindex[2] ]], partial= 2)[frequency] ) 
#                   Target_baseanalysis_Jan0910_table[duplicateindex[2], 8 ] <- Upsiglist[[duplicateindex[2]]][secondbigidx]
#                   Target_baseanalysis_Jan0910_table[duplicateindex[2], 3 ] <- a_magdif[[ duplicateindex[2] ]][secondbigidx]
# #                   Target_baseanalysis_Jan0910_table[duplicateindex[2], 2 ] <- a_basemagdif[[ duplicateindex[2] ]][secondbigidx]
#               }
#               
#               else {
#                 Target_baseanalysis_Jan0910_table[duplicateindex[2], 8 ] <- 999
#                 Target_baseanalysis_Jan0910_table[duplicateindex[2], 3 ] <- 999
#                 Target_baseanalysis_Jan0910_table[duplicateindex[2], 2 ] <- 999
#               }
#             }
#                 
# 
#             else
#             {
#               frequency <- duplicateindex_archive_table [ which ( duplicateindex_archive_table$duplicateindex_archive == 
#                                                                     Target_baseanalysis_Jan0910_table[duplicateindex[1] ,8] ) , 2] + 1
#               
#               if ( frequency < length (a_magdif[[ duplicateindex[1] ]] )) {
#                     secondbigidx <- which (a_magdif[[ duplicateindex[1] ]] == sort( a_magdif[[ duplicateindex[1] ]], partial= 2)[frequency] )
#                     Target_baseanalysis_Jan0910_table[duplicateindex[1], 8 ] <- Upsiglist[[duplicateindex[1]]][secondbigidx]
#                     Target_baseanalysis_Jan0910_table[duplicateindex[1], 3 ] <- a_magdif[[ duplicateindex[1] ]][secondbigidx]
# #                     Target_baseanalysis_Jan0910_table[duplicateindex[1], 2 ] <- a_basemagdif[[ duplicateindex[1] ]][secondbigidx]
#               }
#               
#               else {
#                 Target_baseanalysis_Jan0910_table[duplicateindex[1], 8 ] <- 999
#                 Target_baseanalysis_Jan0910_table[duplicateindex[1], 3 ] <- 999
#                 Target_baseanalysis_Jan0910_table[duplicateindex[1], 2 ] <- 999
#               }
#                 
#               }
#               
#                           
#             
#             duplicatenumbers_temp <-  unique(Target_baseanalysis_Jan0910_table[1:z,8] [ duplicated(Target_baseanalysis_Jan0910_table[1:z,8] )])
#             remove <- c(999)
#             duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
#             duplicateindex <- which( Target_baseanalysis_Jan0910_table[1:z,8]  %in%   duplicatenumbers )
#                         
#             duplicateindex_archive <- cbind(duplicatenumbers, duplicateindex_archive  )
#             duplicateindex_archive_table <- as.data.frame ( table ( duplicateindex_archive ) )
#             
#           }
#   }
# }

# second module

for (z in 1: length(Class)){

  setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result/") 
  TargetTable <- subset(Target_baseanalysis_Jan0910_table, Target_baseanalysis_Jan0910_table[,1] == Class[z])
  classresult <- f.ResultNN (threshold_NN,  TargetTable, p )
  Result_NN[[z]] <- classresult$resultnn
  TargetTable_NN[[z]] <- classresult$tt
 
  
  #   classresult <- list()
  #   assign(paste("Result_NN",Class[z],sep=""), classresult$resultnn)
  #   assign(paste("TargetTable_NN",Class[z], sep=""),classresult$tt )
  #write.table(classresult$tt, paste("TargetTable",Class[z], (".txt"), sep=""), sep="\t",row.names=FALSE)
  #write.table(classresult$resultnn, paste("Result_NN",Class[z], (".txt"), sep=""), sep="\t",row.names=FALSE)
}



# write.table(TargetTable9, "./TargetTable9.txt", sep="\t",row.names=FALSE)
 write.table(Result_NN[5], "./Result_NN_Class9.txt", sep="\t",row.names=FALSE)

# utils::View(Result_NN9)
rm ( a, b, a_Upid_after, base_Upid_after, Result_tmp_NN, classresult, p, z  )

save(Target_baseanalysis_Jan0910_table , file= "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Target_baseanalysis_Jan0910_table.RData")
save(Result_NN, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_NN.RData")
save(TargetTable_NN, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_NN.RData")
save(SOLCFHWAClass,file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/SOLCFHWAClass.RData" )
# save(Target_baseanalysis_Jan0910_table, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Target_base_Jan0910.RData")