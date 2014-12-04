# find mismatching
rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_12032014")

thresholdForDif <- 2.0
buf <- 0.001
weightSig <- 1000


## Extract attributes (train)
Upcandidates<- list()
Upcandidates_train<- list()
Upcandidates_test<- list()
Upcandidates_attribute_train <- list()
Upcandidates_attribute_test <- list()
Downtarget_attributes <- data.frame()


Attribute_difftemp_train <- list()
Attribute_diff_nonnormal_train <- list()
Attribute_difftemp_test <- list()
Attribute_diff_nonnormal_test <- list()
Upcandidatesindex <- list()
Upcandidatesindex_train <- list()
Upcandidatesindex_test <- list()

Downtarget_attributes_all <- cbind(Downheader_new[,13:20],Downheader_new[,7],Downheader_new[,12]  )
Downtarget_attributes_train <- subset( Downtarget_attributes_all[,], 
                                       as.numeric(str_sub (Downtarget_attributes_all[,10],-13,-1) ) <  utcbd  )
Downtarget_attributes_test <- subset( Downtarget_attributes_all[,], 
                                      as.numeric(str_sub (Downtarget_attributes_all[,10],-13,-1) ) >   utcbd   )

Upheader_new_train <- data.frame()
Upheader_new_test <- data.frame()
Upheader_new_train <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) ) <  utcbd  )
Upheader_new_test <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) )  >  utcbd  )

a_magdif_train <- a_magdif[DownheaderTrainIdx]
a_magdif_test <- a_magdif[DownheaderTestIdx]

for (i in 1: length(Upsiglist_train)) {  
  
  Upcandidatesindex_train[[i]] <- which(a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  Upcandidates_train[[i]] <- subset (Upsiglist_train[[i]], a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  
  # train 
  Upcandidates_attribute_train[[i]] <- cbind(      
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),13:20] ,
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),7] ,
    Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),12])
  
  
  
  for (j in 1: length(Upcandidatesindex_train[[i]])) {  
    Attribute_difftemp_train[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute_train[[i]][j,3:length(Upcandidates_attribute_train[[i]])]))) - 
                                            as.numeric(Downtarget_attributes_train[i,3:length(Upcandidates_attribute_train[[i]]) ])  )
    
    
    
  }
  
  
  Attribute_diff_nonnormal_train[[length(Attribute_diff_nonnormal_train)+1]] <- Attribute_difftemp_train # list in the list
  
  Attribute_difftemp_train <- list()
}






# test
for (i in 1: length(Upsiglist_test)) {  
  
  Upcandidatesindex_test[[i]] <- which(a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  Upcandidates_test[[i]] <- subset (Upsiglist_test[[i]], a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  
  
  Upcandidates_attribute_test[[i]] <- cbind(      
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),13:20] ,
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),7] ,
    Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),12])
  
  for (j in 1: length(Upcandidatesindex_test[[i]])) {  
    Attribute_difftemp_test[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute_test[[i]][j,3:length(Upcandidates_attribute_test[[i]])]))) - 
                                           as.numeric(Downtarget_attributes_test[i,3:length(Upcandidates_attribute_test[[i]]) ])  )
    
  }
  
  
  Attribute_diff_nonnormal_test[[length(Attribute_diff_nonnormal_test)+1]] <- Attribute_difftemp_test # list in the list
  Attribute_difftemp_test <- list()
}

Attribute_diff_nonnormal_train <- Attribute_diff_nonnormal_train
Attribute_diff_nonnormal_test <- Attribute_diff_nonnormal_test


Attribute_diff_train <- Attribute_diff_nonnormal_train 
for (i in 1: length(Upsiglist_train)) {  
  for (j in 1: length(Upcandidatesindex_train[[i]])) {  
    
    Attribute_diff_train[[i]][[j]][1] <- Attribute_diff_nonnormal_train[[i]][[j]][1] / max_train[1]
    Attribute_diff_train[[i]][[j]][2] <- Attribute_diff_nonnormal_train[[i]][[j]][2] / max_train[2]
    Attribute_diff_train[[i]][[j]][3] <- Attribute_diff_nonnormal_train[[i]][[j]][3] / max_train[3]
    Attribute_diff_train[[i]][[j]][4] <- Attribute_diff_nonnormal_train[[i]][[j]][4] / max_train[4]
    Attribute_diff_train[[i]][[j]][5] <- Attribute_diff_nonnormal_train[[i]][[j]][5] / max_train[5]
    Attribute_diff_train[[i]][[j]][6] <- Attribute_diff_nonnormal_train[[i]][[j]][6] / max_train[6]
    Attribute_diff_train[[i]][[j]][7] <- Attribute_diff_nonnormal_train[[i]][[j]][7] / max_train[7]
    Attribute_diff_train[[i]][[j]][8] <- Attribute_diff_nonnormal_train[[i]][[j]][8] / max_train[8]
    Attribute_diff_train[[i]][[j]][9] <- a_magdif_train[[i]][[j]]  
    
  }
}

Attribute_diff_test <- Attribute_diff_nonnormal_test 
for (i in 1: length(Upsiglist_test)) {  
  for (j in 1: length(Upcandidatesindex_test[[i]])) {  
    
    Attribute_diff_test[[i]][[j]][1] <- Attribute_diff_nonnormal_test[[i]][[j]][1] / max_test[1]
    Attribute_diff_test[[i]][[j]][2] <- Attribute_diff_nonnormal_test[[i]][[j]][2] / max_test[2]
    Attribute_diff_test[[i]][[j]][3] <- Attribute_diff_nonnormal_test[[i]][[j]][3] / max_test[3]
    Attribute_diff_test[[i]][[j]][4] <- Attribute_diff_nonnormal_test[[i]][[j]][4] / max_test[4]
    Attribute_diff_test[[i]][[j]][5] <- Attribute_diff_nonnormal_test[[i]][[j]][5] / max_test[5]
    Attribute_diff_test[[i]][[j]][6] <- Attribute_diff_nonnormal_test[[i]][[j]][6] / max_test[6]
    Attribute_diff_test[[i]][[j]][7] <- Attribute_diff_nonnormal_test[[i]][[j]][7] / max_test[7]
    Attribute_diff_test[[i]][[j]][8] <- Attribute_diff_nonnormal_test[[i]][[j]][8] / max_test[8]
#     Attribute_diff_test[[i]][[j]][9] <- sub_all_test[ match( as.numeric(Upcandidates_test[[i]][j]), 
#                                                              as.numeric( sub_all_test[,6]) ), 3]  
    Attribute_diff_test[[i]][[j]][9] <- a_magdif_test[[i]][[j]]
  }
}

# joint probability
jointprobtemp_train <- data.frame()
jointprob_train <- list()
idxjointprob_train <- vector()
UpFinalcandidates_train <- vector()

jointprobtemp_test <- data.frame()
jointprob_test <- list()
idxjointprob_test <- vector()
UpFinalcandidates_test <- vector()
k <- 0
m <- 0

Target_baseanalysis_Jan0910_table_train <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) < utcbd  )
Target_baseanalysis_Jan0910_table_test<- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd  )


for (i in 1:length(Upcandidates_train)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) {  # Only Class 9
    
  
   k <- k+1
                                                           
      for (j in 1: length(Upcandidatesindex_train[[i]])) {
        
         jointprobtemp_train[j,1] <-  Upcandidates_train[[i]][j]                                               
         jointprobtemp_train[j,2] <-(approx(kernel_mat[[1]]$x, kernel_mat[[1]]$y, Attribute_diff_train[[i]][[j]][1]) )$y
         jointprobtemp_train[j,3] <-(approx(kernel_mat[[2]]$x, kernel_mat[[2]]$y, Attribute_diff_train[[i]][[j]][2]) )$y
         jointprobtemp_train[j,4] <-(approx(kernel_mat[[3]]$x, kernel_mat[[3]]$y, Attribute_diff_train[[i]][[j]][3]) )$y
         jointprobtemp_train[j,5] <-(approx(kernel_mat[[4]]$x, kernel_mat[[4]]$y, Attribute_diff_train[[i]][[j]][4]) )$y
         jointprobtemp_train[j,6] <-(approx(kernel_mat[[5]]$x, kernel_mat[[5]]$y, Attribute_diff_train[[i]][[j]][5]) )$y
         jointprobtemp_train[j,7] <-(approx(kernel_mat[[6]]$x, kernel_mat[[6]]$y, Attribute_diff_train[[i]][[j]][6]) )$y
         jointprobtemp_train[j,8] <-(approx(kernel_mat[[7]]$x, kernel_mat[[7]]$y, Attribute_diff_train[[i]][[j]][7]) )$y
         jointprobtemp_train[j,9] <-(approx(kernel_mat[[8]]$x, kernel_mat[[8]]$y, Attribute_diff_train[[i]][[j]][8]) )$y
#          jointprobtemp_train[j,10] <-(approx(kernel_mat[[9]]$x, kernel_mat[[9]]$y, Attribute_diff_train[[i]][[j]][9]) )$y # alt 1
#          jointprobtemp_train[j,9] <- as.numeric (prob[[i]][[1]][Upcandidatesindex_train[[i]][j]] ) *w9 # alt 2
          jointprobtemp_train[j,10] <- 1 / Attribute_diff_train[[i]][[j]][9] * weightSig

                                                             
         jointprobtemp_train [is.na(jointprobtemp_train )] <- buf 
         jointprobtemp_train[j,11] <-  jointprobtemp_train[j,2] * jointprobtemp_train[j,3] *
            jointprobtemp_train[j,4] *  jointprobtemp_train[j,5] * jointprobtemp_train[j,6] *
            jointprobtemp_train[j,7] * jointprobtemp_train[j,8] *  jointprobtemp_train[j,9] *jointprobtemp_train[j,10]
                                                                                                                       
       }
    jointprob_train[[length(jointprob_train)+1]] <-  jointprobtemp_train
    jointprobtemp_train <- data.frame()
    idxjointprob_train[k] <- which.max(unlist (jointprob_train[[k]][11]) )
                                                           
    UpFinalcandidates_train[k] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[k]] ]
  }
}
  
# test
    
for (i in 1:length(Upcandidates_test)){
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    m <- m+1
                                                                
    for (j in 1: length(Attribute_diff_test[[i]])) {
                                                                  
       jointprobtemp_test[j,1] <-  Upcandidates_test[[i]][j]       
       jointprobtemp_test[j,2] <-(approx(kernel_mat[[1]]$x, kernel_mat[[1]]$y, Attribute_diff_test[[i]][[j]][1]) )$y
       jointprobtemp_test[j,3] <-(approx(kernel_mat[[2]]$x, kernel_mat[[2]]$y, Attribute_diff_test[[i]][[j]][2]) )$y
       jointprobtemp_test[j,4] <-(approx(kernel_mat[[3]]$x, kernel_mat[[3]]$y, Attribute_diff_test[[i]][[j]][3]) )$y
       jointprobtemp_test[j,5] <-(approx(kernel_mat[[4]]$x, kernel_mat[[4]]$y, Attribute_diff_test[[i]][[j]][4]) )$y
       jointprobtemp_test[j,6] <-(approx(kernel_mat[[5]]$x, kernel_mat[[5]]$y, Attribute_diff_test[[i]][[j]][5]) )$y
       jointprobtemp_test[j,7] <-(approx(kernel_mat[[6]]$x, kernel_mat[[6]]$y, Attribute_diff_test[[i]][[j]][6]) )$y
       jointprobtemp_test[j,8] <-(approx(kernel_mat[[7]]$x, kernel_mat[[7]]$y, Attribute_diff_test[[i]][[j]][7]) )$y
       jointprobtemp_test[j,9] <-(approx(kernel_mat[[8]]$x, kernel_mat[[8]]$y, Attribute_diff_test[[i]][[j]][8]) )$y
#        jointprobtemp_test[j,10] <-(approx(kernel_mat[[9]]$x, kernel_mat[[9]]$y, Attribute_diff_test[[i]][[j]][9]) )$y # alt 1
#        jointprobtemp_test[j,10] <- as.numeric (prob[[i]][[1]][Upcandidatesindex_test[[i]][j]] ) *w9 # alt 2
      jointprobtemp_test[j,10] <- 1 / Attribute_diff_test[[i]][[j]][9] * weightSig
                                                                  
       jointprobtemp_test [is.na(jointprobtemp_test )] <-buf 
       jointprobtemp_test[j,11] <-  jointprobtemp_test[j,2] * jointprobtemp_test[j,3] *
           jointprobtemp_test[j,4] *  jointprobtemp_test[j,5] * jointprobtemp_test[j,6] *
           jointprobtemp_test[j,7] * jointprobtemp_test[j,8] *  jointprobtemp_test[j,9] *jointprobtemp_test[j,10]
                                                                  
     }
    jointprob_test[[length(jointprob_test)+1]] <-  jointprobtemp_test
    jointprobtemp_test <- data.frame()
    idxjointprob_test[m] <- which.max(unlist (jointprob_test[[m]][11]) )
                                                                
    UpFinalcandidates_test[m] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[m]] ]
  } 
}


### performance 
#index for class 9
idxForClass9 <- which(Downheader_new[,14] == 9)
 
rm(ResultMisMatching_train, ResultMisMatching_test)

ResultMisMatching_train <- cbind(Target_baseanalysis_Jan0910_table_train[Target_baseanalysis_Jan0910_table_train[,1] == 9, 6],
      Target_baseanalysis_Jan0910_table_train[Target_baseanalysis_Jan0910_table_train[,1] == 9, 4], UpFinalcandidates_train)
ResultMisMatching_test<- cbind(Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1] == 9, 6],
      Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1] == 9, 4], UpFinalcandidates_test)


# train
ResultMisMatching_train <- na.omit(ResultMisMatching_train)
TargetTable_train <- ResultMisMatching_train 
Target_obj_train  <- ResultMisMatching_train [,1]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

matching_NN_train <- sum ( as.numeric ((ResultMisMatching_train [,1]) == as.numeric (ResultMisMatching_train [,3])) &
                      as.numeric (ResultMisMatching_train [,1]) != 999)

missing_NN_train <- sum ( as.numeric (ResultMisMatching_train[,1]) == c(999))

CMVeh_train <-  matching_NN_train[1]
CVeh_train <- matching_obj_train[1]
p <- 3
MVeh_train <- sum(   (as.numeric( TargetTable_train[,p])) > 1000 )  

SIMR_train <- CMVeh_train / CVeh_train
SCMR_train <- CMVeh_train / MVeh_train

MMVeh_train <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                         !=  as.numeric( TargetTable_train[,p])   ))


Veh_train <- length(TargetTable_train[,1])
SER_train <- MMVeh_train / Veh_train

ResultMismatching_train <- data.frame( matching_obj_train[1], missing_obj_train[1],              
                      matching_NN_train[[1]],  missing_NN_train[[1]],
                      CMVeh_train[[1]], CVeh_train[[1]], MVeh_train[[1]],
                      SIMR_train[[1]], SCMR_train[[1]], MMVeh_train[[1]], Veh_train[[1]], SER_train[[1]] )


# test
ResultMisMatching_test <- na.omit(ResultMisMatching_test)
TargetTable_test <- ResultMisMatching_test 
Target_obj_test  <- ResultMisMatching_test [,1]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

matching_NN_test <- sum ( as.numeric ((ResultMisMatching_test[,1]) == as.numeric (ResultMisMatching_test[,3])) &
                            as.numeric (ResultMisMatching_test [,1]) != 999)

missing_NN_test <- sum ( as.numeric (ResultMisMatching_test [,1]) == c(999))

CMVeh_test <-  matching_NN_test[1]
CVeh_test <- matching_obj_test[1]
p <- 3
MVeh_test <- sum(   (as.numeric( TargetTable_test[,p])) > 1000 )  

SIMR_test <- CMVeh_test / CVeh_test
SCMR_test <- CMVeh_test / MVeh_test

MMVeh_test <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                              !=  as.numeric( TargetTable_test[,p])   ))


Veh_test <- length(TargetTable_test[,1])
SER_test <- MMVeh_test / Veh_test

ResultMismatching_test <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                     matching_NN_test[[1]],  missing_NN_test[[1]],
                                     CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                     SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )


# loo more closely
Upsiglist[[ idxForClass9[[16]]  ]]
a_magdif [[ idxForClass9[[16]]  ]]
jointprob_train[[16]]
ResultMismatching_test
ResultMismatching_train
