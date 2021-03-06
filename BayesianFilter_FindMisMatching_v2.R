# find mismatching
rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_12152014")

thresholdForDif <- 2.0
buf <- 0.001
weightSig <- 0.1


## Extract attributes (train)
Upcandidates<- list()
Upcandidates_train<- list()
Upcandidates_test<- list()

Downtarget_attributes_all <- data.frame()


Upcandidatesindex <- list()
Upcandidatesindex_train <- list()
Upcandidatesindex_test <- list()


colnames(Downheader_new)[7] <- c("duration")
colnames(Downheader_new)[14:44] <- c( "class", "numax", "utc", "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt")

Downtarget_attributes_all <- cbind(Downheader_new[,13:44],Downheader_new[,7] )

library(stringr)
Downtarget_attributes_train <- subset( Downtarget_attributes_all[,], 
                                       as.numeric(str_sub (Downtarget_attributes_all[,4],-13,-1) ) >  utcbd  )
Downtarget_attributes_test <- subset( Downtarget_attributes_all[,], 
                                      as.numeric(str_sub (Downtarget_attributes_all[,4],-13,-1) ) <   utcbd   )




colnames(Upheader_new)[7] <- c("duration")
colnames(Upheader_new)[14:44] <- c( "class", "numax", "utc", "length", "gvw", 
                                      "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                                      "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                                      "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                                      "ax9lwt", "ax9rwt")

Upheader_new_train <- data.frame()
Upheader_new_test <- data.frame()
Upheader_new_train <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) ) >  utcbd  )
Upheader_new_test <-  subset(Upheader_new[,], as.numeric(str_sub (Upheader_new[,12],-13,-1) )  <  utcbd  )

a_magdif_train <- a_magdif[DownheaderTrainIdx]
a_magdif_test <- a_magdif[DownheaderTestIdx]




Upcandidates_attribute_train <- list()
Upcandidates_attribute_test <- list()

Attribute_difftemp_train <- list()
Attribute_diff_nonnormal_train <- list()
Attribute_difftemp_test <- list()
Attribute_diff_nonnormal_test <- list()


for (i in 1: length(Upsiglist_train)) {  
  
  Upcandidatesindex_train[[i]] <- which(a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  Upcandidates_train[[i]] <- subset (Upsiglist_train[[i]], a_magdif_train[[i]] < thresholdForDif * min(a_magdif_train[[i]]) )
  
  # train 
  if ( any (is.na (Upcandidates_train[[i]] ) ) )
    Upcandidates_attribute_train[[i]] <- 999
  
  else {
    Upcandidates_attribute_train[[i]] <- cbind(      
      Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),13:44] , 
      Upheader_new_train[ match( as.numeric(Upcandidates_train[[i]]), as.numeric(Upheader_new_train[,13])),7] 
     )
  }
  
  

  
  for (j in 1: length(Upcandidatesindex_train[[i]])) {  
    
    
    if (Upcandidates_attribute_train[[i]] != 999 ) {
    Attribute_difftemp_train[[j]] <-  
     abs (as.numeric( (unlist (Upcandidates_attribute_train[[i]][j,4:33] )  ) ) - 
       as.numeric(Downtarget_attributes_train[i,4:33] )  )     
    }
    
    else {
      Attribute_difftemp_train[[j]] <- NA      
    }
   
  }
  
  
  Attribute_diff_nonnormal_train[[length(Attribute_diff_nonnormal_train)+1]] <- Attribute_difftemp_train # list in the list
  Attribute_difftemp_train <- list()
  
}





# test
for (i in 1: length(Upsiglist_test)) {  
  
  Upcandidatesindex_test[[i]] <- which(a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  Upcandidates_test[[i]] <- subset (Upsiglist_test[[i]], a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  
  
  # train 
  if ( any (is.na (Upcandidates_test[[i]] ) ) )
    Upcandidates_attribute_test[[i]] <- 999
  
  else {
    Upcandidates_attribute_test[[i]] <- cbind(      
      Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),13:44] ,
      Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),7] 
    )
  }
  
  
  
  
  for (j in 1: length(Upcandidatesindex_test[[i]])) {  
    
    if (Upcandidates_attribute_test[[i]] != 999 ) {
      Attribute_difftemp_test[[j]] <-  
        abs (as.numeric( (unlist (Upcandidates_attribute_test[[i]][j,4:33] )  ) ) - 
               as.numeric(Downtarget_attributes_test[i,4:33] )  )     
    }
    
    else {
      Attribute_difftemp_test[[j]] <- NA     
    }
    
  }

  
  Attribute_diff_nonnormal_test[[length(Attribute_diff_nonnormal_test)+1]] <- Attribute_difftemp_test # list in the list
  Attribute_difftemp_test <- list()
}




Attribute_diff_train_mat <-  Attribute_diff_nonnormal_train
Attribute_diff_train_nonmat <-  Attribute_diff_nonnormal_train

Attribute_diff_test_mat <- Attribute_diff_nonnormal_test
Attribute_diff_test_nonmat <- Attribute_diff_nonnormal_test



for (i in 1: length(Upsiglist_train)) {  
  for (j in 1: length(Upcandidatesindex_train[[i]])) {  
    
 
    
    for (k in 1: 30) {
      
      Attribute_diff_train_mat[[i]][[j]][k] <- ( Attribute_diff_nonnormal_train[[i]][[j]][k] - min_train_mat[k] ) /  
        ( max_train_mat[k] -  min_train_mat[k] )
      
      Attribute_diff_train_nonmat[[i]][[j]][k] <-  ( Attribute_diff_nonnormal_train[[i]][[j]][k] - min_train_nonmat[k] ) /  
        ( max_train_nonmat[k] -  min_train_nonmat[k] )
      
      
      if (Attribute_diff_train_mat[[i]][[j]][k] < 0 & ( !is.na( Attribute_diff_train_mat[[i]][[j]][k] ) ) )
      { Attribute_diff_train_mat[[i]][[j]][k] <- 0 }
      
      if ( Attribute_diff_train_nonmat[[i]][[j]][k] < 0 & ( !is.na( Attribute_diff_train_nonmat[[i]][[j]][k] ) ) )
      { Attribute_diff_train_nonmat[[i]][[j]][k] <- 0 }
      
    }
    
    Attribute_diff_train_mat[[i]][[j]][31] <- a_magdif_train[[i]][[j]] 
    Attribute_diff_train_nonmat[[i]][[j]][31] <- a_magdif_train[[i]][[j]] 
    
    Attribute_diff_train_mat[[i]][[j]][31] <- ( a_magdif_train[[i]][[j]] - min_train_mat[31] ) /
      ( max_train_mat[31] -  min_train_mat[31] )
    Attribute_diff_train_nonmat[[i]][[j]][31] <- ( a_magdif_train[[i]][[j]] - min_train_nonmat[31] ) /
      ( max_train_nonmat[31] -  min_train_nonmat[31] )
   
    
  }
}


for (i in 1: length(Upsiglist_test)) {  
  for (j in 1: length(Upcandidatesindex_test[[i]])) {  
    
  
    
    for (k in 1: 30) {
      
      Attribute_diff_test_mat[[i]][[j]][k] <-  ( Attribute_diff_nonnormal_test[[i]][[j]][k] - min_train_mat[k] ) /  
        ( max_train_mat[k] -  min_train_mat[k] )
      
      Attribute_diff_test_nonmat[[i]][[j]][k] <-  ( Attribute_diff_nonnormal_test[[i]][[j]][k] - min_train_nonmat[k] ) /  
        ( max_train_nonmat[k] -  min_train_nonmat[k] )
      
      if (Attribute_diff_test_mat[[i]][[j]][k] < 0 & ( !is.na( Attribute_diff_test_mat[[i]][[j]][k] ) ) )
      { Attribute_diff_test_mat[[i]][[j]][k] <- 0 }
      
      if ( Attribute_diff_test_nonmat[[i]][[j]][k] < 0 & ( !is.na( Attribute_diff_test_nonmat[[i]][[j]][k] ) ) )
      { Attribute_diff_test_nonmat[[i]][[j]][k] <- 0 }
      
    } 
    
    Attribute_diff_test_mat[[i]][[j]][31] <- ( a_magdif_test[[i]][[j]] - min_train_mat[31] ) /
      ( max_train_mat[31] -  min_train_mat[31] )
    Attribute_diff_test_nonmat[[i]][[j]][31] <- ( a_magdif_test[[i]][[j]] - min_train_nonmat[31] ) /
      ( max_train_nonmat[31] -  min_train_nonmat[31] )
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

Target_baseanalysis_Jan0910_table_train <- data.frame()
Target_baseanalysis_Jan0910_table_test <- data.frame()



Target_baseanalysis_Jan0910_table_train <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd  )
Target_baseanalysis_Jan0910_table_test  <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) < utcbd  )

k <- 0
m <- 0
p <- 0

class9idxprob <- c(1:4, 6:7, 13:21, 30:31)
Downtarget_attributes_train_Class9 <- subset ( Downtarget_attributes_train ,Downtarget_attributes_train [,2] == 9  )

for (i in 1:length(Upcandidates_train)){
# for (i in 1:3){
  
  if ( as.numeric ( Downtarget_attributes_train [i,2] ) == 9 ) {  # Only Class 9
    
  
#    k <- k+1
   
   if ( is.na ( Attribute_diff_nonnormal_train[[i]][1])   ) {
     idxjointprob_train[i] <- 999
     UpFinalcandidates_train[i] <- 999
   }
   
   else {
            
#       p <- p+1 
     
      for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
                           
          for (m in 1: 31) {
            
              # option 1 - non parametric
              if ( m %in% class9idxprob) {
               jointprobtemp_train[j,m] <- as.numeric ( (approx( kernel_mat[[m]]$x, kernel_mat[[m]]$y,
                                                     Attribute_diff_train_mat[[i]][[j]][m]) )$y )
              }
              
#               # option 2 - parametric
#               if ( m %in% class9idxprob) {
#                 jointprobtemp_train[j,m] <- as.numeric ( (approx( kernel_para_mat[[m]]$x, kernel_para_mat[[m]]$y,
#                                                                   Attribute_diff_train_mat[[i]][[j]][m]) )$y )
#               }
#               
              
              else {
                jointprobtemp_train[j,m] <- 99999
              }
          
          }
         
#           jointprobtemp_train[j,31] <- Attribute_diff_train_mat[[i]][[j]][31]
          jointprobtemp_train[j,31] <- 1 /  Attribute_diff_train_mat[[i]][[j]][31]   
          jointprobtemp_train [is.na(jointprobtemp_train )] <- buf 

         
          
#           # option 1
#            jointprobtemp_train[j,32] <-  jointprobtemp_train[j,1] * jointprobtemp_train[j,2] * jointprobtemp_train[j,3]  * 
#               jointprobtemp_train[j,6] * jointprobtemp_train[j,7] * 
#               jointprobtemp_train[j,14] *  jointprobtemp_train[j,16] *
#               jointprobtemp_train[j,18] *  jointprobtemp_train[j,20] *
#               jointprobtemp_train[j,30]  *
#               jointprobtemp_train[j,31]  

#                ( 1 /jointprobtemp_train[j,31] * weightSig )


          # option 2
         
#           jointprobtemp_train[j,32] <-  log10(jointprobtemp_train[j,1]) +  log10(jointprobtemp_train[j,2]) +
#             log10(jointprobtemp_train[j,3])  + 
#             log10(jointprobtemp_train[j,6])  + log10(jointprobtemp_train[j,7]) + 
#             log10(jointprobtemp_train[j,14]) + log10(jointprobtemp_train[j,16]) +
#             log10(jointprobtemp_train[j,18]) + log10(jointprobtemp_train[j,20]) +
#             log10(jointprobtemp_train[j,30]) +
#             (log10(jointprobtemp_train[j,31])  * weightSig)

          jointprobtemp_train[j,32] <-  log10(jointprobtemp_train[j,1]) +  log10(jointprobtemp_train[j,2]) +
            log10(jointprobtemp_train[j,3])  + log10(jointprobtemp_train[j,4]) + log10(jointprobtemp_train[j,5])
          log10(jointprobtemp_train[j,6])  + log10(jointprobtemp_train[j,7]) + 
            log10(jointprobtemp_train[j,14]) + log10(jointprobtemp_train[j,15]) + log10(jointprobtemp_train[j,16]) +
            log10(jointprobtemp_train[j,17]) + log10(jointprobtemp_train[j,18]) + log10(jointprobtemp_train[j,19]) + 
            log10(jointprobtemp_train[j,20]) + log10(jointprobtemp_train[j,30]) +
            (log10(jointprobtemp_train[j,31])  * weightSig)

          jointprobtemp_train[j,33] <-  Upcandidates_train[[i]][j]   
         
      }
                                                                                                                       
     
    jointprob_train[[length(jointprob_train)+1]] <-  jointprobtemp_train
    jointprobtemp_train <- data.frame()
   
#     idxjointprob_train[k] <- which.max(unlist (  jointprob_train[[length(jointprob_train)]][32]) )  
#     UpFinalcandidates_train[k] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[k]] ]  
    idxjointprob_train[i] <- which.max(unlist (  jointprob_train[[length(jointprob_train)]][32]) )  
    UpFinalcandidates_train[i] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i]] ]  
#     UpFinalcandidates_train[k] <- jointprob_train[[length(jointprob_train)]][ idxjointprob_train[k] ,33 ] 
  
   } 
  }
    
    else
      
    {
      jointprob_train[[length(jointprob_train)+1]] <-  NA
    
      idxjointprob_train[i] <-NA
      UpFinalcandidates_train[i] <- NA
    }
}





# test


k <- 0
m <- 0
p <- 0


for (i in 1:length(Upcandidates_test)){
  # for (i in 1:3){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) {  # Only Class 9
    
    
    k <- k+1
    
    if ( is.na ( Attribute_diff_nonnormal_test[[i]][1])   ) {
      idxjointprob_test[k] <- 999
      UpFinalcandidates_test[k] <- 999
    }
    
    else {
      
      #       p <- p+1 
      
      for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
        
        for (m in 1: 31) {
          
#           # option 1
          if ( m %in% class9idxprob) {
            jointprobtemp_test[j,m] <- as.numeric ( (approx( kernel_mat[[m]]$x, kernel_mat[[m]]$y,
                                                             Attribute_diff_test_mat[[i]][[j]][m]) )$y )
          }
          
#            # option 2
#             if ( m %in% class9idxprob) {
#               jointprobtemp_test[j,m] <- as.numeric ( (approx( kernel_para_mat[[m]]$x, kernel_para_mat[[m]]$y,
#                                                                        Attribute_diff_test_mat[[i]][[j]][m]) )$y )
#               }
#           
          
          else {
            jointprobtemp_test[j,m] <- 99999
          }
          
        }
        
#         jointprobtemp_test[j,31] <- Attribute_diff_test_mat[[i]][[j]][31]
        jointprobtemp_test[j,31] <- 1 /  Attribute_diff_test_mat[[i]][[j]][31]   
        jointprobtemp_test [is.na(jointprobtemp_test )] <- buf 

        jointprobtemp_test[j,32] <-  log10(jointprobtemp_test[j,1]) +  log10(jointprobtemp_test[j,2]) +
          log10(jointprobtemp_test[j,3])  + log10(jointprobtemp_test[j,4]) + log10(jointprobtemp_test[j,5])
        log10(jointprobtemp_test[j,6])  + log10(jointprobtemp_test[j,7]) + 
          log10(jointprobtemp_test[j,14]) + log10(jointprobtemp_test[j,15]) + log10(jointprobtemp_test[j,16]) +
          log10(jointprobtemp_test[j,17]) + log10(jointprobtemp_test[j,18]) + log10(jointprobtemp_test[j,19]) + 
          log10(jointprobtemp_test[j,20]) + log10(jointprobtemp_test[j,30]) +
          (log10(jointprobtemp_test[j,31])  * weightSig)
        
#         jointprobtemp_test[j,32] <-  jointprobtemp_test[j,1] * jointprobtemp_test[j,2] * jointprobtemp_test[j,3] *  
#           jointprobtemp_test[j,6] * jointprobtemp_test[j,7] *  
#           jointprobtemp_test[j,14] *  jointprobtemp_test[j,16] *
#           jointprobtemp_test[j,18] *  jointprobtemp_test[j,20] *
#           jointprobtemp_test[j,30]  *
#           jointprobtemp_test[j,31]
#           ( 1 /jointprobtemp_test[j,31] * weightSig )
        
        jointprobtemp_test[j,33] <-  Upcandidates_test[[i]][j]   
        
      }
      
      
      jointprob_test[[length(jointprob_test)+1]] <-  jointprobtemp_test
      jointprobtemp_test <- data.frame()
      
      idxjointprob_test[k] <- which.max(unlist (  jointprob_test[[length(jointprob_test)]][32]) )  
      UpFinalcandidates_test[k] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[k]] ]  
      
    } 
  }
}


### performance 
#index for class 9
idxForClass9 <- which(Downheader_new[,14] == 9)
 
rm(ResultMisMatching_train, ResultMisMatching_test)



ResultMisMatching_train <- cbind(Target_baseanalysis_Jan0910_table_train[,1],Target_baseanalysis_Jan0910_table_train[,6],
      Target_baseanalysis_Jan0910_table_train[, 4], UpFinalcandidates_train)
ResultMisMatching_test<- cbind(Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1], 6],
      Target_baseanalysis_Jan0910_table_test[Target_baseanalysis_Jan0910_table_test[,1], 4], UpFinalcandidates_test)


# train
ResultMisMatching_train[is.na ( ResultMisMatching_train)]  <- c(999)
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
ResultMisMatching_test[is.na ( ResultMisMatching_test)]  <- c(999)
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


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_12222014")

# # look more closely
# Upsiglist[[ idxForClass9[[16]]  ]]
# a_magdif [[ idxForClass9[[16]]  ]]
# jointprob_train[[16]]
# ResultMismatching_test
# ResultMismatching_train



## end


# Attribute_diff_train <- Attribute_diff_nonnormal_train 

# att_1 <- vector()
# att_2 <- vector()
# att_3 <- vector()
# att_4 <- vector()
# att_5 <- vector()
# att_6 <- vector()
# att_7 <- vector()
# att_8 <- vector()
# att_9 <- vector()
# 
# k<-0
# for (i in 1: length(Upsiglist_train)) {  
#   for (j in 1: length(Upcandidatesindex_train[[i]])) {  
#     k <- k+1
#     att_1[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][1] )
#     att_2[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][2] )
#     att_3[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][3] )
#     att_4[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][4] )
#     att_5[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][5] )
#     att_6[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][6] )
#     att_7[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][7] )
#     att_8[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][8] )
# #     att_9[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][9] )
#   
#   
#   }
#   att_1 <- na.omit(att_1)
#   att_2 <- na.omit(att_2)
#   att_3 <- na.omit(att_3)
#   att_4 <- na.omit(att_4)
#   att_5 <- na.omit(att_5)
#   att_6 <- na.omit(att_6)
#   att_7 <- na.omit(att_7)
#   att_8 <- na.omit(att_8)
# #   att_9 <- na.omit(att_9)
#   
#   
#   max_att_1_tr <- max(att_1)
#   max_att_2_tr <- max(att_2)
#   max_att_3_tr <- max(att_3)
#   max_att_4_tr <- max(att_4)
#   max_att_5_tr <- max(att_5)
#   max_att_6_tr <- max(att_6)
#   max_att_7_tr <- max(att_7)
#   max_att_8_tr <- max(att_8)
# #   max_att_9 <- max(att_9)
# 
#   min_att_1_tr <- min(att_1)
#   min_att_2_tr <- min(att_2)
#   min_att_3_tr <- min(att_3)
#   min_att_4_tr <- min(att_4)
#   min_att_5_tr <- min(att_5)
#   min_att_6_tr <- min(att_6)
#   min_att_7_tr <- min(att_7)
#   min_att_8_tr <- min(att_8)
# }
# 
# k<-0
# for (i in 1: length(Upsiglist_test)) {  
#   for (j in 1: length(Upcandidatesindex_test[[i]])) {  
#     k <- k+1
#     att_1[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][1] )
#     att_2[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][2] )
#     att_3[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][3] )
#     att_4[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][4] )
#     att_5[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][5] )
#     att_6[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][6] )
#     att_7[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][7] )
#     att_8[k] <- unlist( Attribute_diff_nonnormal_test[[i]][[j]][8] )
#     #     att_9[k] <- unlist( Attribute_diff_nonnormal_train[[i]][[j]][9] )
# 
#   }
#   att_1 <- na.omit(att_1)
#   att_2 <- na.omit(att_2)
#   att_3 <- na.omit(att_3)
#   att_4 <- na.omit(att_4)
#   att_5 <- na.omit(att_5)
#   att_6 <- na.omit(att_6)
#   att_7 <- na.omit(att_7)
#   att_8 <- na.omit(att_8)
#   #   att_9 <- na.omit(att_9)
#   
#   
#   max_att_1_te <- max(att_1)
#   max_att_2_te <- max(att_2)
#   max_att_3_te <- max(att_3)
#   max_att_4_te <- max(att_4)
#   max_att_5_te <- max(att_5)
#   max_att_6_te <- max(att_6)
#   max_att_7_te <- max(att_7)
#   max_att_8_te <- max(att_8)
#   #   max_att_9 <- max(att_9)
#   
#   min_att_1_te <- min(att_1)
#   min_att_2_te <- min(att_2)
#   min_att_3_te <- min(att_3)
#   min_att_4_te <- min(att_4)
#   min_att_5_te <- min(att_5)
#   min_att_6_te <- min(att_6)
#   min_att_7_te <- min(att_7)
#   min_att_8_te <- min(att_8)
# }
#    
#          
#          jointprobtemp_train[j,2] <-(approx(kernel_mat[[1]]$x, kernel_mat[[1]]$y, Attribute_diff_train_mat[[i]][[j]][1]) )$y
#          jointprobtemp_train[j,3] <-(approx(kernel_mat[[2]]$x, kernel_mat[[2]]$y, Attribute_diff_train_mat[[i]][[j]][2]) )$y
#          jointprobtemp_train[j,4] <-(approx(kernel_mat[[3]]$x, kernel_mat[[3]]$y, Attribute_diff_train_mat[[i]][[j]][3]) )$y
#          jointprobtemp_train[j,5] <-(approx(kernel_mat[[4]]$x, kernel_mat[[4]]$y, Attribute_diff_train_mat[[i]][[j]][4]) )$y
#          jointprobtemp_train[j,6] <-(approx(kernel_mat[[5]]$x, kernel_mat[[5]]$y, Attribute_diff_train_mat[[i]][[j]][5]) )$y
#          jointprobtemp_train[j,7] <-(approx(kernel_mat[[6]]$x, kernel_mat[[6]]$y, Attribute_diff_train_mat[[i]][[j]][6]) )$y
#          jointprobtemp_train[j,8] <-(approx(kernel_mat[[7]]$x, kernel_mat[[7]]$y, Attribute_diff_train_mat[[i]][[j]][7]) )$y
#          jointprobtemp_train[j,9] <-(approx(kernel_mat[[8]]$x, kernel_mat[[8]]$y, Attribute_diff_train_mat[[i]][[j]][8]) )$y
#         
# #           jointprobtemp_train[j,10] <- as.numeric (prob_train[[i]][[1]][Upcandidatesindex_train[[i]][j]] ) * weightSig  # alt 2
#          jointprobtemp_train[j,10] <- 1 / Attribute_diff_train_mat[[i]][[j]][9] * weightSig




#     Attribute_diff_train_mat[[i]][[j]][1] <- ( Attribute_diff_nonnormal_train[[i]][[j]][1] - min_train_mat[1,1] ) /  
#       ( max_train_mat[1,1] -  min_train_mat[1,1] )
#      
#     Attribute_diff_train_mat[[i]][[j]][2] <- ( Attribute_diff_nonnormal_train[[i]][[j]][2] - min_train_mat[2,1] ) / 
#       ( max_train_mat[2,1] -  min_train_mat[2,1] )
#     Attribute_diff_train_mat[[i]][[j]][3] <- ( Attribute_diff_nonnormal_train[[i]][[j]][3] - min_train_mat[3,1] ) / 
#       ( max_train_mat[3,1] -  min_train_mat[3,1] )
#     Attribute_diff_train_mat[[i]][[j]][4] <- ( Attribute_diff_nonnormal_train[[i]][[j]][4] - min_train_mat[4,1] ) / 
#       ( max_train_mat[4,1] -  min_train_mat[4,1] )
#     Attribute_diff_train_mat[[i]][[j]][5] <- ( Attribute_diff_nonnormal_train[[i]][[j]][5] - min_train_mat[5,1] ) / 
#       ( max_train_mat[5,1] -  min_train_mat[5,1] )
#     Attribute_diff_train_mat[[i]][[j]][6] <- ( Attribute_diff_nonnormal_train[[i]][[j]][6] - min_train_mat[6,1] ) / 
#       ( max_train_mat[6,1] -  min_train_mat[6,1] )
#     Attribute_diff_train_mat[[i]][[j]][7] <- ( Attribute_diff_nonnormal_train[[i]][[j]][7] - min_train_mat[7,1] ) / 
#       ( max_train_mat[7,1] -  min_train_mat[7,1] )
#     Attribute_diff_train_mat[[i]][[j]][8] <- ( Attribute_diff_nonnormal_train[[i]][[j]][8] - min_train_mat[8,1] ) / 
#       ( max_train_mat[8,1] -  min_train_mat[8,1] )

# 
# Attribute_diff_test_mat <- Attribute_diff_nonnormal_test 
# for (i in 1: length(Upsiglist_test)) {  
#   for (j in 1: length(Upcandidatesindex_test[[i]])) {  
#     
#     Attribute_diff_test_mat[[i]][[j]][1] <- ( Attribute_diff_nonnormal_test[[i]][[j]][1] - min_train_mat[1,1] ) / 
#       ( max_train_mat[1,1] -  min_train_mat[1,1] )
#     Attribute_diff_test_mat[[i]][[j]][2] <- ( Attribute_diff_nonnormal_test[[i]][[j]][2] - min_train_mat[2,1] ) / 
#       ( max_train_mat[2,1] -  min_train_mat[2,1] )
#     Attribute_diff_test_mat[[i]][[j]][3] <- ( Attribute_diff_nonnormal_test[[i]][[j]][3] - min_train_mat[3,1] ) / 
#       ( max_train_mat[3,1] -  min_train_mat[3,1] )
#     Attribute_diff_test_mat[[i]][[j]][4] <- ( Attribute_diff_nonnormal_test[[i]][[j]][4] - min_train_mat[4,1] ) / 
#       ( max_train_mat[4,1] -  min_train_mat[4,1] )
#     Attribute_diff_test_mat[[i]][[j]][5] <- ( Attribute_diff_nonnormal_test[[i]][[j]][5] - min_train_mat[5,1] ) /
#       ( max_train_mat[5,1] -  min_train_mat[5,1] )
#     Attribute_diff_test_mat[[i]][[j]][6] <- ( Attribute_diff_nonnormal_test[[i]][[j]][6] - min_train_mat[6,1] ) /
#       ( max_train_mat[6,1] -  min_train_mat[6,1] )
#     Attribute_diff_test_mat[[i]][[j]][7] <- ( Attribute_diff_nonnormal_test[[i]][[j]][7] - min_train_mat[7,1] ) /  
#       ( max_train_mat[7,1] -  min_train_mat[7,1] )
#     Attribute_diff_test_mat[[i]][[j]][8] <- ( Attribute_diff_nonnormal_test[[i]][[j]][8] - min_train_mat[8,1] ) / 
#       ( max_train_mat[8,1] -  min_train_mat[8,1] )
#     #     Attribute_diff_test[[i]][[j]][9] <- sub_all_test[ match( as.numeric(Upcandidates_test[[i]][j]), 
#     #                                                              as.numeric( sub_all_test[,6]) ), 3]  
#     Attribute_diff_test_mat[[i]][[j]][9] <- a_magdif_test[[i]][[j]]
#   }
#   
# }
# 
# 
# 
# 
# 
# for (i in 1: length(Upsiglist_train)) {  
#   for (j in 1: length(Upcandidatesindex_train[[i]])) {  
#     
#     Attribute_diff_train_nonmat[[i]][[j]][1] <- ( Attribute_diff_nonnormal_train[[i]][[j]][1] - min_train_nonmat[1,1] ) / 
#       ( max_train_nonmat[1,1] -  min_train_nonmat[1,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][2] <- ( Attribute_diff_nonnormal_train[[i]][[j]][2] - min_train_nonmat[2,1] ) / 
#       ( max_train_mat[2,1] -  min_train_nonmat[2,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][3] <- ( Attribute_diff_nonnormal_train[[i]][[j]][3] - min_train_nonmat[3,1] ) / 
#       ( max_train_nonmat[3,1] -  min_train_nonmat[3,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][4] <- ( Attribute_diff_nonnormal_train[[i]][[j]][4] - min_train_nonmat[4,1] ) / 
#       ( max_train_nonmat[4,1] -  min_train_nonmat[4,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][5] <- ( Attribute_diff_nonnormal_train[[i]][[j]][5] - min_train_nonmat[5,1] ) / 
#       ( max_train_nonmat[5,1] -  min_train_nonmat[5,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][6] <- ( Attribute_diff_nonnormal_train[[i]][[j]][6] - min_train_nonmat[6,1] ) / 
#       ( max_train_nonmat[6,1] -  min_train_nonmat[6,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][7] <- ( Attribute_diff_nonnormal_train[[i]][[j]][7] - min_train_nonmat[7,1] ) / 
#       ( max_train_nonmat[7,1] -  min_train_nonmat[7,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][8] <- ( Attribute_diff_nonnormal_train[[i]][[j]][8] - min_train_nonmat[8,1] ) / 
#       ( max_train_nonmat[8,1] -  min_train_nonmat[8,1] )
#     Attribute_diff_train_nonmat[[i]][[j]][9] <- a_magdif_train[[i]][[j]]  
#     
#   }
# }
# 
# Attribute_diff_test_nonmat <- Attribute_diff_nonnormal_test 
# for (i in 1: length(Upsiglist_test)) {  
#   for (j in 1: length(Upcandidatesindex_test[[i]])) {  
#     
#     Attribute_diff_test_nonmat[[i]][[j]][1] <- ( Attribute_diff_nonnormal_test[[i]][[j]][1] - min_train_nonmat[1,1] ) / 
#       ( max_train_nonmat[1,1] -  min_train_nonmat[1,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][2] <- ( Attribute_diff_nonnormal_test[[i]][[j]][2] - min_train_nonmat[2,1] ) / 
#       ( max_train_nonmat[2,1] -  min_train_nonmat[2,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][3] <- ( Attribute_diff_nonnormal_test[[i]][[j]][3] - min_train_nonmat[3,1] ) / 
#       ( max_train_nonmat[3,1] -  min_train_nonmat[3,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][4] <- ( Attribute_diff_nonnormal_test[[i]][[j]][4] - min_train_nonmat[4,1] ) / 
#       ( max_train_nonmat[4,1] -  min_train_nonmat[4,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][5] <- ( Attribute_diff_nonnormal_test[[i]][[j]][5] - min_train_nonmat[5,1] ) /
#       ( max_train_nonmat[5,1] -  min_train_nonmat[5,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][6] <- ( Attribute_diff_nonnormal_test[[i]][[j]][6] - min_train_nonmat[6,1] ) /
#       ( max_train_nonmat[6,1] -  min_train_nonmat[6,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][7] <- ( Attribute_diff_nonnormal_test[[i]][[j]][7] - min_train_nonmat[7,1] ) /  
#       ( max_train_nonmat[7,1] -  min_train_nonmat[7,1] )
#     Attribute_diff_test_nonmat[[i]][[j]][8] <- ( Attribute_diff_nonnormal_test[[i]][[j]][8] - min_train_nonmat[8,1] ) / 
#       ( max_train_nonmat[8,1] -  min_train_nonmat[8,1] )
#     #     Attribute_diff_test[[i]][[j]][9] <- sub_all_test[ match( as.numeric(Upcandidates_test[[i]][j]), 
#     #                                                              as.numeric( sub_all_test[,6]) ), 3]  
#     Attribute_diff_test_nonmat[[i]][[j]][9] <- a_magdif_test[[i]][[j]]
#   }
# }