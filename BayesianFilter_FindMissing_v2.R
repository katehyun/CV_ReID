# find missing
Upcandidates_attribute_train_missing <- data.frame()
Attribute_difftemp_train_missing <- list()
Attribute_diff_nonnormal_train_missing <- list()

Upcandidates_attribute_test_missing <- data.frame()
Attribute_difftemp_test_missing <- list()
Attribute_diff_nonnormal_test_missing <- list()

# train 
j <- 0

# for (i in 1: length(TargetTable_train[,3])) {  
#   
#   
#   Upcandidates_attribute_train_missing[i,1] <-      
#     abs( Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),15] -
#            Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),15] )  / max_train[1]
#   
#   
#   Upcandidates_attribute_train_missing[i,2]  <-    
#     abs( Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),16] -
#            Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),16])  / max_train[2]
#   
#   
#   Upcandidates_attribute_train_missing[i,3]  <-    
#     abs( Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),17] -
#            Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),17])   / max_train[3]
#   
#   
#   Upcandidates_attribute_train_missing[i,4]  <-    
#     abs( Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),18] -
#            Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),18])   / max_train[4]
#   
#   
#   Upcandidates_attribute_train_missing[i,5]  <-    
#     abs(  Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),19] -
#             Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),19])  / max_train[5]
#   
#   Upcandidates_attribute_train_missing[i,6]  <-    
#     abs(  Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),20] -
#             Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])),20])  / max_train[6]
#   
#   Upcandidates_attribute_train_missing[i,7] <-
#     abs(  Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),7] - 
#             Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])), 7])  / max_train[7]
#   
#   Upcandidates_attribute_train_missing[i,8] <-
#     abs( Upheader_new_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric(Upheader_new_train[,13])),12] - 
#            Downheader_new[ match( as.numeric(TargetTable_train[i,2]), as.numeric( Downheader_new[,13])), 12])  / max_train[8]
#   
#   Upcandidates_attribute_train_missing[i,9] <-
#     sub_all_train[ match( as.numeric(TargetTable_train[i,3]), as.numeric( sub_all_train[,6])),3]  / max_train[9]
#   
#   
# }
k <- 0
Upcandidates_attribute_train_missing <- data.frame()
for (i in 1: length(Attribute_diff_train)) {  
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
      k <- k+1
      
      for (j in 1:length(Attribute_diff_train[[1]][[1]] ) ) 
      Upcandidates_attribute_train_missing[k,j] <-  Attribute_diff_train[[i]][[ idxjointprob_train[k] ]] [j] 
    }
}

Upcandidates_attribute_test_missing <- data.frame()
for (i in 1: length(Attribute_diff_test)) {  
  
  Upcandidates_attribute_test_missing <-  Attribute_diff_test[[i]][ idxjointprob_test[i] ]  
}


# # test
# j <- 0
# 
# for (i in 1: length(TargetTable_test[,3])) {  
#   
#   
#   Upcandidates_attribute_test_missing[i,1] <-      
#     abs( Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),15] -
#            Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),15] ) / max_test[1]
#   
#   
#   Upcandidates_attribute_test_missing[i,2]  <-    
#     abs( Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),16] -
#            Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),16])  / max_test[2]
#   
#   
#   Upcandidates_attribute_test_missing[i,3]  <-    
#     abs( Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),17] -
#            Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),17])  / max_test[3]
#   
#   
#   Upcandidates_attribute_test_missing[i,4]  <-    
#     abs( Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),18] -
#            Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),18])  / max_test[4]
#   
#   
#   Upcandidates_attribute_test_missing[i,5]  <-    
#     abs(  Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),19] -
#             Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),19])  / max_test[5]
#   
#   Upcandidates_attribute_test_missing[i,6]  <-    
#     abs(  Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),20] -
#             Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])),20])  / max_test[6]
#   
#   Upcandidates_attribute_test_missing[i,7] <-
#     abs(  Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),7] - 
#             Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])), 7])  / max_test[7]
#   
#   Upcandidates_attribute_test_missing[i,8] <-
#     abs( Upheader_new_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric(Upheader_new_test[,13])),12] - 
#            Downheader_new[ match( as.numeric(TargetTable_test[i,2]), as.numeric( Downheader_new[,13])), 12]) / max_test[8]
#   
#   Upcandidates_attribute_test_missing[i,9] <-
#     sub_all_test[ match( as.numeric(TargetTable_test[i,3]), as.numeric( sub_all_test[,6])),3]  / max_test[9]
#   
#   
# }

# Only Class 9
jointprob_matching_train <- data.frame()
jointprob_missing_train <- data.frame()

jointprob_matching_test <- data.frame()
jointprob_missing_test <- data.frame()


j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_train_missing[,1])){
  #   if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
  
  
  j <- j+1
  
  
  
  jointprob_matching_train[j,1] <-(approx(kernel_mat[[1]]$x, kernel_mat[[1]]$y,  Upcandidates_attribute_train_missing[i,1]) )$y
  jointprob_matching_train[j,2] <-(approx(kernel_mat[[2]]$x, kernel_mat[[2]]$y,  Upcandidates_attribute_train_missing[i,2]) )$y
  jointprob_matching_train[j,3] <-(approx(kernel_mat[[3]]$x, kernel_mat[[3]]$y,  Upcandidates_attribute_train_missing[i,3]) )$y
  jointprob_matching_train[j,4] <-(approx(kernel_mat[[4]]$x, kernel_mat[[4]]$y,  Upcandidates_attribute_train_missing[i,4]) )$y
  jointprob_matching_train[j,5] <-(approx(kernel_mat[[5]]$x, kernel_mat[[5]]$y,  Upcandidates_attribute_train_missing[i,5]) )$y
  jointprob_matching_train[j,6] <-(approx(kernel_mat[[6]]$x, kernel_mat[[6]]$y,  Upcandidates_attribute_train_missing[i,6]) )$y
  jointprob_matching_train[j,7] <-(approx(kernel_mat[[7]]$x, kernel_mat[[7]]$y,  Upcandidates_attribute_train_missing[i,7]) )$y
  jointprob_matching_train[j,8] <-(approx(kernel_mat[[8]]$x, kernel_mat[[8]]$y,  Upcandidates_attribute_train_missing[i,8]) )$y
  jointprob_matching_train[j,9] <-(approx(kernel_mat[[9]]$x, kernel_mat[[9]]$y,  Upcandidates_attribute_train_missing[i,9]) )$y
  Attribute_diff_train[[i]][[j]][9] <- a_magdif_train[[i]][[j]]  Upcandidates_attribute_test_missing[[i]]
  
  jointprob_matching_train [is.na(jointprob_matching_train )] <- buf 
  jointprob_matching_train[j,10] <- jointprob_matching_train[j,1]* jointprob_matching_train[j,2] * jointprob_matching_train[j,3] *
    jointprob_matching_train[j,4] *  jointprob_matching_train[j,5] * jointprob_matching_train[j,6] *
    jointprob_matching_train[j,7] * jointprob_matching_train[j,8] * jointprob_matching_train[j,9] 
  
  
  k <- k+1
  jointprob_missing_train[k,1] <-(approx(kernel_nonmat[[1]]$x, kernel_nonmat[[1]]$y,  Upcandidates_attribute_train_missing[i,1]) )$y
  jointprob_missing_train[k,2] <-(approx(kernel_nonmat[[2]]$x, kernel_nonmat[[2]]$y,  Upcandidates_attribute_train_missing[i,2]) )$y
  jointprob_missing_train[k,3] <-(approx(kernel_nonmat[[3]]$x, kernel_nonmat[[3]]$y,  Upcandidates_attribute_train_missing[i,3]) )$y
  jointprob_missing_train[k,4] <-(approx(kernel_nonmat[[4]]$x, kernel_nonmat[[4]]$y,  Upcandidates_attribute_train_missing[i,4]) )$y
  jointprob_missing_train[k,5] <-(approx(kernel_nonmat[[5]]$x, kernel_nonmat[[5]]$y,  Upcandidates_attribute_train_missing[i,5]) )$y
  jointprob_missing_train[k,6] <-(approx(kernel_nonmat[[6]]$x, kernel_nonmat[[6]]$y,  Upcandidates_attribute_train_missing[i,6]) )$y
  jointprob_missing_train[k,7] <-(approx(kernel_nonmat[[7]]$x, kernel_nonmat[[7]]$y,  Upcandidates_attribute_train_missing[i,7]) )$y
  jointprob_missing_train[k,8] <-(approx(kernel_nonmat[[8]]$x, kernel_nonmat[[8]]$y,  Upcandidates_attribute_train_missing[i,8]) )$y
  jointprob_missing_train[k,9] <-(approx(kernel_nonmat[[9]]$x, kernel_nonmat[[9]]$y,  Upcandidates_attribute_train_missing[i,9]) )$y
  
  jointprob_missing_train [is.na(jointprob_missing_train )] <- buf 
  jointprob_missing_train[k,10] <- jointprob_missing_train[k,1]* jointprob_missing_train[k,2] * jointprob_missing_train[k,3] *
    jointprob_missing_train[k,4] *  jointprob_missing_train[k,5] * jointprob_missing_train[k,6] *
    jointprob_missing_train[k,7] * jointprob_missing_train[k,8] * jointprob_missing_train[k,9] 
  
  #     }
  
}

j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_test_missing[,1])){
  #   if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
  
  
  j <- j+1
  
  jointprob_matching_test[j,1] <-(approx(kernel_mat[[1]]$x, kernel_mat[[1]]$y,  Upcandidates_attribute_test_missing[i,1]) )$y
  jointprob_matching_test[j,2] <-(approx(kernel_mat[[2]]$x, kernel_mat[[2]]$y,  Upcandidates_attribute_test_missing[i,2]) )$y
  jointprob_matching_test[j,3] <-(approx(kernel_mat[[3]]$x, kernel_mat[[3]]$y,  Upcandidates_attribute_test_missing[i,3]) )$y
  jointprob_matching_test[j,4] <-(approx(kernel_mat[[4]]$x, kernel_mat[[4]]$y,  Upcandidates_attribute_test_missing[i,4]) )$y
  jointprob_matching_test[j,5] <-(approx(kernel_mat[[5]]$x, kernel_mat[[5]]$y,  Upcandidates_attribute_test_missing[i,5]) )$y
  jointprob_matching_test[j,6] <-(approx(kernel_mat[[6]]$x, kernel_mat[[6]]$y,  Upcandidates_attribute_test_missing[i,6]) )$y
  jointprob_matching_test[j,7] <-(approx(kernel_mat[[7]]$x, kernel_mat[[7]]$y,  Upcandidates_attribute_test_missing[i,7]) )$y
  jointprob_matching_test[j,8] <-(approx(kernel_mat[[8]]$x, kernel_mat[[8]]$y,  Upcandidates_attribute_test_missing[i,8]) )$y
  jointprob_matching_test[j,9] <-(approx(kernel_mat[[9]]$x, kernel_mat[[9]]$y,  Upcandidates_attribute_test_missing[i,9]) )$y
  
  jointprob_matching_test [is.na(jointprob_matching_test )] <- buf 
  jointprob_matching_test[j,10] <- jointprob_matching_test[j,1]* jointprob_matching_test[j,2] * jointprob_matching_test[j,3] *
    jointprob_matching_test[j,4] *  jointprob_matching_test[j,5] * jointprob_matching_test[j,6] *
    jointprob_matching_test[j,7] * jointprob_matching_test[j,8] * jointprob_matching_test[j,9] 
  
  
  
  k <- k+1
  
  jointprob_missing_test[k,1] <-(approx(kernel_nonmat[[1]]$x, kernel_nonmat[[1]]$y,  Upcandidates_attribute_test_missing[i,1]) )$y
  jointprob_missing_test[k,2] <-(approx(kernel_nonmat[[2]]$x, kernel_nonmat[[2]]$y,  Upcandidates_attribute_test_missing[i,2]) )$y
  jointprob_missing_test[k,3] <-(approx(kernel_nonmat[[3]]$x, kernel_nonmat[[3]]$y,  Upcandidates_attribute_test_missing[i,3]) )$y
  jointprob_missing_test[k,4] <-(approx(kernel_nonmat[[4]]$x, kernel_nonmat[[4]]$y,  Upcandidates_attribute_test_missing[i,4]) )$y
  jointprob_missing_test[k,5] <-(approx(kernel_nonmat[[5]]$x, kernel_nonmat[[5]]$y,  Upcandidates_attribute_test_missing[i,5]) )$y
  jointprob_missing_test[k,6] <-(approx(kernel_nonmat[[6]]$x, kernel_nonmat[[6]]$y,  Upcandidates_attribute_test_missing[i,6]) )$y
  jointprob_missing_test[k,7] <-(approx(kernel_nonmat[[7]]$x, kernel_nonmat[[7]]$y,  Upcandidates_attribute_test_missing[i,7]) )$y
  jointprob_missing_test[k,8] <-(approx(kernel_nonmat[[8]]$x, kernel_nonmat[[8]]$y,  Upcandidates_attribute_test_missing[i,8]) )$y
  jointprob_missing_test[k,9] <-(approx(kernel_nonmat[[9]]$x, kernel_nonmat[[9]]$y,  Upcandidates_attribute_test_missing[i,9]) )$y
  
  jointprob_missing_test [is.na(jointprob_missing_test )] <- buf 
  jointprob_missing_test[k,10] <- jointprob_missing_test[k,1]* jointprob_missing_test[k,2] * jointprob_missing_test[k,3] *
    jointprob_missing_test[k,4] *  jointprob_missing_test[k,5] * jointprob_missing_test[k,6] *
    jointprob_missing_test[k,7] * jointprob_missing_test[k,8] * jointprob_missing_test[k,9] 
  
  #   }
  
}



TargetTable_train <- TargetTable_train[,-4:-65]
TargetTable_train <- cbind(TargetTable_train, NA )

for (i in 1:length(jointprob_matching_train[,1])){
  
  if ( jointprob_matching_train[i,10] >  jointprob_missing_train[i,10]) {
    TargetTable_train[i,4] <-TargetTable_train[i,3] 
  }
  else {
    TargetTable_train[i,4] <-  999
  }
}


TargetTable_test <- TargetTable_test[,-4:-65]
TargetTable_test <- cbind(TargetTable_test, NA )

for (i in 1:length(jointprob_matching_test[,1])){
  
  if ( jointprob_matching_test[i,10] >  jointprob_missing_test[i,10]) {
    TargetTable_test[i,4] <-TargetTable_test[i,3] 
  }
  else {
    TargetTable_test[i,4] <-  999
  }
}


# performance
Target_obj_train  <- TargetTable_train[,1]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

matching_NN_train <- sum ( as.numeric ((TargetTable_train [,1]) == as.numeric (TargetTable_train [,4])) &
                             as.numeric (TargetTable_train [,1]) != 999)

missing_NN_train <- sum ( as.numeric (TargetTable_train[,4]) == c(999))

CMVeh_train <-  matching_NN_train[1]
CVeh_train <- matching_obj_train[1]
p <- 4
MVeh_train <- sum(   (as.numeric( TargetTable_train[,p])) > 1000 )  

SIMR_train <- CMVeh_train / CVeh_train
SCMR_train <- CMVeh_train / MVeh_train

MMVeh_train <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                               !=  as.numeric( TargetTable_train[,p])   ))


Veh_train <- length(TargetTable_train[,1])
SER_train <- MMVeh_train / Veh_train

ResultMismissing_train <- data.frame( matching_obj_train[1], missing_obj_train[1],              
                                      matching_NN_train[[1]],  missing_NN_train[[1]],
                                      CMVeh_train[[1]], CVeh_train[[1]], MVeh_train[[1]],
                                      SIMR_train[[1]], SCMR_train[[1]], MMVeh_train[[1]], Veh_train[[1]], SER_train[[1]] )


Target_obj_test  <- TargetTable_test[,1]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

matching_NN_test <- sum ( as.numeric ((TargetTable_test [,1]) == as.numeric (TargetTable_test [,4])) &
                            as.numeric (TargetTable_test [,1]) != 999)

missing_NN_test <- sum ( as.numeric (TargetTable_test[,4]) == c(999))

CMVeh_test <-  matching_NN_test[1]
CVeh_test <- matching_obj_test[1]
p <- 4
MVeh_test <- sum(   (as.numeric( TargetTable_test[,p])) > 1000 )  

SIMR_test <- CMVeh_test / CVeh_test
SCMR_test <- CMVeh_test / MVeh_test

MMVeh_test <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                              !=  as.numeric( TargetTable_test[,p])   ))


Veh_test <- length(TargetTable_test[,1])
SER_test <- MMVeh_test / Veh_test

ResultMismissing_test <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                     matching_NN_test[[1]],  missing_NN_test[[1]],
                                     CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                     SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )


## end


# rm(sub_all_v2, aub_all_diff)
# 
# 
# 
# 
# UpFinalcandidates_train <-as.numeric (unlist(UpFinalcandidates_train) )
# UpFinalcandidates_test <-as.numeric (unlist(UpFinalcandidates_test) )
# Target_baseanalysis_Jan0910_table_class9 <- subset( Target_baseanalysis_Jan0910_table, 
#                            as.numeric (Target_baseanalysis_Jan0910_table[,1]) == 9  )
# 
# sub_all_v2 <- Target_baseanalysis_Jan0910_table_class9[,1:4]
# sub_all_v2 <- cbind(sub_all_v2,Target_baseanalysis_Jan0910_table_class9[,6])
# sub_all_v2 <- cbind(sub_all_v2,UpFinalcandidates)
# sub_all_v2 <- cbind( sub_all_v2,      
#                      Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),13:20] ,
#                      Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),7] ,
#                      Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),12] ,
#                      Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),13:20] , # should be 6?
#                      Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),7] ,
#                      Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),12] ) 
# sub_all_v2 <- na.omit(sub_all_v2)
# 
# 
# ### approaximation
# 
# sub_all_diff <- abs(sub_all_v2[,9:16] - sub_all_v2[,19:26])
# sub_all_diff <- cbind(sub_all_diff, sub_all_v2[,3])
# 
# #normalize
# sub_all_diff [,1] <- sub_all_diff [,1]/maxlength
# sub_all_diff [,2] <- sub_all_diff [,2]/maxgvw
# sub_all_diff [,3] <- sub_all_diff [,3]/maxax12sp
# sub_all_diff [,4] <- sub_all_diff [,4]/maxax23sp
# sub_all_diff [,5] <- sub_all_diff [,5]/maxax34sp
# sub_all_diff [,6] <- sub_all_diff [,6]/maxax45sp
# sub_all_diff [,7] <- sub_all_diff [,7]/maxdur
# sub_all_diff [,8] <- sub_all_diff [,8]/maxutc
# sub_all_diff [,9] <- sub_all_diff [,9]/maxmag
# 
# colnames(sub_all_diff)[1:9] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc", "mag")
# 
# 
# q=9
# sub_all_diff[,q+1] <-(approx(kernel.length_m$x, kernel.length_m$y, sub_all_diff[,1]) )$y
# sub_all_diff[,q+2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, sub_all_diff[,2]) )$y
# sub_all_diff[,q+3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,3]) )$y
# sub_all_diff[,q+4] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,4]) )$y
# sub_all_diff[,q+5] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,5]) )$y
# sub_all_diff[,q+6] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,6]) )$y
# sub_all_diff[,q+7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, sub_all_diff[,7]) )$y
# sub_all_diff[,q+8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, sub_all_diff[,8]) )$y
# sub_all_diff[,q+9] <-(approx(kernel.magdif_m$x, kernel.magdif_m$y, sub_all_diff[,9]) )$y
# 
# colnames(sub_all_diff)[10:18] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc", "mag")
# 
# 
# qq=9+9
# sub_all_diff[,qq+1] <-(approx(kernel.length_nm$x, kernel.length_nm$y, sub_all_diff[,1]) )$y
# sub_all_diff[,qq+2] <-(approx(kernel.gvw_nm$x, kernel.gvw_nm$y, sub_all_diff[,2]) )$y
# sub_all_diff[,qq+3] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,3]) )$y
# sub_all_diff[,qq+4] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,4]) )$y
# sub_all_diff[,qq+5] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,5]) )$y
# sub_all_diff[,qq+6] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,6]) )$y
# sub_all_diff[,qq+7] <-(approx(kernel.dur_nm$x, kernel.dur_nm$y, sub_all_diff[,7]) )$y
# sub_all_diff[,qq+8] <-(approx(kernel.utc_nm$x, kernel.utc_nm$y, sub_all_diff[,8]) )$y
# sub_all_diff[,qq+9] <-(approx(kernel.magdif_nm$x, kernel.magdif_nm$y, sub_all_diff[,9]) )$y
# 
# colnames(sub_all_diff)[19:27] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc", "mag")
# 
# sub_all_diff [is.na(sub_all_diff )] <- 0.000000000001
# 
# colnames(sub_all_diff)[10:18] <- c("mlength", "mgvw", "max12sp", "max23sp", "max34sp", "max45sp", 
#                                    "mduration", "mutc", "mmagdif")
# colnames(sub_all_diff)[19:27] <- c("nmlength", "nmgvw", "nmax12sp", "nmax23sp", "nmax34sp", "nmax45sp", 
#                                    "nmduration", "nmutc", "nmmagdif")
# 
# q <- 9
# weight <- 1
# sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2] *
#   sub_all_diff[,q+3] * sub_all_diff[,q+4] * sub_all_diff[,q+5] * sub_all_diff[,q+6] * 
#   sub_all_diff[,q+7] * sub_all_diff[,q+8] * weight * sub_all_diff[,q+9] * 100000000000000 # version 1 : /w distance
# # 
# # sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2]*
# #   sub_all_diff[,q+3] * sub_all_diff[,q+4]* sub_all_diff[,q+5]* sub_all_diff[,q+6]* 
# #   sub_all_diff[,q+7]* sub_all_diff[,q+8]* 100000000000000 # version 2 : /wo distance
# 
# qq <- 18
# sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
#   sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
#   sub_all_diff[,qq+7]* sub_all_diff[,qq+8]*  weight * sub_all_diff[,qq+9] * 100000000000000 # version 1 : /w distance
# 
# # sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
# #   sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
# #   sub_all_diff[,qq+7]* sub_all_diff[,qq+8] * 100000000000000 # version 2 : /wo distance
# 
# sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_v2[,length(sub_all_v2[1,])-1] > 
#   sub_all_v2[,length(sub_all_v2[1,])]
# 
# 
# col <- length(sub_all_v2[1,])
# for (i in 1: length( sub_all_v2[,col])) {
#   if (sub_all_v2[i,col] == TRUE ){
#     sub_all_v2[i,col+1] <- as.numeric( sub_all_v2[i,6]  )
#   }
#   else {
#     sub_all_v2[i,col+1] <- 999
#   } 
# }
# 
# 
# 
# # test
# 
# Target_obj <- sub_all_v2[,5]
# a_Upid_after <- sub_all_v2[,30]
# # a_Upid_after <- sub_all[,6]
# 
# 
# # Target_obj <- TargetTable9[,6]
# # # a_Upid_after <- sub_all[,30]
# # a_Upid_after <- TargetTable9[,47-20+8]
# 
# rm(TargetTable)
# TargetTable <- cbind (Target_obj , a_Upid_after )
# missing_obj <- length (Target_obj[Target_obj == 999]) 
# matching_obj <- length (Target_obj[Target_obj != 999]) 
# 
# matching_NN <- sum( Target_obj == a_Upid_after & Target_obj!= 999)
# 
# missing_NN <- sum (a_Upid_after == c(999))
# 
# CMVeh <-  matching_NN[1]
# CVeh <- matching_obj[1]
# p <- 2
# MVeh <- sum(   (as.numeric( TargetTable[,p])) > 1000 )  
# 
# SIMR <- CMVeh / CVeh
# SCMR <- CMVeh / MVeh
# 
# MMVeh <- length(  subset(TargetTable[,1], as.numeric( Target_obj ) 
#                          !=  as.numeric( TargetTable[,p])   ))
# 
# 
# Veh <- length(TargetTable[,1])
# SER <- MMVeh / Veh
# 
# Result<- data.frame( matching_obj[1], missing_obj[1],              
#                      matching_NN[[1]],  missing_NN[[1]],
#                      CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
# rm(test)
# options(digits=6)
# test <- cbind(sub_all_v2[,3], sub_all_v2[,5], sub_all_v2[,6],sub_all_v2[,27],sub_all_v2[,28] ,sub_all_v2[,30])
# test <- round(test , digits=6)
# # sub_all <- sub_all[,-27:-29]
# 
# View(Result)
#########################################end
# 
# # non-parametric approaximation
# hist(data, breaks = 5, freq = F, xlab = 'Length (ppb)', ylim = c(0, 0.5), ylab = 'Probability',
#      main = 'Histogram of Matching  Length Data with Kernel Density Plot')
# lines(density(data, na.rm = T, from = 0, to = max(data)))
# dev.off()
# 
# # histogram with normal density curve
# data.histogram = hist(data, breaks = 5, freq = F)
# data.norm = dnorm(data, mean = mean(data), sd = sd(data))
# data.ylim.normal = range(0, data.histogram$density, dnorm(data, mean = mean(data), sd = sd(data)), na.rm = T)
# hist(data, breaks = 5, freq = F, ylim = c(0, 0.5), xlab = 'length', ylab = 'Probability', 
#      main = 'Histogram of Ozone Pollution Data with Normal Density Curve')
# curve(dnorm(x, mean = mean(data), sd = sd(data)), add = T)
# 
# # histogram with  gamma  density curve
# data.histogram = hist(data, breaks = 50, freq = F)
# data.ylim.normal = range(0, data.histogram$density, dnorm(data, mean = mean(data), sd = sd(data)), na.rm = T)
# hist(data, breaks = 15, freq = F, ylim = c(0, 0.1), xlab = 'length', ylab = 'Probability', 
#      main = 'Histogram of Ozone Pollution Data with Gamma Density Curve')
# curve(dgamma(x, shape = mean(data)^2/var(data), scale = var(data)/mean(data)), add = T)

###############

