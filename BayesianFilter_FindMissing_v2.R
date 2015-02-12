
weightSig <- 1
weightGVW <- 1


# find missing
Upcandidates_attribute_train_missing <- data.frame()
Attribute_difftemp_train_missing <- list()
Attribute_diff_nonnormal_train_missing <- list()

Upcandidates_attribute_test_missing <- data.frame()
Attribute_difftemp_test_missing <- list()
Attribute_diff_nonnormal_test_missing <- list()


j <- 0
k <- 0
m <- 0

Upcandidates_attribute_train_missing_mat <- data.frame()

for (i in 1: length(Upcandidates_train)) {  
  
 
  if ( as.numeric (Downtarget_attributes_train[i,2] ) == 9 ) {  
   
    k <- k+1  
    
      for (j in 1:length(max_train_mat) ) { 
        
        if (idxjointprob_train[k] != 999) {
      Upcandidates_attribute_train_missing_mat[k,j] <-  Attribute_diff_train_mat[[i]][[ idxjointprob_train[k] ]] [j] 
        }
      
      else {
        Upcandidates_attribute_train_missing_mat[k,j] <- NA
        
      }
      
      }
    
#     if (Upcandidates_attribute_train_missing_mat[k,31] == 99999)
#       Upcandidates_attribute_train_missing_mat[k,31] <- NA

    }
}

 maxsig_tr_mat <- na.omit(Upcandidates_attribute_train_missing_mat[,31])
 max_sig_tr_mat <- max(maxsig_tr_mat)


j <- 0
k <- 0
m <- 0

Upcandidates_attribute_train_missing_nonmat <- data.frame()

for (i in 1: length(Upcandidates_train)) {  
  
  
  if ( as.numeric (Downtarget_attributes_train[i,2] ) == 9 ) {  
    
    k <- k+1  
    
    for (j in 1:length(max_train_nonmat) ) { 
      
      if (idxjointprob_train[k] != 999) {
        Upcandidates_attribute_train_missing_nonmat[k,j] <-  Attribute_diff_train_nonmat[[i]][[ idxjointprob_train[k] ]] [j] 
      }
      
      else {
        Upcandidates_attribute_train_missing_nonmat[k,j] <- NA
        
      }
      
    }
    
    #     if (Upcandidates_attribute_train_missing_nonmat[k,31] == 99999)
    #       Upcandidates_attribute_train_missing_nonmat[k,31] <- NA
    
  }
}

maxsig_tr_nonmat <- na.omit(Upcandidates_attribute_train_missing_nonmat[,31])
max_sig_tr_nonmat <- max(maxsig_tr_nonmat)



j <- 0
k <- 0
m <- 0

Upcandidates_attribute_test_missing_mat <- data.frame()

for (i in 1: length(Upcandidates_test)) {  
  
  
  if ( as.numeric (Downtarget_attributes_test[i,2] ) == 9 ) {  
    
    k <- k+1  
    
    for (j in 1:length(max_train_mat) ) { 
      
      if (idxjointprob_test[k] != 999) {
        Upcandidates_attribute_test_missing_mat[k,j] <-  Attribute_diff_test_mat[[i]][[ idxjointprob_test[k] ]] [j] 
      }
      
      else {
        Upcandidates_attribute_test_missing_mat[k,j] <- NA
        
      }
      
    }
    
    #     if (Upcandidates_attribute_test_missing_mat[k,31] == 99999)
    #       Upcandidates_attribute_test_missing_mat[k,31] <- NA
    
  }
}

maxsig_te_mat <- na.omit(Upcandidates_attribute_test_missing_mat[,31])
max_sig_te_mat <- max(maxsig_te_mat)


j <- 0
k <- 0
m <- 0

Upcandidates_attribute_test_missing_nonmat <- data.frame()

for (i in 1: length(Upcandidates_test)) {  
  
  
  if ( as.numeric (Downtarget_attributes_test[i,2] ) == 9 ) {  
    
    k <- k+1  
    
    for (j in 1:length(max_train_nonmat) ) { 
      
      if (idxjointprob_test[k] != 999) {
        Upcandidates_attribute_test_missing_nonmat[k,j] <-  Attribute_diff_test_nonmat[[i]][[ idxjointprob_test[k] ]] [j] 
      }
      
      else {
        Upcandidates_attribute_test_missing_nonmat[k,j] <- NA
        
      }
      
    }
    
    #     if (Upcandidates_attribute_test_missing_nonmat[k,31] == 99999)
    #       Upcandidates_attribute_test_missing_nonmat[k,31] <- NA
    
  }
}

maxsig_te_nonmat <- na.omit(Upcandidates_attribute_test_missing_nonmat[,31])
max_sig_te_nonmat <- max(maxsig_te_nonmat)




# Only Class 9
jointprob_matching_train <- data.frame()
jointprob_missing_train <- data.frame()

jointprob_nonmatching_train <- data.frame()
jointprob_nonmissing_test <- data.frame()


j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_train_missing_mat[,1])){
#  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
  
  
  for (m in 1: 31) { 
    
    # option 1 - non parametric
#     if ( m %in% class9idxprob) {
#       jointprob_matching_train[i,m] <- as.numeric ( approx( kernel_mat[[m]]$x, kernel_mat[[m]]$y,
#                                                              Upcandidates_attribute_train_missing_mat[i,m] )$y )
#     }
      
    # option 2 - parametric
    if ( m %in% class9idxprob) {
      jointprob_matching_train[i,m]  <- as.numeric ( approx( kernel_para_mat[[m]]$x, kernel_para_mat[[m]]$y,
                                                        Upcandidates_attribute_train_missing_mat[i,m] )$y ) 
    }
    
    else {
      jointprob_matching_train[i,m] <- 99999
    }
    
  }
  
#   jointprob_matching_train[i,31] <- (Upcandidates_attribute_train_missing_mat[i,31]/max_sig_tr_mat ) * weightSig


  
  jointprob_matching_train [is.na(jointprob_matching_train)] <- buf 
  
  jointprob_matching_train[i,32] <-  jointprob_matching_train[i,1] * jointprob_matching_train[i,2] * jointprob_matching_train[i,3] *  jointprob_matching_train[i,4] * 
    jointprob_matching_train[i,5] * jointprob_matching_train[i,6] * jointprob_matching_train[i,7] *  jointprob_matching_train[i,12] * 
    jointprob_matching_train[i,13] * jointprob_matching_train[i,14] *  jointprob_matching_train[i,15] * jointprob_matching_train[i,16] *
    jointprob_matching_train[i,17] * jointprob_matching_train[i,18] *  jointprob_matching_train[i,19] *  jointprob_matching_train[i,20] *
    jointprob_matching_train[i,21] *   jointprob_matching_train[i,30]  *
    jointprob_matching_train[i,31] 
  
}


j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_test_missing_mat[,1])){
  #  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
  
  
  for (m in 1: 31) { 
    
#     # option 1 - nonparametric
#     if ( m %in% class9idxprob) {
#       jointprob_matching_test[i,m] <- as.numeric ( approx( kernel_mat[[m]]$x, kernel_mat[[m]]$y,
#                                                            Upcandidates_attribute_test_missing_mat[i,m] )$y )
#     }
    
    # option 2 - parametric
    if ( m %in% class9idxprob) {
      jointprob_matching_test[i,m] <- as.numeric  (approx( kernel_para_mat[[m]]$x, kernel_para_mat[[m]]$y,
                                                        Upcandidates_attribute_test_missing_mat[i,m] )$y )
    }
    
    else {
      jointprob_matching_test[i,m] <- 99999
    }
    
  }
  
#   jointprob_matching_test[i,31] <- (Upcandidates_attribute_test_missing_mat[i,31]/max_sig_tr_mat ) * weightSig
  
  jointprob_matching_test [is.na(jointprob_matching_test)] <- buf 
  
  jointprob_matching_test[i,32] <-  jointprob_matching_test[i,1] * jointprob_matching_test[i,2] * jointprob_matching_test[i,3] *  jointprob_matching_test[i,4] * 
    jointprob_matching_test[i,5] * jointprob_matching_test[i,6] * jointprob_matching_test[i,7] *  jointprob_matching_test[i,12] * 
    jointprob_matching_test[i,13] * jointprob_matching_test[i,14] *  jointprob_matching_test[i,15] * jointprob_matching_test[i,16] *
    jointprob_matching_test[i,17] * jointprob_matching_test[i,18] *  jointprob_matching_test[i,19] *  jointprob_matching_test[i,20] *
    jointprob_matching_test[i,21] *   jointprob_matching_test[i,30]  *
    jointprob_matching_test[i,31] 
  
}


j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_train_missing_nonmat[,1])){
  #  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
  
  
  for (m in 1: 31) { 
    
#     # option 1 - nonparametric
#     if ( m %in% class9idxprob) {
#       jointprob_missing_train[i,m] <- as.numeric ( approx( kernel_nonmat[[m]]$x, kernel_nonmat[[m]]$y,
#                                                            Upcandidates_attribute_train_missing_nonmat[i,m] )$y )
#     }
    
    # option 2 - parametric
    if ( m %in% class9idxprob) {
      jointprob_missing_train[i,m]  <- as.numeric  (approx( kernel_para_nonmat[[m]]$x, kernel_para_nonmat[[m]]$y,
                                                             Upcandidates_attribute_train_missing_nonmat[i,m] )$y )
    }
    
    else {
      jointprob_missing_train[i,m] <- 99999
    }
    
  }
  
#   jointprob_missing_train[i,31] <- (Upcandidates_attribute_train_missing_nonmat[i,31]/max_sig_tr_nonmat ) * weightSig
  
  jointprob_missing_train [is.na(jointprob_missing_train)] <- buf 
  
  jointprob_missing_train[i,32] <-  jointprob_missing_train[i,1] * jointprob_missing_train[i,2] * jointprob_missing_train[i,3] *  jointprob_missing_train[i,4] * 
    jointprob_missing_train[i,5] * jointprob_missing_train[i,6] * jointprob_missing_train[i,7] *  jointprob_missing_train[i,12] * 
    jointprob_missing_train[i,13] * jointprob_missing_train[i,14] *  jointprob_missing_train[i,15] * jointprob_missing_train[i,16] *
    jointprob_missing_train[i,17] * jointprob_missing_train[i,18] *  jointprob_missing_train[i,19] *  jointprob_missing_train[i,20] *
    jointprob_missing_train[i,21] *   jointprob_missing_train[i,30]  *
    jointprob_missing_train[i,31] 
  
}


j <- 0
k <- 0

for (i in 1:length(Upcandidates_attribute_test_missing_nonmat[,1])){
  #  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
  
  
  for (m in 1: 31) { 
    
    # option 1 - nonparametric
#     if ( m %in% class9idxprob) {
#       jointprob_missing_test[i,m] <- as.numeric ( approx( kernel_nonmat[[m]]$x, kernel_nonmat[[m]]$y,
#                                                           Upcandidates_attribute_test_missing_nonmat[i,m] )$y )
#     }
    
    
    # option 2 - parametric
    if ( m %in% class9idxprob) {
      jointprob_missing_test[i,m]  <- as.numeric (approx( kernel_para_nonmat[[m]]$x, kernel_para_nonmat[[m]]$y,
                                                            Upcandidates_attribute_test_missing_nonmat[i,m] )$y )
    }
    
    else {
      jointprob_missing_test[i,m] <- 99999
    }
    
  }
  
#   jointprob_missing_test[i,31] <- (Upcandidates_attribute_test_missing_nonmat[i,31]/max_sig_tr_nonmat ) * weightSig
  
  jointprob_missing_test [is.na(jointprob_missing_test)] <- buf 
  
  jointprob_missing_test[i,32] <-  jointprob_missing_test[i,1] * jointprob_missing_test[i,2] * jointprob_missing_test[i,3] *  jointprob_missing_test[i,4] * 
    jointprob_missing_test[i,5]  * jointprob_missing_test[i,6] * jointprob_missing_test[i,7] *  jointprob_missing_test[i,12] * 
    jointprob_missing_test[i,13] * jointprob_missing_test[i,14] *  jointprob_missing_test[i,15] * jointprob_missing_test[i,16] *
    jointprob_missing_test[i,17] * jointprob_missing_test[i,18] *  jointprob_missing_test[i,19] *  jointprob_missing_test[i,20] *
    jointprob_missing_test[i,21] * jointprob_missing_test[i,30]  *
    jointprob_missing_test[i,31] 
  
}



# probability normalization
jointprob_matching_train_normalized <- data.frame()
jointprob_missing_train_normalized <- data.frame()

for (i in 1: length(jointprob_matching_train[,1])){
  
  for (j in 1: (length(jointprob_missing_train[1,])-1)){
 
    jointprob_matching_train_normalized[i,j] = 
      jointprob_matching_train[i,j]  / ( jointprob_matching_train[i,j] + jointprob_missing_train[i,j] )
    
    jointprob_missing_train_normalized[i,j] =
      jointprob_missing_train[i,j]   / ( jointprob_matching_train[i,j] + jointprob_missing_train[i,j] )

  }

  jointprob_matching_train_normalized[i,32] = 
    jointprob_matching_train_normalized[i,1] * jointprob_matching_train_normalized[i,2] * jointprob_matching_train_normalized[i,3] * 
    jointprob_matching_train_normalized[i,4] * jointprob_matching_train_normalized[i,5] * jointprob_matching_train_normalized[i,6] *  
    jointprob_matching_train_normalized[i,7] * jointprob_matching_train_normalized[i,12] * jointprob_matching_train_normalized[i,13] *
    jointprob_matching_train_normalized[i,14] * jointprob_matching_train_normalized[i,15] * jointprob_matching_train_normalized[i,16] *
    jointprob_matching_train_normalized[i,17] * jointprob_matching_train_normalized[i,18] * jointprob_matching_train_normalized[i,19] *
    jointprob_matching_train_normalized[i,20] * jointprob_matching_train_normalized[i,21] * jointprob_matching_train_normalized[i,30] *
    jointprob_matching_train_normalized[i,31] 
  
  jointprob_missing_train_normalized[i,32] = 
    jointprob_missing_train_normalized[i,1] * jointprob_missing_train_normalized[i,2] * jointprob_missing_train_normalized[i,3] * 
    jointprob_missing_train_normalized[i,4] * jointprob_missing_train_normalized[i,5] * jointprob_missing_train_normalized[i,6] *  
    jointprob_missing_train_normalized[i,7] * jointprob_missing_train_normalized[i,12] * jointprob_missing_train_normalized[i,13] *
    jointprob_missing_train_normalized[i,14] * jointprob_missing_train_normalized[i,15] * jointprob_missing_train_normalized[i,16] *
    jointprob_missing_train_normalized[i,17] * jointprob_missing_train_normalized[i,18] * jointprob_missing_train_normalized[i,19] *
    jointprob_missing_train_normalized[i,20] * jointprob_missing_train_normalized[i,21] * jointprob_missing_train_normalized[i,30] *
    jointprob_missing_train_normalized[i,31] 
}


jointprob_matching_test_normalized <- data.frame()
jointprob_missing_test_normalized <- data.frame()

for (i in 1: length(jointprob_matching_test[,1])){
  for (j in 1: (length(jointprob_missing_test[1,])-1)){
    
    
    jointprob_matching_test_normalized[i,j] = 
      jointprob_matching_test[i,j]  / ( jointprob_matching_test[i,j] + jointprob_missing_test[i,j] )
    
    jointprob_missing_test_normalized[i,j] =
      jointprob_missing_test[i,j]   / ( jointprob_matching_test[i,j] + jointprob_missing_test[i,j] )
    
  }
  
  jointprob_matching_test_normalized[i,32] = 
    jointprob_matching_test_normalized[i,1] * jointprob_matching_test_normalized[i,2] * jointprob_matching_test_normalized[i,3] * 
    jointprob_matching_test_normalized[i,4] * jointprob_matching_test_normalized[i,5] * jointprob_matching_test_normalized[i,6] *  
    jointprob_matching_test_normalized[i,7] * jointprob_matching_test_normalized[i,12] * jointprob_matching_test_normalized[i,13] *
    jointprob_matching_test_normalized[i,14] * jointprob_matching_test_normalized[i,15] * jointprob_matching_test_normalized[i,16] *
    jointprob_matching_test_normalized[i,17] * jointprob_matching_test_normalized[i,18] * jointprob_matching_test_normalized[i,19] *
    jointprob_matching_test_normalized[i,20] * jointprob_matching_test_normalized[i,21] * jointprob_matching_test_normalized[i,30] *
    jointprob_matching_test_normalized[i,31] 
  
  jointprob_missing_test_normalized[i,32] = 
    jointprob_missing_test_normalized[i,1] * jointprob_missing_test_normalized[i,2] * jointprob_missing_test_normalized[i,3] * 
    jointprob_missing_test_normalized[i,4] * jointprob_missing_test_normalized[i,5] * jointprob_missing_test_normalized[i,6] *  
    jointprob_missing_test_normalized[i,7] * jointprob_missing_test_normalized[i,12] * jointprob_missing_test_normalized[i,13] *
    jointprob_missing_test_normalized[i,14] * jointprob_missing_test_normalized[i,15] * jointprob_missing_test_normalized[i,16] *
    jointprob_missing_test_normalized[i,17] * jointprob_missing_test_normalized[i,18] * jointprob_missing_test_normalized[i,19] *
    jointprob_missing_test_normalized[i,20] * jointprob_missing_test_normalized[i,21] * jointprob_missing_test_normalized[i,30] *
    jointprob_missing_test_normalized[i,31] 
}


#non normalized
TargetTable_train <- TargetTable_train[,-4:-65]
TargetTable_train <- cbind(TargetTable_train, NA )

for (i in 1:length(jointprob_matching_train[,1])){
  
  if ( jointprob_matching_train[i,32] >  jointprob_missing_train[i,32]) {
    TargetTable_train[i,4] <-TargetTable_train[i,3] 
  }
  else {
    TargetTable_train[i,4] <-  999
  }
}


TargetTable_test <- TargetTable_test[,-4:-65]
TargetTable_test <- cbind(TargetTable_test, NA )

for (i in 1:length(jointprob_matching_test[,1])){
  
  
  if ( jointprob_matching_test[i,32] >  jointprob_missing_test[i,32]) {
    TargetTable_test[i,4] <- TargetTable_test[i,3] 
  }
  else {
    TargetTable_test[i,4] <-  999
  }
}


#normalized
TargetTable_train <- TargetTable_train[,-5:-65]
TargetTable_train <- cbind(TargetTable_train, NA )

for (i in 1:length(jointprob_matching_train_normalized[,1])){
  
  
  if ( jointprob_matching_train_normalized[i,32] >  jointprob_missing_train_normalized[i,32]) {
    TargetTable_train[i,5] <- TargetTable_train[i,3] 
  }
  else {
    TargetTable_train[i,5] <-  999
  }
}


TargetTable_test <- TargetTable_test[,-5:-65]
TargetTable_test <- cbind(TargetTable_test, NA )

for (i in 1:length(jointprob_matching_test_normalized[,1])){
  
  
  if ( jointprob_matching_test_normalized[i,32] >  jointprob_missing_test_normalized[i,32]) {
    TargetTable_test[i,5] <- TargetTable_test[i,3] 
  }
  else {
    TargetTable_test[i,5] <-  999
  }
}


# performance
Target_obj_train  <- TargetTable_train[,1]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

matching_NN_train <- sum ( as.numeric ((TargetTable_train [,1]) == as.numeric (TargetTable_train [,4])) &
                             as.numeric (TargetTable_train [,1]) != 999)
p <- 4
missing_NN_train <- sum ( as.numeric (TargetTable_train[,p]) == c(999))

CMVeh_train <-  matching_NN_train[1]
CVeh_train <- matching_obj_train[1]

MVeh_train <- sum(   (as.numeric( TargetTable_train[,p])) > 1000 )  

SIMR_train <- CMVeh_train / CVeh_train
SCMR_train <- CMVeh_train / MVeh_train

MMVeh_train <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                               !=  as.numeric( TargetTable_train[,p])   ))


Veh_train <- length(TargetTable_train[,1])
SER_train <- MMVeh_train / Veh_train

ResultMissing_train <- data.frame( matching_obj_train[1], missing_obj_train[1],              
                                      matching_NN_train[[1]],  missing_NN_train[[1]],
                                      CMVeh_train[[1]], CVeh_train[[1]], MVeh_train[[1]],
                                      SIMR_train[[1]], SCMR_train[[1]], MMVeh_train[[1]], Veh_train[[1]], SER_train[[1]] )

# test
Target_obj_test  <- TargetTable_test[,1]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

matching_NN_test <- sum ( as.numeric ((TargetTable_test [,1]) == as.numeric (TargetTable_test [,4])) &
                            as.numeric (TargetTable_test [,1]) != 999)

missing_NN_test <- sum ( as.numeric (TargetTable_test[,p]) == c(999))

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

ResultMissing_test <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                     matching_NN_test[[1]],  missing_NN_test[[1]],
                                     CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                     SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )


# normalized

p <- 5
missing_NN_train <- sum ( as.numeric (TargetTable_train[,p]) == c(999))

CMVeh_train <-  matching_NN_train[1]
CVeh_train <- matching_obj_train[1]

MVeh_train <- sum(   (as.numeric( TargetTable_train[,p])) > 1000 )  

SIMR_train <- CMVeh_train / CVeh_train
SCMR_train <- CMVeh_train / MVeh_train

MMVeh_train <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                               !=  as.numeric( TargetTable_train[,p])   ))


Veh_train <- length(TargetTable_train[,1])
SER_train <- MMVeh_train / Veh_train

ResultMissing_train_normalized <- data.frame( matching_obj_train[1], missing_obj_train[1],              
                                   matching_NN_train[[1]],  missing_NN_train[[1]],
                                   CMVeh_train[[1]], CVeh_train[[1]], MVeh_train[[1]],
                                   SIMR_train[[1]], SCMR_train[[1]], MMVeh_train[[1]], Veh_train[[1]], SER_train[[1]] )


missing_NN_test <- sum ( as.numeric (TargetTable_test[,p]) == c(999))

CMVeh_test <-  matching_NN_test[1]
CVeh_test <- matching_obj_test[1]

MVeh_test <- sum(   (as.numeric( TargetTable_test[,p])) > 1000 )  

SIMR_test <- CMVeh_test / CVeh_test
SCMR_test <- CMVeh_test / MVeh_test

MMVeh_test <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                              !=  as.numeric( TargetTable_test[,p])   ))


Veh_test <- length(TargetTable_test[,1])
SER_test <- MMVeh_test / Veh_test

ResultMissing_test_normalized <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                  matching_NN_test[[1]],  missing_NN_test[[1]],
                                  CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                  SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_12222014")
## end

row <- 85
objDown <- TargetTable_train[row,2]
Downheader_new [match(objDown, Downheader_new[,13]),]
objUp <- TargetTable_train[row,3]
Upheader_new [match(objUp, Upheader_new[,13]),]
