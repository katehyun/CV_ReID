library(stringr)
options(scipen=999)

w <- 0
ResultMissing_test_all_1 <- data.frame()
ResultMissing_train_all_1 <- data.frame()
ResultMissing_test_all_2 <- data.frame()
ResultMissing_train_all_2 <- data.frame()

ResultMissing_train_all <- data.frame()
ResultMissing_test_all <- data.frame()
ResultMissing_train_tt <- data.frame()
ResultMissing_test_tt <- data.frame()
ResultMissing_train_su <- data.frame()
ResultMissing_test_su <- data.frame()

weightwim <- 3
weightsig1 <- 0
weightsig2 <- 1


#  result normalization - train
for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  #         if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { 
  
  if (!is.na(jointprob_matching_train_result[[i]][1][1])) {
    
    for (j in 1: 6 ) {
      
      jointprob_matching_train_result[[i]][j,7] <- (jointprob_matching_train_result[[i]][j,1] -
                                                      ( min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) - 
            min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) )
      
      jointprob_missing_train_result[[i]][j,7] <- (jointprob_missing_train_result[[i]][j,1] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) - 
            min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) )
      
      jointprob_matching_train_result[[i]][j,8] <- (jointprob_matching_train_result[[i]][j,2] -
                                                      ( min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) - 
            min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) )
      
      jointprob_missing_train_result[[i]][j,8] <- (jointprob_missing_train_result[[i]][j,2] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) - 
            min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) )
      
      jointprob_matching_train_result[[i]][j,9] <- (jointprob_matching_train_result[[i]][j,3] -
                                                      ( min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) - 
            min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) )
      
      jointprob_missing_train_result[[i]][j,9] <- (jointprob_missing_train_result[[i]][j,3] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ) )/ 
        ( max(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) - 
            min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) )
      
      jointprob_matching_train_result[[i]][is.na( jointprob_matching_train_result[[i]])] <- 0
      jointprob_missing_train_result[[i]][is.na( jointprob_missing_train_result[[i]])] <- 0
      
      jointprob_matching_train_result[[i]][j,10] <- jointprob_matching_train_result[[i]][j,7] * weightwim +
        jointprob_matching_train_result[[i]][j,8] * weightsig1
      
      jointprob_missing_train_result[[i]][j,10] <- jointprob_missing_train_result[[i]][j,7]  * weightwim  +
        jointprob_missing_train_result[[i]][j,8] * weightsig1
      
      jointprob_matching_train_result[[i]][j,11] <- jointprob_matching_train_result[[i]][j,7] * weightwim  +
        jointprob_matching_train_result[[i]][j,9] * weightsig2
      
      jointprob_missing_train_result[[i]][j,11] <- jointprob_missing_train_result[[i]][j,7] * weightwim  +
        jointprob_missing_train_result[[i]][j,9] * weightsig2
      
      jointprob_matching_train_result[[i]][j,12] <- jointprob_matching_train_result[[i]][j,7] * weightwim  +
        jointprob_matching_train_result[[i]][j,8] * weightsig1 + jointprob_matching_train_result[[i]][j,9] * weightsig2
      
      jointprob_missing_train_result[[i]][j,12] <- jointprob_missing_train_result[[i]][j,7] * weightwim  +
        jointprob_missing_train_result[[i]][j,8] * weightsig1 + jointprob_missing_train_result[[i]][j,9] * weightsig2
      
      jointprob_matching_train_result[[i]][j,13] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 13]  
      jointprob_missing_train_result[[i]][j,13] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 13] 
      
      
    }
  } 
  #       }
}



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  #         if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { 
  
  if (!is.na(jointprob_matching_train_result_normalized[[i]][1][1])) {
    
    for (j in 1: 6 ) {
      
      jointprob_matching_train_result_normalized[[i]][j,7] <- (jointprob_matching_train_result_normalized[[i]][j,1] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) )
      
      jointprob_missing_train_result_normalized[[i]][j,7] <- (jointprob_missing_train_result_normalized[[i]][j,1] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) )
      
      jointprob_matching_train_result_normalized[[i]][j,8] <- (jointprob_matching_train_result_normalized[[i]][j,2] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) )
      
      jointprob_missing_train_result_normalized[[i]][j,8] <- (jointprob_missing_train_result_normalized[[i]][j,2] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) )
      
      jointprob_matching_train_result_normalized[[i]][j,9] <- (jointprob_matching_train_result_normalized[[i]][j,3] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) )
      
      jointprob_missing_train_result_normalized[[i]][j,9] <- (jointprob_missing_train_result_normalized[[i]][j,3] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ) )/ 
        ( max(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) - 
            min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) )
      
      jointprob_matching_train_result_normalized[[i]][is.na( jointprob_matching_train_result_normalized[[i]])] <- 0
      jointprob_missing_train_result_normalized[[i]][is.na( jointprob_missing_train_result_normalized[[i]])] <- 0
      
      jointprob_matching_train_result_normalized[[i]][j,10] <- jointprob_matching_train_result_normalized[[i]][j,7] * weightwim +
        jointprob_matching_train_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_missing_train_result_normalized[[i]][j,10] <- jointprob_missing_train_result_normalized[[i]][j,7] * weightwim +
        jointprob_missing_train_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_matching_train_result_normalized[[i]][j,11] <- jointprob_matching_train_result_normalized[[i]][j,7] * weightwim + 
        jointprob_matching_train_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_train_result_normalized[[i]][j,11] <- jointprob_missing_train_result_normalized[[i]][j,7] * weightwim  +
        jointprob_missing_train_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_train_result_normalized[[i]][j,12] <- jointprob_matching_train_result_normalized[[i]][j,7] * weightwim + 
        jointprob_matching_train_result_normalized[[i]][j,8] * weightsig1 + jointprob_matching_train_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_train_result_normalized[[i]][j,12] <- jointprob_missing_train_result_normalized[[i]][j,7] * weightwim  +
        jointprob_missing_train_result_normalized[[i]][j,8] * weightsig1 + jointprob_missing_train_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_train_result_normalized[[i]][j,13] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 13]  
      jointprob_missing_train_result_normalized[[i]][j,13] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 13]    
      
    }
    #           }
  } 
}


#  result normalization - test
for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  #         if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { 
  
  if (! is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
    
    for (j in 1: 6 ) {
      
      jointprob_matching_test_result[[i]][j,7] <- (jointprob_matching_test_result[[i]][j,1] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ))
      
      jointprob_missing_test_result[[i]][j,7] <- (jointprob_missing_test_result[[i]][j,1] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ))
      
      jointprob_matching_test_result[[i]][j,8] <- (jointprob_matching_test_result[[i]][j,2] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ))
      
      jointprob_missing_test_result[[i]][j,8] <- (jointprob_missing_test_result[[i]][j,2] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ))
      
      jointprob_matching_test_result[[i]][j,9] <- (jointprob_matching_test_result[[i]][j,3] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ))
      
      jointprob_missing_test_result[[i]][j,9] <- (jointprob_missing_test_result[[i]][j,3] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ))
      
      jointprob_matching_test_result[[i]][is.na( jointprob_matching_test_result[[i]])] <- 0
      jointprob_missing_test_result[[i]][is.na( jointprob_missing_test_result[[i]])] <- 0
      
      jointprob_matching_test_result[[i]][j,10] <- jointprob_matching_test_result[[i]][j,7] * weightwim +
        jointprob_matching_test_result[[i]][j,8] * weightsig1
      
      jointprob_missing_test_result[[i]][j,10] <- jointprob_missing_test_result[[i]][j,7] * weightwim +
        jointprob_missing_test_result[[i]][j,8] * weightsig1
      
      jointprob_matching_test_result[[i]][j,11] <- jointprob_matching_test_result[[i]][j,7] * weightwim +
        jointprob_matching_test_result[[i]][j,9]  * weightsig2
      
      jointprob_missing_test_result[[i]][j,11] <- jointprob_missing_test_result[[i]][j,7] * weightwim +
        jointprob_missing_test_result[[i]][j,9] * weightsig2
      
      jointprob_matching_test_result[[i]][j,12] <- jointprob_matching_test_result[[i]][j,7] * weightwim +
        jointprob_matching_test_result[[i]][j,8]  * weightsig1 + jointprob_matching_test_result[[i]][j,9]  * weightsig2
      
      jointprob_missing_test_result[[i]][j,12] <- jointprob_missing_test_result[[i]][j,7] * weightwim +
        jointprob_missing_test_result[[i]][j,8] * weightsig1 + jointprob_missing_test_result[[i]][j,9] * weightsig2
      
      jointprob_matching_test_result[[i]][j,13] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 13]  
      jointprob_missing_test_result[[i]][j,13] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 13] 
      
      
    }
  }
} 
#       }



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  #         if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { 
  
  if ( !is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
    
    for (j in 1:6 ) {
      
      jointprob_matching_test_result_normalized[[i]][j,7] <- (jointprob_matching_test_result_normalized[[i]][j,1] -
                                                                ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ))
      
      jointprob_missing_test_result_normalized[[i]][j,7] <- (jointprob_missing_test_result_normalized[[i]][j,1] -
                                                               ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ))
      
      jointprob_matching_test_result_normalized[[i]][j,8] <- (jointprob_matching_test_result_normalized[[i]][j,2] -
                                                                ( min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ))
      
      jointprob_missing_test_result_normalized[[i]][j,8] <- (jointprob_missing_test_result_normalized[[i]][j,2] -
                                                               ( min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ))
      
      jointprob_matching_test_result_normalized[[i]][j,9] <- (jointprob_matching_test_result_normalized[[i]][j,3] -
                                                                ( min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ))
      
      jointprob_missing_test_result_normalized[[i]][j,9] <- (jointprob_missing_test_result_normalized[[i]][j,3] -
                                                               ( min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ))
      
      jointprob_matching_test_result_normalized[[i]][is.na( jointprob_matching_test_result_normalized[[i]])] <- 0
      jointprob_missing_test_result_normalized[[i]][is.na( jointprob_missing_test_result_normalized[[i]])] <- 0
      
      jointprob_matching_test_result_normalized[[i]][j,10] <- jointprob_matching_test_result_normalized[[i]][j,7] * weightwim +
        jointprob_matching_test_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_missing_test_result_normalized[[i]][j,10] <- jointprob_missing_test_result_normalized[[i]][j,7]  * weightwim +
        jointprob_missing_test_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_matching_test_result_normalized[[i]][j,11] <- jointprob_matching_test_result_normalized[[i]][j,7] * weightwim +
        jointprob_matching_test_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_test_result_normalized[[i]][j,11] <- jointprob_missing_test_result_normalized[[i]][j,7] * weightwim +
        jointprob_missing_test_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_test_result_normalized[[i]][j,12] <- jointprob_matching_test_result_normalized[[i]][j,7] * weightwim +
        jointprob_matching_test_result_normalized[[i]][j,8] * weightsig1 +jointprob_matching_test_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_test_result_normalized[[i]][j,12] <- jointprob_missing_test_result_normalized[[i]][j,7] * weightwim +
        jointprob_missing_test_result_normalized[[i]][j,8] * weightsig1 + jointprob_missing_test_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_test_result_normalized[[i]][j,13] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 13]  
      jointprob_missing_test_result_normalized[[i]][j,13] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 13]    
      
      
    }
  }
  #         } 
}




# performance
ResultMisMatching_train <- ResultMisMatching_train_temp 
ResultMisMatching_train <-  ResultMisMatching_train[,-10:-65]
ResultMisMatching_train <- cbind( ResultMisMatching_train, NA ,NA, NA , NA , NA,  NA ,NA, NA , NA , NA ,  NA , NA )


for (i in 1:length(jointprob_matching_train_result) ){
  
  #         if ( as.numeric ( Downtarget_attributes_train [i,2] ) >= 8 ) { 
  
  
  if (!is.na(jointprob_matching_train_result[[i]][1][1])) {
    # wim only
    if ( jointprob_matching_train_result[[i]][1,7] >  jointprob_missing_train_result[[i]][1,7] ) {
      ResultMisMatching_train[i,10] <- ResultMisMatching_train[i,4] 
    }
    else {
      ResultMisMatching_train[i,10] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_train_result[[i]][2,8] >  jointprob_missing_train_result[[i]][2,8] ) {
      ResultMisMatching_train[i,11] <- ResultMisMatching_train[i,5] 
    }
    else {
      ResultMisMatching_train[i,11] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_train_result[[i]][3,9] >  jointprob_missing_train_result[[i]][3,9] ) {
      ResultMisMatching_train[i,12] <- ResultMisMatching_train[i,6] 
    }
    else {
      ResultMisMatching_train[i,12] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_train_result[[i]][4,10] >  jointprob_missing_train_result[[i]][4,10] ) {
      ResultMisMatching_train[i,13] <- ResultMisMatching_train[i,7] 
    }
    else {
      ResultMisMatching_train[i,13] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_train_result[[i]][5,11] >  jointprob_missing_train_result[[i]][5,11] ) {
      ResultMisMatching_train[i,14] <- ResultMisMatching_train[i,8] 
    }
    else {
      ResultMisMatching_train[i,14] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_train_result[[i]][6,12] >  jointprob_missing_train_result[[i]][6,12] ) {
      ResultMisMatching_train[i,15] <- ResultMisMatching_train[i,9] 
    }
    else {
      ResultMisMatching_train[i,15] <-  999
    }
    
    
    # normalization
    # wim only
    if ( jointprob_matching_train_result_normalized[[i]][1,7] >  jointprob_missing_train_result_normalized[[i]][1,7] ) {
      ResultMisMatching_train[i,16] <- ResultMisMatching_train[i,4] 
    }
    else {
      ResultMisMatching_train[i,16] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_train_result_normalized[[i]][2,8] >  jointprob_missing_train_result_normalized[[i]][2,8] ) {
      ResultMisMatching_train[i,17] <- ResultMisMatching_train[i,5] 
    }
    else {
      ResultMisMatching_train[i,17] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_train_result_normalized[[i]][3,9] >  jointprob_missing_train_result_normalized[[i]][3,9] ) {
      ResultMisMatching_train[i,18] <- ResultMisMatching_train[i,6] 
    }
    else {
      ResultMisMatching_train[i,18] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_train_result_normalized[[i]][4,10] >  jointprob_missing_train_result_normalized[[i]][4,10] ) {
      ResultMisMatching_train[i,19] <- ResultMisMatching_train[i,7] 
    }
    else {
      ResultMisMatching_train[i,19] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_train_result_normalized[[i]][5,11] >  jointprob_missing_train_result_normalized[[i]][5,11] ) {
      ResultMisMatching_train[i,20] <- ResultMisMatching_train[i,8] 
    }
    else {
      ResultMisMatching_train[i,20] <-  999
    }
    
    
    # wim + sigfeature
    if ( jointprob_matching_train_result_normalized[[i]][6,12] >  jointprob_missing_train_result_normalized[[i]][6,12] ) {
      ResultMisMatching_train[i,21] <- ResultMisMatching_train[i,9] 
    }
    else {
      ResultMisMatching_train[i,21] <-  999
    }
    
  }
  
  else{    
    
    ResultMisMatching_train[i,10] <-  999
    ResultMisMatching_train[i,11] <-  999
    ResultMisMatching_train[i,12] <-  999
    ResultMisMatching_train[i,13] <-  999
    ResultMisMatching_train[i,14] <-  999
    ResultMisMatching_train[i,15] <-  999
    ResultMisMatching_train[i,16] <-  999
    ResultMisMatching_train[i,17] <-  999
    ResultMisMatching_train[i,18] <-  999
    ResultMisMatching_train[i,19] <-  999
    ResultMisMatching_train[i,20] <-  999
    ResultMisMatching_train[i,21] <-  999
  }
  #         }
  
  #         else{    
  #         
  #           ResultMisMatching_train[i,10] <-  999
  #           ResultMisMatching_train[i,11] <-  999
  #           ResultMisMatching_train[i,12] <-  999
  #           ResultMisMatching_train[i,13] <-  999
  #           ResultMisMatching_train[i,14] <-  999
  #           ResultMisMatching_train[i,15] <-  999
  #           ResultMisMatching_train[i,16] <-  999
  #           ResultMisMatching_train[i,17] <-  999
  #           ResultMisMatching_train[i,18] <-  999
  #           ResultMisMatching_train[i,19] <-  999
  #           ResultMisMatching_train[i,20] <-  999
  #           ResultMisMatching_train[i,21] <-  999
  #         }
}

ResultMisMatching_test <-  ResultMisMatching_test_temp
ResultMisMatching_test <-  ResultMisMatching_test[,-10:-65]
ResultMisMatching_test <- cbind( ResultMisMatching_test, NA ,NA, NA , NA , NA,  NA ,NA, NA , NA , NA ,  NA , NA )


for (i in 1:length(jointprob_matching_test_result) ){
  
  #         if ( as.numeric ( Downtarget_attributes_test [i,2] ) >= 8 ) { 
  
  
  if (!is.na(jointprob_matching_test_result[[i]][1][1])) {
    # wim only
    if ( jointprob_matching_test_result[[i]][1,7] >  jointprob_missing_test_result[[i]][1,7] ) {
      ResultMisMatching_test[i,10] <- ResultMisMatching_test[i,4] 
    }
    else {
      ResultMisMatching_test[i,10] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_test_result[[i]][2,8] >  jointprob_missing_test_result[[i]][2,8] ) {
      ResultMisMatching_test[i,11] <- ResultMisMatching_test[i,5] 
    }
    else {
      ResultMisMatching_test[i,11] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_test_result[[i]][3,9] >  jointprob_missing_test_result[[i]][3,9] ) {
      ResultMisMatching_test[i,12] <- ResultMisMatching_test[i,6] 
    }
    else {
      ResultMisMatching_test[i,12] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_test_result[[i]][4,10] >  jointprob_missing_test_result[[i]][4,10] ) {
      ResultMisMatching_test[i,13] <- ResultMisMatching_test[i,7] 
    }
    else {
      ResultMisMatching_test[i,13] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_test_result[[i]][5,11] >  jointprob_missing_test_result[[i]][5,11] ) {
      ResultMisMatching_test[i,14] <- ResultMisMatching_test[i,8] 
    }
    else {
      ResultMisMatching_test[i,14] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_test_result[[i]][6,12] >  jointprob_missing_test_result[[i]][6,12] ) {
      ResultMisMatching_test[i,15] <- ResultMisMatching_test[i,9] 
    }
    else {
      ResultMisMatching_test[i,15] <-  999
    }
    
    
    # normalization
    # wim only
    if ( jointprob_matching_test_result_normalized[[i]][1,7] >  jointprob_missing_test_result_normalized[[i]][1,7] ) {
      ResultMisMatching_test[i,16] <- ResultMisMatching_test[i,4] 
    }
    else {
      ResultMisMatching_test[i,16] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_test_result_normalized[[i]][2,8] >  jointprob_missing_test_result_normalized[[i]][2,8] ) {
      ResultMisMatching_test[i,17] <- ResultMisMatching_test[i,5] 
    }
    else {
      ResultMisMatching_test[i,17] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_test_result_normalized[[i]][3,9] >  jointprob_missing_test_result_normalized[[i]][3,9] ) {
      ResultMisMatching_test[i,18] <- ResultMisMatching_test[i,6] 
    }
    else {
      ResultMisMatching_test[i,18] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_test_result_normalized[[i]][4,10] >  jointprob_missing_test_result_normalized[[i]][4,10] ) {
      ResultMisMatching_test[i,19] <- ResultMisMatching_test[i,7] 
    }
    else {
      ResultMisMatching_test[i,19] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_test_result_normalized[[i]][5,11] >  jointprob_missing_test_result_normalized[[i]][5,11] ) {
      ResultMisMatching_test[i,20] <- ResultMisMatching_test[i,8] 
    }
    else {
      ResultMisMatching_test[i,20] <-  999
    }
    
    
    # wim + sigfeature
    if ( jointprob_matching_test_result_normalized[[i]][6,12] >  jointprob_missing_test_result_normalized[[i]][6,12] ) {
      ResultMisMatching_test[i,21] <- ResultMisMatching_test[i,9] 
    }
    else {
      ResultMisMatching_test[i,21] <-  999
    }
    
  }
  
  else{    
    
    ResultMisMatching_test[i,10] <-  999
    ResultMisMatching_test[i,11] <-  999
    ResultMisMatching_test[i,12] <-  999
    ResultMisMatching_test[i,13] <-  999
    ResultMisMatching_test[i,14] <-  999
    ResultMisMatching_test[i,15] <-  999
    ResultMisMatching_test[i,16] <-  999
    ResultMisMatching_test[i,17] <-  999
    ResultMisMatching_test[i,18] <-  999
    ResultMisMatching_test[i,19] <-  999
    ResultMisMatching_test[i,20] <-  999
    ResultMisMatching_test[i,21] <-  999
  }
  #         }
  
  #         else{    
  #         
  #           ResultMisMatching_test[i,10] <-  999
  #           ResultMisMatching_test[i,11] <-  999
  #           ResultMisMatching_test[i,12] <-  999
  #           ResultMisMatching_test[i,13] <-  999
  #           ResultMisMatching_test[i,14] <-  999
  #           ResultMisMatching_test[i,15] <-  999
  #           ResultMisMatching_test[i,16] <-  999
  #           ResultMisMatching_test[i,17] <-  999
  #           ResultMisMatching_test[i,18] <-  999
  #           ResultMisMatching_test[i,19] <-  999
  #           ResultMisMatching_test[i,20] <-  999
  #           ResultMisMatching_test[i,21] <-  999
  #         }
}

# remove duplicate
remove <- c(999)


for (i in 1:length(ResultMisMatching_train[,1]) ){
  
  #         if ( as.numeric ( ResultMisMatching_train[i,1] ) >= 8 ) { 
  
  
  if (!ResultMisMatching_train[i,10] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,10] [ duplicated(ResultMisMatching_train[1:i,10] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,10] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][1,7] > jointprob_matching_train_result[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching_train[duplicateindex[2], 10] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 10] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,11] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,11] [ duplicated(ResultMisMatching_train[1:i,11] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <-  which( ResultMisMatching_train[1:i,11] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][2,8] > jointprob_matching_train_result[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching_train[duplicateindex[2], 11] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 11] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_train[i,12] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,12] [ duplicated(ResultMisMatching_train[1:i,12] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,12] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][3,9] > jointprob_matching_train_result[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching_train[duplicateindex[2], 12] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 12] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,13] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,13] [ duplicated(ResultMisMatching_train[1:i,13] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,13] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][4,10] > jointprob_matching_train_result[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching_train[duplicateindex[2], 13] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 13] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,14] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,14] [ duplicated(ResultMisMatching_train[1:i,14] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,14] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][5,11] > jointprob_matching_train_result[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching_train[duplicateindex[2], 14] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 14] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,15] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,15] [ duplicated(ResultMisMatching_train[1:i,15] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,15] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result[[ duplicateindex[1] ]][6,12] > jointprob_matching_train_result[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching_train[duplicateindex[2], 15] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 15] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_train[i,16] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,16] [ duplicated(ResultMisMatching_train[1:i,16] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,16] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][1,7] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching_train[duplicateindex[2], 16] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 16] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_train[i,17] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,17] [ duplicated(ResultMisMatching_train[1:i,17] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,17] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][2,8] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching_train[duplicateindex[2], 17] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 17] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,18] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,18] [ duplicated(ResultMisMatching_train[1:i,18] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,18] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][3,9] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching_train[duplicateindex[2], 18] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 18] <- 999
      }
    }
  }
  
  
  
  if (!ResultMisMatching_train[i,19] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,19] [ duplicated(ResultMisMatching_train[1:i,19] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,19] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][4,10] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching_train[duplicateindex[2], 19] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 19] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_train[i,20] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,20] [ duplicated(ResultMisMatching_train[1:i,20] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,20] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][5,11] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching_train[duplicateindex[2], 20] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 20] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_train[i,21] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_train[1:i,21] [ duplicated(ResultMisMatching_train[1:i,21] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_train[1:i,21] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_train_result_normalized[[ duplicateindex[1] ]][6,12] > jointprob_matching_train_result_normalized[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching_train[duplicateindex[2], 21] <- 999
      }
      else{
        ResultMisMatching_train[duplicateindex[1], 21] <- 999
      }
    }
  }
  
  
  #         }
}




for (i in 1:length(ResultMisMatching_test[,1]) ){
  
  #   if ( as.numeric ( ResultMisMatching_test[i,1] ) >= 8 ) { 
  
  
  if (!ResultMisMatching_test[i,10] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,10] [ duplicated(ResultMisMatching_test[1:i,10] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,10] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][1,7] > jointprob_matching_test_result[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching_test[duplicateindex[2], 10] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 10] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,11] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,11] [ duplicated(ResultMisMatching_test[1:i,11] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <-  which( ResultMisMatching_test[1:i,11] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][2,8] > jointprob_matching_test_result[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching_test[duplicateindex[2], 11] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 11] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_test[i,12] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,12] [ duplicated(ResultMisMatching_test[1:i,12] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,12] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][3,9] > jointprob_matching_test_result[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching_test[duplicateindex[2], 12] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 12] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,13] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,13] [ duplicated(ResultMisMatching_test[1:i,13] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,13] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][4,10] > jointprob_matching_test_result[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching_test[duplicateindex[2], 13] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 13] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,14] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,14] [ duplicated(ResultMisMatching_test[1:i,14] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,14] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][5,11] > jointprob_matching_test_result[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching_test[duplicateindex[2], 14] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 14] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,15] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,15] [ duplicated(ResultMisMatching_test[1:i,15] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,15] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result[[ duplicateindex[1] ]][6,12] > jointprob_matching_test_result[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching_test[duplicateindex[2], 15] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 15] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_test[i,16] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,16] [ duplicated(ResultMisMatching_test[1:i,16] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,16] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][1,7] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching_test[duplicateindex[2], 16] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 16] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_test[i,17] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,17] [ duplicated(ResultMisMatching_test[1:i,17] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,17] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][2,8] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching_test[duplicateindex[2], 17] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 17] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,18] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,18] [ duplicated(ResultMisMatching_test[1:i,18] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,18] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][3,9] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching_test[duplicateindex[2], 18] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 18] <- 999
      }
    }
  }
  
  
  
  if (!ResultMisMatching_test[i,19] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,19] [ duplicated(ResultMisMatching_test[1:i,19] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,19] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][4,10] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching_test[duplicateindex[2], 19] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 19] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching_test[i,20] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,20] [ duplicated(ResultMisMatching_test[1:i,20] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,20] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][5,11] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching_test[duplicateindex[2], 20] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 20] <- 999
      }
    }
  }
  
  if (!ResultMisMatching_test[i,21] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching_test[1:i,21] [ duplicated(ResultMisMatching_test[1:i,21] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching_test[1:i,21] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_test_result_normalized[[ duplicateindex[1] ]][6,12] > jointprob_matching_test_result_normalized[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching_test[duplicateindex[2], 21] <- 999
      }
      else{
        ResultMisMatching_test[duplicateindex[1], 21] <- 999
      }
    }
  }
  
  
  #   }
}


# travel time estimation
Traveltime_su_test <- data.frame()
Traveltime_tt_test <- data.frame()
Traveltime_su_train <- data.frame()
Traveltime_tt_train <- data.frame()

TT_su_test_subset <- subset(ResultMisMatching_test[,c(1:3,15,21)], ResultMisMatching_test[,1] < 8) 
TT_tt_test_subset <- subset(ResultMisMatching_test[,c(1:3,15,21)], ResultMisMatching_test[,1] >= 8)
TT_su_train_subset <- subset(ResultMisMatching_train[,c(1:3,15,21)], ResultMisMatching_train[,1] < 8)
TT_tt_train_subset <- subset(ResultMisMatching_train[,c(1:3,15,21)], ResultMisMatching_train[,1] >= 8)


Act_traveltime_su_test <- data.frame()
Est_traveltime_su_test <- data.frame()
j <- 0
k <- 0

for ( i in 1: length(TT_su_test_subset[,1])){
  
  if (  as.numeric(TT_su_test_subset[i,2]) > 1000 ){  
    j <- j+1
    Act_traveltime_su_test[j,1] <- as.numeric(str_sub(TT_su_test_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_su_test_subset[i,2], -13,-1))
  }
  if (  as.numeric(TT_su_test_subset[i,4]) > 1000 ){ 
    k <- k+1
    Est_traveltime_su_test[k,1] <- as.numeric(str_sub(TT_su_test_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_su_test_subset[i,4], -13,-1))  
  }
  
}

TT_MS <- data.frame()
TT_MS<- cbind( mean( Act_traveltime_su_test[,1]) , mean( Est_traveltime_su_test[,1]),
                 (abs(mean( Act_traveltime_su_test[,1]) -mean( Est_traveltime_su_test[,1]) ))  / mean( Act_traveltime_su_test[,1]) )



Act_traveltime_tt_test <- data.frame()
Est_traveltime_tt_test <- data.frame()
j <- 0
k <- 0

for ( i in 1: length(TT_tt_test_subset[,1])){
  
  if (  as.numeric(TT_tt_test_subset[i,2]) > 1000 ){  
    j <- j+1
    Act_traveltime_tt_test[j,1] <- as.numeric(str_sub(TT_tt_test_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_tt_test_subset[i,2], -13,-1))
  }
  if (  as.numeric(TT_tt_test_subset[i,4]) > 1000 ){ 
    k <- k+1
    Est_traveltime_tt_test[k,1] <- as.numeric(str_sub(TT_tt_test_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_tt_test_subset[i,4], -13,-1))  
  }
  
}


TT_MS  <- cbind(TT_MS, mean( Act_traveltime_tt_test[,1]) , mean( Est_traveltime_tt_test[,1]),
                        (abs(mean( Act_traveltime_tt_test[,1]) -mean( Est_traveltime_tt_test[,1]) ))  / mean( Act_traveltime_tt_test[,1]) )



Act_traveltime_su_train <- data.frame()
Est_traveltime_su_train <- data.frame()
j <- 0
k <- 0

for ( i in 1: length(TT_su_train_subset[,1])){
  
  if (  as.numeric(TT_su_train_subset[i,2]) > 1000 ){  
    j <- j+1
    Act_traveltime_su_train[j,1] <- as.numeric(str_sub(TT_su_train_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_su_train_subset[i,2], -13,-1))
  }
  if (  as.numeric(TT_su_train_subset[i,4]) > 1000 ){ 
    k <- k+1
    Est_traveltime_su_train[k,1] <- as.numeric(str_sub(TT_su_train_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_su_train_subset[i,4], -13,-1))  
  }
  
}


TT_MS <- cbind( TT_MS , mean( Act_traveltime_su_train[,1]) , mean( Est_traveltime_su_train[,1]),
               (abs(mean( Act_traveltime_su_train[,1]) -mean( Est_traveltime_su_train[,1]) ))  / mean( Act_traveltime_su_train[,1]) )



Act_traveltime_tt_train <- data.frame()
Est_traveltime_tt_train <- data.frame()
j <- 0
k <- 0

for ( i in 1: length(TT_tt_train_subset[,1])){
  
  if (  as.numeric(TT_tt_train_subset[i,2]) > 1000 ){  
    j <- j+1
    Act_traveltime_tt_train[j,1] <- as.numeric(str_sub(TT_tt_train_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_tt_train_subset[i,2], -13,-1))
  }
  if (  as.numeric(TT_tt_train_subset[i,4]) > 1000 ){ 
    k <- k+1
    Est_traveltime_tt_train[k,1] <- as.numeric(str_sub(TT_tt_train_subset[i,3], -13,-1)) - as.numeric(str_sub(TT_tt_train_subset[i,4], -13,-1))  
  }
  
}


TT_MS  <- cbind(TT_MS, mean( Act_traveltime_tt_train[,1]) , mean( Est_traveltime_tt_train[,1]),
                (abs(mean( Act_traveltime_tt_train[,1]) -mean( Est_traveltime_tt_train[,1]) ))  / mean( Act_traveltime_tt_train[,1]) )
