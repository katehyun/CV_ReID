load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_04082014")


weight <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,50)
weight_n <- c(1.5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1.5)
buf_matching <- 0.001 
buf_missing <- 0.001 
alphasig <- 0.01
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

Upcandidates_attribute_train_missing<- data.frame()


for (i in 1: length(Upcandidates_train)) {  
  
 
  #   if ( as.numeric (Downtarget_attributes_train[i,2] ) == 9 ) {  
   
  #     k <- k+1  
    
      for (j in 1:length(max_train_mat) ) { 
        
       if (idxjointprob_train[i] != 999 & !is.na(idxjointprob_train[i]) ) {
        Upcandidates_attribute_train_missing[i,j] <-  Attribute_diff_nonnormal_train[[i]][[ idxjointprob_train[i] ]] [j] 
        }
      
      else {
        Upcandidates_attribute_train_missing[i,j] <- NA       
      }
      
      }
    
#     if (Upcandidates_attribute_train_missing_mat[k,31] == 99999)
#       Upcandidates_attribute_train_missing_mat[k,31] <- NA

}

 maxsig_tr <- na.omit(Upcandidates_attribute_train_missing[,31])
 max_sig_tr <- max(maxsig_tr)

j <- 0
k <- 0
m <- 0

Upcandidates_attribute_test_missing <- data.frame()

for (i in 1: length(Upcandidates_test)) {  
  
  
#   if ( as.numeric (Downtarget_attributes_test[i,2] ) == 9 ) {  
    
#     k <- k+1    
    
  for (j in 1:length(max_train_mat) ) { 
    
    if (idxjointprob_test[i] != 999 & !is.na(idxjointprob_test[i]) ) {
      Upcandidates_attribute_test_missing[i,j] <-   Attribute_diff_nonnormal_test[[i]][[ idxjointprob_test[i] ]] [j] 
    }
    
    else {
      Upcandidates_attribute_test_missing[i,j] <- NA
      
    }
    
    #     if (Upcandidates_attribute_test_missing_mat[k,31] == 99999)
    #       Upcandidates_attribute_test_missing_mat[k,31] <- NA
    
  }
}

maxsig_te <- na.omit(Upcandidates_attribute_test_missing[,31])
max_sig_te <- max(maxsig_te)


jointprob_matching_train <- data.frame(matrix(ncol = 34))
jointprob_missing_train <- data.frame(matrix(ncol = 34))


# train - matching
k <- 0

for (i in 1:length(Upcandidates_attribute_train_missing[,1])){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[i,1] )  ) {
      jointprob_matching_train[i,] <-  seq(0,0, length=34)  
      jointprob_matching_train[i,][jointprob_matching_train[i,] == 0] <- NA
    }
    
    else
    {
      
      for (m in 1: 31) { 
#       for (m in 1: 30) { 
        
        # option 1 - non parametric
        #         if ( m %in% class9idxprob) {
        #           jointprob_matching_train[i,m] <- as.numeric ( approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
        #                                                                Upcandidates_attribute_train_missing[i,m] )$y )
        #         }
        
        # another option 1 - non parametric but smoothing
#         if ( m %in% class9idxprob) {
#           jointprob_matching_train[i,m] <- as.numeric ( approx( density_smooth_hist_mat_c[[m]]$x, density_smooth_hist_mat_c[[m]]$y,
#                                                                 Upcandidates_attribute_train_missing[i,m] )$y )
#           jointprob_matching_train[i,m] [is.na(jointprob_matching_train[i,m])] <- buf 
#         }
        
        
        # option 2 - parametric
            if ( m %in% class9idxprob) {
              jointprob_matching_train[i,m] <- as.numeric ( approx( diffseq_mat_c[[m]], 
                                               normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                               Upcandidates_attribute_train_missing[i,m] )$y )
              jointprob_matching_train[i,m] [is.na(jointprob_matching_train[i,m])] <- buf_matching 
            }
        
        # option 3 - hitogram
        #     if ( m %in% class9idxprob) {
        #       
        #       
        #       if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[i,m] & 
        #                               Upcandidates_attribute_train_missing[i,m] &  <  histdensity_c[[m]][,2])) > 0 )
        #         
        #       {
        #         jointprob_matching_train[i,m]  <- 
        #           histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing_mat[i,m] &
        #                                        Upcandidates_attribute_train_missing_mat[i,m] < histdensity_c[[m]][,2]),3]  
        #       }
        #    
        #     
        #       else {
        #         jointprob_matching_train[i,m] <- buf
        #       }
        #     }
        #     
        
        
        else {
          jointprob_matching_train[i,m] <- 99999
        }
        
      }
      
#         jointprob_matching_train[i,31] <- (Upcandidates_attribute_train_missing[i,31]) 

      
      
      # option 1
      #   jointprob_matching_train[i,32] <-  jointprob_matching_train[i,1] * jointprob_matching_train[i,2] * jointprob_matching_train[i,3] *  jointprob_matching_train[i,4] * 
      #     jointprob_matching_train[i,5] * jointprob_matching_train[i,6] * jointprob_matching_train[i,7] *  jointprob_matching_train[i,12] * 
      #     jointprob_matching_train[i,13] * jointprob_matching_train[i,14] *  jointprob_matching_train[i,15] * jointprob_matching_train[i,16] *
      #     jointprob_matching_train[i,17] * jointprob_matching_train[i,18] *  jointprob_matching_train[i,19] *  jointprob_matching_train[i,20] *
      #     jointprob_matching_train[i,21] *   jointprob_matching_train[i,30]  *
      #     jointprob_matching_train[i,31] 
      #   
      
      # option 2
      
      jointprob_matching_train[i,32] <-   log10( jointprob_matching_train[i,1]) * weight[1] +  
                                          log10( jointprob_matching_train[i,2]) * weight[2] +
                                          log10( jointprob_matching_train[i,3]) * weight[3] +
                                          log10( jointprob_matching_train[i,4]) * weight[4] +
                                          log10( jointprob_matching_train[i,5]) * weight[5] +
                                          log10( jointprob_matching_train[i,6]) * weight[6] +
                                          log10( jointprob_matching_train[i,7]) * weight[7] +
                                          log10( jointprob_matching_train[i,13]) * weight[13] +
                                          log10( jointprob_matching_train[i,14]) * weight[14] +
                                          log10( jointprob_matching_train[i,15]) * weight[15] +
                                          log10( jointprob_matching_train[i,16]) * weight[16] +
                                          log10( jointprob_matching_train[i,17]) * weight[17] +
                                          log10( jointprob_matching_train[i,18]) * weight[18] +
                                          log10( jointprob_matching_train[i,19]) * weight[19] +
                                          log10( jointprob_matching_train[i,20]) * weight[20] +
                                          log10( jointprob_matching_train[i,21]) * weight[21] +
                                          log10( jointprob_matching_train[i,30]) * weight[30] 
#                                           log10(    1 - ( alphasig * jointprob_matching_train[i,31] ) )
#                                           log10( jointprob_matching_train[i,31]) * weight[31] 
#                                           log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
#       jointprob_matching_train[i,33] <-  log10( 1/ jointprob_matching_train[i,31]) * weight[31]  
      jointprob_matching_train[i,33] <-  log10(jointprob_matching_train[i,31]) * weight[31] 
      jointprob_matching_train[i,34] <-  Upcandidates_train[[i]][idxjointprob_train[i]]   
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_train[i,] <-  seq(0,0, length=34)  
    jointprob_matching_train[i,][jointprob_matching_train[i,] == 0] <- NA
  }
  
}


# train - missing

k <- 0

for (i in 1:length(Upcandidates_attribute_train_missing[,1])){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[i,1] )  ) {
      jointprob_missing_train[i,] <-  seq(0,0, length=34)  
      jointprob_missing_train[i,][jointprob_missing_train[i,] == 0] <- NA
    }
    
    else
    {
      
      for (m in 1: 31) { 
        
        # option 1 - non parametric
#         if ( m %in% class9idxprob) {
#           jointprob_missing_train[i,m] <- as.numeric ( approx( kernel_nonmat_c[[m]]$x, kernel_nonmat_c[[m]]$y,
#                                                                Upcandidates_attribute_train_missing[i,m] )$y )
#         }
        
        # another option 1 - non parametric but smoothing
#             if ( m %in% class9idxprob) {
#               jointprob_missing_train[i,m] <- as.numeric ( approx( density_smooth_hist_nonmat_c[[m]]$x, density_smooth_hist_nonmat_c[[m]]$y,
#                                                                     Upcandidates_attribute_train_missing[i,m] )$y )
#               jointprob_missing_train[i,m] [is.na(jointprob_missing_train[i,m])] <- buf 
#             }
        
        
        # option 2 - parametric
            if ( m %in% class9idxprob) {
              jointprob_missing_train[i,m] <- as.numeric ( approx( diffseq_nonmat_c[[m]], 
                                               normal_nonmat_c[[m]]  * multiplier_hist_nonmat_c[[m]][ which.min(is.na( multiplier_hist_nonmat_c[[m]] ) ) ] ,
                                               Upcandidates_attribute_train_missing[i,m] )$y )
              jointprob_missing_train[i,m] [is.na(jointprob_missing_train[i,m])] <- buf_missing 
            }
        
        # option 3 - hitogram
        #     if ( m %in% class9idxprob) {
        #       
        #       
        #       if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[i,m] & 
        #                               Upcandidates_attribute_train_missing[i,m] &  <  histdensity_c[[m]][,2])) > 0 )
        #         
        #       {
        #         jointprob_missing_train[i,m]  <- 
        #           histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing_mat[i,m] &
        #                                        Upcandidates_attribute_train_missing_mat[i,m] < histdensity_c[[m]][,2]),4]  
        #       }
        #    
        #     
        #       else {
        #         jointprob_missing_train[i,m] <- buf
        #       }
        #     }
        #     
        
        
        else {
          jointprob_missing_train[i,m] <- 99999
        }
        
      }
     
#         jointprob_missing_train[i,31] <- (Upcandidates_attribute_train_missing[i,31] ) 
                  
      
      
      # option 1
      #   jointprob_missing_train[i,32] <-  jointprob_missing_train[i,1] * jointprob_missing_train[i,2] * jointprob_missing_train[i,3] *  jointprob_missing_train[i,4] * 
      #     jointprob_missing_train[i,5] * jointprob_missing_train[i,6] * jointprob_missing_train[i,7] *  jointprob_missing_train[i,12] * 
      #     jointprob_missing_train[i,13] * jointprob_missing_train[i,14] *  jointprob_missing_train[i,15] * jointprob_missing_train[i,16] *
      #     jointprob_missing_train[i,17] * jointprob_missing_train[i,18] *  jointprob_missing_train[i,19] *  jointprob_missing_train[i,20] *
      #     jointprob_missing_train[i,21] *   jointprob_missing_train[i,30]  *
      #     jointprob_missing_train[i,31] 
      #   
      
      # option 2
      

      jointprob_missing_train[i,32] <-    log10( jointprob_missing_train[i,1]) * weight[1] +  
                                          log10( jointprob_missing_train[i,2]) * weight[2] +
                                          log10( jointprob_missing_train[i,3]) * weight[3] +
                                          log10( jointprob_missing_train[i,4]) * weight[4] +
                                          log10( jointprob_missing_train[i,5]) * weight[5] +
                                          log10( jointprob_missing_train[i,6]) * weight[6] +
                                          log10( jointprob_missing_train[i,7]) * weight[7] +
                                          log10( jointprob_missing_train[i,13]) * weight[13] +
                                          log10( jointprob_missing_train[i,14]) * weight[14] +
                                          log10( jointprob_missing_train[i,15]) * weight[15] +
                                          log10( jointprob_missing_train[i,16]) * weight[16] +
                                          log10( jointprob_missing_train[i,17]) * weight[17] +
                                          log10( jointprob_missing_train[i,18]) * weight[18] +
                                          log10( jointprob_missing_train[i,19]) * weight[19] +
                                          log10( jointprob_missing_train[i,20]) * weight[20] +
                                          log10( jointprob_missing_train[i,21]) * weight[21] +
                                          log10( jointprob_missing_train[i,30]) * weight[30]    
#                                           log10( alphasig * jointprob_missing_train[i,31] ) 
#                                           log10( jointprob_missing_train[i,31]) * weight[31] 
#                                           log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 

#       jointprob_missing_train[i,33] <-  log10( 1/jointprob_missing_train[i,31]) * weight[31] 
      jointprob_missing_train[i,33] <-  log10( jointprob_missing_train[i,31]) * weight[31] 
      jointprob_missing_train[i,34] <-  Upcandidates_train[[i]][idxjointprob_train[i]]   
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_train[i,] <-  seq(0,0, length=34)  
    jointprob_missing_train[i,][jointprob_missing_train[i,] == 0] <- NA
  }
  
}



# test - matching

jointprob_matching_test <- data.frame(matrix(ncol = 34))
jointprob_missing_test <- data.frame(matrix(ncol = 34))


# test - matching
k <- 0

for (i in 1:length(Upcandidates_attribute_test_missing[,1])){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[i,1] )  ) {
      jointprob_matching_test[i,] <-  seq(0,0, length=34)  
      jointprob_matching_test[i,][jointprob_matching_test[i,] == 0] <- NA
    }
    
    else
    {
      
      for (m in 1: 31) { 
        
        # option 1 - non parametric
        #         if ( m %in% class9idxprob) {
        #           jointprob_matching_test[i,m] <- as.numeric ( approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
        #                                                                Upcandidates_attribute_test_missing[i,m] )$y )
        #         }
        
        # another option 1 - non parametric but smoothing
#         if ( m %in% class9idxprob) {
#           jointprob_matching_test[i,m] <- as.numeric ( approx( density_smooth_hist_mat_c[[m]]$x, density_smooth_hist_mat_c[[m]]$y,
#                                                                Upcandidates_attribute_test_missing[i,m] )$y )
#           jointprob_matching_test[i,m] [is.na(jointprob_matching_test[i,m])] <- buf 
#         }
        
        
        # option 2 - parametric
            if ( m %in% class9idxprob) {
              jointprob_matching_test[i,m] <- as.numeric ( approx( diffseq_mat_c[[m]], 
                                               normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                               Upcandidates_attribute_test_missing[i,m] )$y )
              jointprob_matching_test[i,m] [is.na(jointprob_matching_test[i,m])] <- buf_matching
            }
        
        # option 3 - hitogram
        #     if ( m %in% class9idxprob) {
        #       
        #       
        #       if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[i,m] & 
        #                               Upcandidates_attribute_test_missing[i,m] &  <  histdensity_c[[m]][,2])) > 0 )
        #         
        #       {
        #         jointprob_matching_test[i,m]  <- 
        #           histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing_mat[i,m] &
        #                                        Upcandidates_attribute_test_missing_mat[i,m] < histdensity_c[[m]][,2]),3]  
        #       }
        #    
        #     
        #       else {
        #         jointprob_matching_test[i,m] <- buf
        #       }
        #     }
        #     
        
        
        else {
          jointprob_matching_test[i,m] <- 99999
        }
        
      }
      
#         jointprob_matching_test[i,31] <- (  Upcandidates_attribute_test_missing[i,31] ) 
      
      
      
      # option 1
      #   jointprob_matching_test[i,32] <-  jointprob_matching_test[i,1] * jointprob_matching_test[i,2] * jointprob_matching_test[i,3] *  jointprob_matching_test[i,4] * 
      #     jointprob_matching_test[i,5] * jointprob_matching_test[i,6] * jointprob_matching_test[i,7] *  jointprob_matching_test[i,12] * 
      #     jointprob_matching_test[i,13] * jointprob_matching_test[i,14] *  jointprob_matching_test[i,15] * jointprob_matching_test[i,16] *
      #     jointprob_matching_test[i,17] * jointprob_matching_test[i,18] *  jointprob_matching_test[i,19] *  jointprob_matching_test[i,20] *
      #     jointprob_matching_test[i,21] *   jointprob_matching_test[i,30]  *
      #     jointprob_matching_test[i,31] 
      #   
      
      # option 2
      

jointprob_matching_test[i,32] <-    log10( jointprob_matching_test[i,1]) * weight[1] +  
                                    log10( jointprob_matching_test[i,2]) * weight[2] +
                                    log10( jointprob_matching_test[i,3]) * weight[3] +
                                    log10( jointprob_matching_test[i,4]) * weight[4] +
                                    log10( jointprob_matching_test[i,5]) * weight[5] +
                                    log10( jointprob_matching_test[i,6]) * weight[6] +
                                    log10( jointprob_matching_test[i,7]) * weight[7] +
                                    log10( jointprob_matching_test[i,13]) * weight[13] +
                                    log10( jointprob_matching_test[i,14]) * weight[14] +
                                    log10( jointprob_matching_test[i,15]) * weight[15] +
                                    log10( jointprob_matching_test[i,16]) * weight[16] +
                                    log10( jointprob_matching_test[i,17]) * weight[17] +
                                    log10( jointprob_matching_test[i,18]) * weight[18] +
                                    log10( jointprob_matching_test[i,19]) * weight[19] +
                                    log10( jointprob_matching_test[i,20]) * weight[20] +
                                    log10( jointprob_matching_test[i,21]) * weight[21] +
                                    log10( jointprob_matching_test[i,30]) * weight[30] 
#                                     log10(    1 - ( alphasig * jointprob_matching_test[i,31] ) )
#                                     log10( jointprob_matching_test[i,31]) * weight[31] 
#                                     log10(1/ Upcandidates_attribute_test_missing[i,31]) * weight[31] 

#       jointprob_matching_test[i,33] <- log10( 1/ jointprob_matching_test[i,31]) * weight[31] 
      jointprob_matching_test[i,33] <- log10( jointprob_matching_test[i,31]) * weight[31]
      jointprob_matching_test[i,34] <-  Upcandidates_test[[i]][idxjointprob_test[i]]   
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_test[i,] <-  seq(0,0, length=34)  
    jointprob_matching_test[i,][jointprob_matching_test[i,] == 0] <- NA
  }
  
}


# test - missing

k <- 0

for (i in 1:length(Upcandidates_attribute_test_missing[,1])){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[i,1] )  ) {
      jointprob_missing_test[i,] <-  seq(0,0, length=34)  
      jointprob_missing_test[i,][jointprob_missing_test[i,] == 0] <- NA
    }
    
    else
    {
      
      for (m in 1: 31) { 
        
        # option 1 - non parametric
        #         if ( m %in% class9idxprob) {
        #           jointprob_missing_test[i,m] <- as.numeric ( approx( kernel_nonmat_c[[m]]$x, kernel_nonmat_c[[m]]$y,
        #                                                                Upcandidates_attribute_test_missing[i,m] )$y )
        #         }
        
        # another option 1 - non parametric but smoothing
#         if ( m %in% class9idxprob) {
#           jointprob_missing_test[i,m] <- as.numeric ( approx( density_smooth_hist_nonmat_c[[m]]$x, density_smooth_hist_nonmat_c[[m]]$y,
#                                                               Upcandidates_attribute_test_missing[i,m] )$y )
#           jointprob_missing_test[i,m] [is.na(jointprob_missing_test[i,m])] <- buf 
#         }
        
        
        # option 2 - parametric
            if ( m %in% class9idxprob) {
              jointprob_missing_test[i,m] <- as.numeric ( approx( diffseq_nonmat_c[[m]], 
                                               normal_nonmat_c[[m]]  * multiplier_hist_nonmat_c[[m]][ which.min(is.na( multiplier_hist_nonmat_c[[m]] ) ) ] ,
                                               Upcandidates_attribute_test_missing[i,m] )$y )
              jointprob_missing_test[i,m] [is.na(jointprob_missing_test[i,m])] <- buf_missing
            }
        
        # option 3 - hitogram
        #     if ( m %in% class9idxprob) {
        #       
        #       
        #       if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[i,m] & 
        #                               Upcandidates_attribute_test_missing[i,m] &  <  histdensity_c[[m]][,2])) > 0 )
        #         
        #       {
        #         jointprob_missing_test[i,m]  <- 
        #           histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing_mat[i,m] &
        #                                        Upcandidates_attribute_test_missing_mat[i,m] < histdensity_c[[m]][,2]),4]  
        #       }
        #    
        #     
        #       else {
        #         jointprob_missing_test[i,m] <- buf
        #       }
        #     }
        #     
        
        
        else {
          jointprob_missing_test[i,m] <- 99999
        }
        
      }
      
#         jointprob_missing_test[i,31] <- (Upcandidates_attribute_test_missing[i,31] ) 
      
      
      
      # option 1
      #   jointprob_missing_test[i,32] <-  jointprob_missing_test[i,1] * jointprob_missing_test[i,2] * jointprob_missing_test[i,3] *  jointprob_missing_test[i,4] * 
      #     jointprob_missing_test[i,5] * jointprob_missing_test[i,6] * jointprob_missing_test[i,7] *  jointprob_missing_test[i,12] * 
      #     jointprob_missing_test[i,13] * jointprob_missing_test[i,14] *  jointprob_missing_test[i,15] * jointprob_missing_test[i,16] *
      #     jointprob_missing_test[i,17] * jointprob_missing_test[i,18] *  jointprob_missing_test[i,19] *  jointprob_missing_test[i,20] *
      #     jointprob_missing_test[i,21] *   jointprob_missing_test[i,30]  *
      #     jointprob_missing_test[i,31] 
      #   
      
      # option 2
      

jointprob_missing_test[i,32] <-     log10( jointprob_missing_test[i,1]) * weight[1] +  
                                    log10( jointprob_missing_test[i,2]) * weight[2] +
                                    log10( jointprob_missing_test[i,3]) * weight[3] +
                                    log10( jointprob_missing_test[i,4]) * weight[4] +
                                    log10( jointprob_missing_test[i,5]) * weight[5] +
                                    log10( jointprob_missing_test[i,6]) * weight[6] +
                                    log10( jointprob_missing_test[i,7]) * weight[7] +
                                    log10( jointprob_missing_test[i,13]) * weight[13] +
                                    log10( jointprob_missing_test[i,14]) * weight[14] +
                                    log10( jointprob_missing_test[i,15]) * weight[15] +
                                    log10( jointprob_missing_test[i,16]) * weight[16] +
                                    log10( jointprob_missing_test[i,17]) * weight[17] +
                                    log10( jointprob_missing_test[i,18]) * weight[18] +
                                    log10( jointprob_missing_test[i,19]) * weight[19] +
                                    log10( jointprob_missing_test[i,20]) * weight[20] +
                                    log10( jointprob_missing_test[i,21]) * weight[21] +
                                    log10( jointprob_missing_test[i,30]) * weight[30] 
#                                     log10(    alphasig * jointprob_matching_test[i,31] ) 
#                                     log10( jointprob_missing_test[i,31]) * weight[31] 
#                                     log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
#       jointprob_missing_test[i,33] <-  log10(1/ jointprob_missing_test[i,31]) * weight[31] 
      jointprob_missing_test[i,33] <-  log10( jointprob_missing_test[i,31]) * weight[31] 
      jointprob_missing_test[i,34] <-  Upcandidates_test[[i]][idxjointprob_test[i]]   
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_test[i,] <-  seq(0,0, length=34)  
    jointprob_missing_test[i,][jointprob_missing_test[i,] == 0] <- NA
  }
  
}



# probability normalization
jointprob_matching_train_normalized <- data.frame()
jointprob_missing_train_normalized <- data.frame()

for (i in 1: length(jointprob_matching_train[,1])){
  
  for (j in 1: (length(jointprob_missing_train[1,])-2) ){
 
    jointprob_matching_train_normalized[i,j] = 
      jointprob_matching_train[i,j]  / ( jointprob_matching_train[i,j] + jointprob_missing_train[i,j] )
    
    jointprob_missing_train_normalized[i,j] =
      jointprob_missing_train[i,j]   / ( jointprob_matching_train[i,j] + jointprob_missing_train[i,j] )

  }
  
  jointprob_matching_train_normalized[i,32] <-    log10( jointprob_matching_train_normalized[i,1]) * weight_n[1] +  
                                                  log10( jointprob_matching_train_normalized[i,2]) * weight_n[2] +
                                                  log10( jointprob_matching_train_normalized[i,3]) * weight_n[3] +
                                                  log10( jointprob_matching_train_normalized[i,4]) * weight_n[4] +
                                                  log10( jointprob_matching_train_normalized[i,5]) * weight_n[5] +
                                                  log10( jointprob_matching_train_normalized[i,6]) * weight_n[6] +
                                                  log10( jointprob_matching_train_normalized[i,7]) * weight_n[7] +
                                                  log10( jointprob_matching_train_normalized[i,13]) * weight_n[13] +
                                                  log10( jointprob_matching_train_normalized[i,14]) * weight_n[14] +
                                                  log10( jointprob_matching_train_normalized[i,15]) * weight_n[15] +
                                                  log10( jointprob_matching_train_normalized[i,16]) * weight_n[16] +
                                                  log10( jointprob_matching_train_normalized[i,17]) * weight_n[17] +
                                                  log10( jointprob_matching_train_normalized[i,18]) * weight_n[18] +
                                                  log10( jointprob_matching_train_normalized[i,19]) * weight_n[19] +
                                                  log10( jointprob_matching_train_normalized[i,20]) * weight_n[20] +
                                                  log10( jointprob_matching_train_normalized[i,21]) * weight_n[21] +
                                                  log10( jointprob_matching_train_normalized[i,30]) * weight_n[30] 
#                                                   log10( jointprob_matching_train_normalized[i,31]) * weight_n[31] + 
#                                                   log10(1/Upcandidates_attribute_train_missing[i,31]) * weight_n[31] 

    jointprob_matching_train_normalized[i,33] =  jointprob_matching_train[i,33]
          
#   jointprob_matching_train_normalized[i,32] = 
#     jointprob_matching_train_normalized[i,1] * jointprob_matching_train_normalized[i,2] * jointprob_matching_train_normalized[i,3] * 
#     jointprob_matching_train_normalized[i,4] * jointprob_matching_train_normalized[i,5] * jointprob_matching_train_normalized[i,6] *  
#     jointprob_matching_train_normalized[i,7] * jointprob_matching_train_normalized[i,12] * jointprob_matching_train_normalized[i,13] *
#     jointprob_matching_train_normalized[i,14] * jointprob_matching_train_normalized[i,15] * jointprob_matching_train_normalized[i,16] *
#     jointprob_matching_train_normalized[i,17] * jointprob_matching_train_normalized[i,18] * jointprob_matching_train_normalized[i,19] *
#     jointprob_matching_train_normalized[i,20] * jointprob_matching_train_normalized[i,21] * jointprob_matching_train_normalized[i,30] *
#     jointprob_matching_train_normalized[i,31] 
  


jointprob_missing_train_normalized[i,32] <-   log10( jointprob_missing_train_normalized[i,1]) * weight_n[1] +  
                                              log10( jointprob_missing_train_normalized[i,2]) * weight_n[2] +
                                              log10( jointprob_missing_train_normalized[i,3]) * weight_n[3] +
                                              log10( jointprob_missing_train_normalized[i,4]) * weight_n[4] +
                                              log10( jointprob_missing_train_normalized[i,5]) * weight_n[5] +
                                              log10( jointprob_missing_train_normalized[i,6]) * weight_n[6] +
                                              log10( jointprob_missing_train_normalized[i,7]) * weight_n[7] +
                                              log10( jointprob_missing_train_normalized[i,13]) * weight_n[13] +
                                              log10( jointprob_missing_train_normalized[i,14]) * weight_n[14] +
                                              log10( jointprob_missing_train_normalized[i,15]) * weight_n[15] +
                                              log10( jointprob_missing_train_normalized[i,16]) * weight_n[16] +
                                              log10( jointprob_missing_train_normalized[i,17]) * weight_n[17] +
                                              log10( jointprob_missing_train_normalized[i,18]) * weight_n[18] +
                                              log10( jointprob_missing_train_normalized[i,19]) * weight_n[19] +
                                              log10( jointprob_missing_train_normalized[i,20]) * weight_n[20] +
                                              log10( jointprob_missing_train_normalized[i,21]) * weight_n[21] +
                                              log10( jointprob_missing_train_normalized[i,30]) * weight_n[30] 
#                                               log10( jointprob_missing_train_normalized[i,31]) * weight_n[31] + 
#                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight_n[31] 

  jointprob_missing_train_normalized[i,33] =  jointprob_missing_train[i,33]
#   jointprob_missing_train_normalized[i,32] = 
#     jointprob_missing_train_normalized[i,1] * jointprob_missing_train_normalized[i,2] * jointprob_missing_train_normalized[i,3] * 
#     jointprob_missing_train_normalized[i,4] * jointprob_missing_train_normalized[i,5] * jointprob_missing_train_normalized[i,6] *  
#     jointprob_missing_train_normalized[i,7] * jointprob_missing_train_normalized[i,12] * jointprob_missing_train_normalized[i,13] *
#     jointprob_missing_train_normalized[i,14] * jointprob_missing_train_normalized[i,15] * jointprob_missing_train_normalized[i,16] *
#     jointprob_missing_train_normalized[i,17] * jointprob_missing_train_normalized[i,18] * jointprob_missing_train_normalized[i,19] *
#     jointprob_missing_train_normalized[i,20] * jointprob_missing_train_normalized[i,21] * jointprob_missing_train_normalized[i,30] *
#     jointprob_missing_train_normalized[i,31] 
}


jointprob_matching_test_normalized <- data.frame()
jointprob_missing_test_normalized <- data.frame()

for (i in 1: length(jointprob_matching_test[,1])){
  for (j in 1: (length(jointprob_missing_test[1,])-2)){
    
    
    jointprob_matching_test_normalized[i,j] = 
      jointprob_matching_test[i,j]  / ( jointprob_matching_test[i,j] + jointprob_missing_test[i,j] )
    
    jointprob_missing_test_normalized[i,j] =
      jointprob_missing_test[i,j]   / ( jointprob_matching_test[i,j] + jointprob_missing_test[i,j] )
    
  }
  
  jointprob_matching_test_normalized[i,32] <-     log10( jointprob_matching_test_normalized[i,1]) * weight_n[1] +  
                                                  log10( jointprob_matching_test_normalized[i,2]) * weight_n[2] +
                                                  log10( jointprob_matching_test_normalized[i,3]) * weight_n[3] +
                                                  log10( jointprob_matching_test_normalized[i,4]) * weight_n[4] +
                                                  log10( jointprob_matching_test_normalized[i,5]) * weight_n[5] +
                                                  log10( jointprob_matching_test_normalized[i,6]) * weight_n[6] +
                                                  log10( jointprob_matching_test_normalized[i,7]) * weight_n[7] +
                                                  log10( jointprob_matching_test_normalized[i,13]) * weight_n[13] +
                                                  log10( jointprob_matching_test_normalized[i,14]) * weight_n[14] +
                                                  log10( jointprob_matching_test_normalized[i,15]) * weight_n[15] +
                                                  log10( jointprob_matching_test_normalized[i,16]) * weight_n[16] +
                                                  log10( jointprob_matching_test_normalized[i,17]) * weight_n[17] +
                                                  log10( jointprob_matching_test_normalized[i,18]) * weight_n[18] +
                                                  log10( jointprob_matching_test_normalized[i,19]) * weight_n[19] +
                                                  log10( jointprob_matching_test_normalized[i,20]) * weight_n[20] +
                                                  log10( jointprob_matching_test_normalized[i,21]) * weight_n[21] +
                                                  log10( jointprob_matching_test_normalized[i,30]) * weight_n[30] 
#                                                   log10( jointprob_matching_test_normalized[i,31]) * weight_n[31] + 
#                                                   log10(1/Upcandidates_attribute_test_missing[i,31]) * weight_n[31] 
  
  jointprob_matching_test_normalized[i,33] = jointprob_matching_test[i,33] 
#   jointprob_matching_test_normalized[i,32] = 
#     jointprob_matching_test_normalized[i,1] * jointprob_matching_test_normalized[i,2] * jointprob_matching_test_normalized[i,3] * 
#     jointprob_matching_test_normalized[i,4] * jointprob_matching_test_normalized[i,5] * jointprob_matching_test_normalized[i,6] *  
#     jointprob_matching_test_normalized[i,7] * jointprob_matching_test_normalized[i,12] * jointprob_matching_test_normalized[i,13] *
#     jointprob_matching_test_normalized[i,14] * jointprob_matching_test_normalized[i,15] * jointprob_matching_test_normalized[i,16] *
#     jointprob_matching_test_normalized[i,17] * jointprob_matching_test_normalized[i,18] * jointprob_matching_test_normalized[i,19] *
#     jointprob_matching_test_normalized[i,20] * jointprob_matching_test_normalized[i,21] * jointprob_matching_test_normalized[i,30] *
#     jointprob_matching_test_normalized[i,31] 
 
jointprob_missing_test_normalized[i,32] <-      log10( jointprob_missing_test_normalized[i,1]) * weight_n[1] +  
                                                log10( jointprob_missing_test_normalized[i,2]) * weight_n[2] +
                                                log10( jointprob_missing_test_normalized[i,3]) * weight_n[3] +
                                                log10( jointprob_missing_test_normalized[i,4]) * weight_n[4] +
                                                log10( jointprob_missing_test_normalized[i,5]) * weight_n[5] +
                                                log10( jointprob_missing_test_normalized[i,6]) * weight_n[6] +
                                                log10( jointprob_missing_test_normalized[i,7]) * weight_n[7] +
                                                log10( jointprob_missing_test_normalized[i,13]) * weight_n[13] +
                                                log10( jointprob_missing_test_normalized[i,14]) * weight_n[14] +
                                                log10( jointprob_missing_test_normalized[i,15]) * weight_n[15] +
                                                log10( jointprob_missing_test_normalized[i,16]) * weight_n[16] +
                                                log10( jointprob_missing_test_normalized[i,17]) * weight_n[17] +
                                                log10( jointprob_missing_test_normalized[i,18]) * weight_n[18] +
                                                log10( jointprob_missing_test_normalized[i,19]) * weight_n[19] +
                                                log10( jointprob_missing_test_normalized[i,20]) * weight_n[20] +
                                                log10( jointprob_missing_test_normalized[i,21]) * weight_n[21] +
                                                log10( jointprob_missing_test_normalized[i,30]) * weight_n[30] 
#                                                 log10( jointprob_missing_test_normalized[i,31]) * weight_n[31] + 
#                                                 log10(1/Upcandidates_attribute_test_missing[i,31]) * weight_n[31] 

  jointprob_missing_test_normalized[i,33] = jointprob_missing_test[i,33] 
#     
#   jointprob_missing_test_normalized[i,32] = 
#     jointprob_missing_test_normalized[i,1] * jointprob_missing_test_normalized[i,2] * jointprob_missing_test_normalized[i,3] * 
#     jointprob_missing_test_normalized[i,4] * jointprob_missing_test_normalized[i,5] * jointprob_missing_test_normalized[i,6] *  
#     jointprob_missing_test_normalized[i,7] * jointprob_missing_test_normalized[i,12] * jointprob_missing_test_normalized[i,13] *
#     jointprob_missing_test_normalized[i,14] * jointprob_missing_test_normalized[i,15] * jointprob_missing_test_normalized[i,16] *
#     jointprob_missing_test_normalized[i,17] * jointprob_missing_test_normalized[i,18] * jointprob_missing_test_normalized[i,19] *
#     jointprob_missing_test_normalized[i,20] * jointprob_missing_test_normalized[i,21] * jointprob_missing_test_normalized[i,30] *
#     jointprob_missing_test_normalized[i,31] 
}


# non-normalized
TargetTable_train <- TargetTable_train[,-5:-65]
TargetTable_train <- cbind(TargetTable_train, NA )
j <- 0


test <- cbind(jointprob_matching_train[,32] , jointprob_missing_train[,32]  )

for (i in 1:length(jointprob_matching_train[,1]) ){
  
  if ( as.numeric ( Downtarget_attributes_train [i,2] ) == 9 ) { 
  
        j <- j+1
        if (!is.na(jointprob_matching_train[i,32])) {
          
              if ( jointprob_matching_train[i,32] >  jointprob_missing_train[i,32] ) {
                TargetTable_train[j,5] <- TargetTable_train[j,4] 
              }
              else {
                TargetTable_train[j,5] <-  999
              }
        }
        
        else{
          
          TargetTable_train[j,5] <-  999
        }
    }
}



# non-normalized
TargetTable_test <- TargetTable_test[,-5:-65]
TargetTable_test <- cbind(TargetTable_test, NA )
j <- 0


test <- cbind(jointprob_matching_test[,32] , jointprob_missing_test[,32]  )

for (i in 1:length(jointprob_matching_test[,1]) ){
  
  if ( as.numeric ( Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    j <- j+1
    if (!is.na(jointprob_matching_test[i,32])) {
      
      if ( jointprob_matching_test[i,32] >  jointprob_missing_test[i,32] ) {
        TargetTable_test[j,5] <- TargetTable_test[j,4] 
      }
      else {
        TargetTable_test[j,5] <-  999
      }
    }
    
    else{
      
      TargetTable_test[j,5] <-  999
    }
  }
}


#normalized
TargetTable_train <- TargetTable_train[,-6:-65]
TargetTable_train <- cbind(TargetTable_train, NA )
j <- 0


for (i in 1:length(jointprob_matching_train_normalized[,1])){
  
  if ( as.numeric ( Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    j <- j+1
    if (!is.na(jointprob_matching_train_normalized[i,32])) {
      
      if ( jointprob_matching_train_normalized[i,32] >  jointprob_missing_train_normalized[i,32] ) {
        TargetTable_train[j,6] <- TargetTable_train[j,4] 
      }
      else {
        TargetTable_train[j,6] <-  999
      }
    }
    
    else{
      
      TargetTable_train[j,6] <-  999
    }
  }
}



#normalized
TargetTable_test <- TargetTable_test[,-6:-65]
TargetTable_test <- cbind(TargetTable_test, NA )
j <- 0


for (i in 1:length(jointprob_matching_test_normalized[,1])){
  
  if ( as.numeric ( Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    j <- j+1
    if (!is.na(jointprob_matching_test_normalized[i,32])) {
      
      if ( jointprob_matching_test_normalized[i,32] >  jointprob_missing_test_normalized[i,32] ) {
        TargetTable_test[j,6] <- TargetTable_test[j,4] 
      }
      else {
        TargetTable_test[j,6] <-  999
      }
    }
    
    else{
      
      TargetTable_test[j,6] <-  999
    }
  }
}

# performance
# non-normalized train
Target_obj_train  <- TargetTable_train[,2]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

p=5
matching_NN_train <- sum ( as.numeric ((TargetTable_train [,2]) == as.numeric (TargetTable_train [,p])) &
                             as.numeric (TargetTable_train [,2]) != 999)
missing_NN_train <- sum ( as.numeric (TargetTable_train[,5]) == c(999))

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


# normalized train
p=6
matching_NN_train <- sum ( as.numeric ((TargetTable_train [,2]) == as.numeric (TargetTable_train [,p])) &
                             as.numeric (TargetTable_train [,2]) != 999)
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



# non-normalized test
Target_obj_test  <- TargetTable_test[,2]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

p=5
matching_NN_test <- sum ( as.numeric ((TargetTable_test [,2]) == as.numeric (TargetTable_test [,p])) &
                            as.numeric (TargetTable_test [,2]) != 999)
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

ResultMissing_test <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                  matching_NN_test[[1]],  missing_NN_test[[1]],
                                  CMVeh_test[[1]], CVeh_test[[1]], MVeh_test[[1]],
                                  SIMR_test[[1]], SCMR_test[[1]], MMVeh_test[[1]], Veh_test[[1]], SER_test[[1]] )


# normalized test
p=6
matching_NN_test <- sum ( as.numeric ((TargetTable_test [,2]) == as.numeric (TargetTable_test [,p])) &
                            as.numeric (TargetTable_test [,2]) != 999)
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


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_04282014")
## end
View(TargetTable_train)
View(jointprob_matching_train_normalized)
View(jointprob_missing_train_normalized)
View(Upcandidates_attribute_train_missing)

row <- 85
objDown <- TargetTable_train[row,2]
Downheader_new [match(objDown, Downheader_new[,13]),]
objUp <- TargetTable_train[row,3]
Upheader_new [match(objUp, Upheader_new[,13]),]
