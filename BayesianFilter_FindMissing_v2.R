# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_05082014")


weight <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
weight_n <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

sigweight <- sigfeatidx
sigweight[sigweight==0] <-1
sigweight[sigweight==2] <-1
sigweight[sigweight==3] <-2

buf_matching <- 0.001 
bufsig <- 0.000000000001
buf_missing <- 0.001 
alphasig <- 0.01

# find missing
Upcandidates_attribute_train_missing <- list()
Upcandidates_attribute_train_missing_sig<- list()
Upcandidates_attribute_train_missing_temp <- data.frame()
Upcandidates_attribute_train_missing_sig_temp <- data.frame()

Attribute_difftemp_train_missing <- list()
Attribute_difftemp_train_missing_sig <- list()
Attribute_diff_nonnormal_train_missing <- list()
Attribute_diff_nonnormal_train_missing_sig <- list()



for (i in 1: length(Upcandidates_train)) {  
  
      for (j in 1:length(max_train_mat) ) { 
        for (k in 1: length(idxjointprob_train[1,])) {
            
           if (idxjointprob_train[i,k] != 999 & !is.na(idxjointprob_train[i,k]) ) {
            Upcandidates_attribute_train_missing_temp[k,j] <- 
              Attribute_diff_nonnormal_train[[i]][[ idxjointprob_train[i,k] ]] [j]            
            }
          
          else {
            Upcandidates_attribute_train_missing_temp[k,j] <- NA  
          }  
        }
      }
      
      for (j in 1 :50){
        for (k in 1: length(idxjointprob_train[1,])) {
          
          if (idxjointprob_train[i,k] != 999 & !is.na(idxjointprob_train[i,k]) ) {
            Upcandidates_attribute_train_missing_sig_temp[k,j] <-  
              Attribute_sig_nonnormal_train[[i]][[ idxjointprob_train[i,k] ]][[1]][j]      
          }
          else {
            Upcandidates_attribute_train_missing_sig_temp[k,j] <- NA  
          }
        }
      }
      
      Upcandidates_attribute_train_missing[[length (Upcandidates_attribute_train_missing) + 1]] <-  
        Upcandidates_attribute_train_missing_temp
      Upcandidates_attribute_train_missing_sig[[length (Upcandidates_attribute_train_missing_sig) + 1]] <-  
        Upcandidates_attribute_train_missing_sig_temp
     
}



# find missing - norm
Upcandidates_attribute_train_missing <- list()
Upcandidates_attribute_train_missing_sig <- list()
Upcandidates_attribute_train_missing_temp <- data.frame()
Upcandidates_attribute_train_missing_sig_temp <- data.frame()

Attribute_difftemp_train_missing <- list()
Attribute_difftemp_train_missing_sig <- list()
Attribute_diff_nonnormal_train_missing <- list()
Attribute_diff_nonnormal_train_missing_sig <- list()



for (i in 1: length(Upcandidates_train)) {  
  
  for (j in 1:length(max_train_mat) ) { 
    for (k in 1: length(idxjointprob_train[1,])) {
      
      if (idxjointprob_train[i,k] != 999 & !is.na(idxjointprob_train[i,k]) ) {
        Upcandidates_attribute_train_missing_temp[k,j] <- 
          Attribute_diff_nonnormal_train[[i]][[ idxjointprob_train[i,k] ]] [j]            
      }
      
      else {
        Upcandidates_attribute_train_missing_temp[k,j] <- NA  
      }  
    }
  }
  
  for (j in 1 :50){
    for (k in 1: length(idxjointprob_train[1,])) {
      
      if (idxjointprob_train[i,k] != 999 & !is.na(idxjointprob_train[i,k]) ) {
        Upcandidates_attribute_train_missing_sig_temp[k,j] <-  
          Attribute_sig_nonnormal_train[[i]][[ idxjointprob_train[i,k] ]][[1]][j]      
      }
      else {
        Upcandidates_attribute_train_missing_sig_temp[k,j] <- NA  
      }
    }
  }
  
  Upcandidates_attribute_train_missing[[length (Upcandidates_attribute_train_missing) + 1]] <-  
    Upcandidates_attribute_train_missing_temp
  Upcandidates_attribute_train_missing_sig[[length (Upcandidates_attribute_train_missing_sig) + 1]] <-  
    Upcandidates_attribute_train_missing_sig_temp
  
}

# train - matching
jointprob_matching_train <- list()
jointprob_matching_train_sig <- list()
jointprob_matching_train_result <- list()


jointprob_matching_train_temp <- data.frame()
jointprob_matching_train_sig_temp <- data.frame()
jointprob_matching_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train[[length(jointprob_matching_train) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
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
                  jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_c[[m]], 
                                                   normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                                   Upcandidates_attribute_train_missing[[i]][j,m] )$y )
                  jointprob_matching_train_temp[j,m] [is.na(jointprob_matching_train_temp[j,m])] <- buf_matching 
                }
                
    
    #         option 3 - hitogram
#             if ( m %in% class9idxprob) {
#            
#                   if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[[i]][m,j] & 
#                                           Upcandidates_attribute_train_missing[[i]][m,j]   <  histdensity_c[[m]][,2])) > 0 )
#                     
#                   {
#                     jointprob_matching_train_temp[m,j]  <- 
#                       histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[[i]][m,j] &
#                                                    Upcandidates_attribute_train_missing[[i]][m,j] < histdensity_c[[m]][,2]),3]  
#                   }
#                
#             
#                   else {
#                     jointprob_matching_train_temp[m,j] <- buf
#                   }
#              }
#      
            else {
              jointprob_matching_train_temp[j,m] <- 99999
            }
      }
#         
#         for (n in 1:50){   
#           if (length (which(histdensity_c_sig[[n]][,1] < Upcandidates_attribute_train_missing_sig[[i]][n,j] &
#               Upcandidates_attribute_train_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2])) > 0) 
#                 {
#                   jointprob_matching_train_sig_temp[n,j] <-             
#                     histdensity_c_sig[[n]][ which (histdensity_c_sig[[n]][,1] <  Upcandidates_attribute_train_missing_sig[[i]][n,j] &
#                       Upcandidates_attribute_train_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2]),3]
#                 }
# 
#           else {
#             jointprob_matching_train_sig_temp[n,j] <- bufsig
#           }
#          
#         }
#         
        for (n in 1: 50) {
  
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                                   normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                   Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
  
  
     jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <- 0.0000000001

     jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_sig 
     jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <- buf_sig 
     jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf
     jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf


    
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
      
      jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * weight[1] +  
                                                log10( jointprob_matching_train_temp[j,2]) * weight[2] +
                                                log10( jointprob_matching_train_temp[j,3]) * weight[3] +
                                                log10( jointprob_matching_train_temp[j,4]) * weight[4] +
                                                log10( jointprob_matching_train_temp[j,5]) * weight[5] +
                                                log10( jointprob_matching_train_temp[j,6]) * weight[6] +
                                                log10( jointprob_matching_train_temp[j,7]) * weight[7] +
                                                log10( jointprob_matching_train_temp[j,13]) * weight[13] +
                                                log10( jointprob_matching_train_temp[j,14]) * weight[14] +
                                                log10( jointprob_matching_train_temp[j,15]) * weight[15] +
                                                log10( jointprob_matching_train_temp[j,16]) * weight[16] +
                                                log10( jointprob_matching_train_temp[j,17]) * weight[17] +
                                                log10( jointprob_matching_train_temp[j,18]) * weight[18] +
                                                log10( jointprob_matching_train_temp[j,19]) * weight[19] +
                                                log10( jointprob_matching_train_temp[j,20]) * weight[20] +
                                                log10( jointprob_matching_train_temp[j,21]) * weight[21] +
                                                log10( jointprob_matching_train_temp[j,30]) * weight[30] 
#                                               log10(    1 - ( alphasig * jointprob_matching_train[i,31] ) )
#                                               log10( jointprob_matching_train[i,31]) * weight[31] 
#                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
#       jointprob_matching_train[i,33] <-  log10( 1/ jointprob_matching_train[i,31]) * weight[31]  
      jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * weight[31]
      jointprob_matching_train_result_temp[j,3] <- 0

      for ( n in 1: length(sigweight)){
        jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
          log10(jointprob_matching_train_sig_temp[j,n]) * sigweight[n]
      }

      jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
      jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
     
    }
       


    jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- jointprob_matching_train_result_temp
    jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
    jointprob_matching_train[[length( jointprob_matching_train) +1]] <-  jointprob_matching_train_temp

    jointprob_matching_train_temp <- data.frame()
    jointprob_matching_train_sig_temp <- data.frame()
    jointprob_matching_train_result_temp <- data.frame()

    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_train[[length(jointprob_matching_train) + 1 ]] <-  NA
    jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
    jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- NA
  }
  
}

# train - missing

jointprob_missing_train <- list()
jointprob_missing_train_sig <- list()
jointprob_missing_train_result <- list()

jointprob_missing_train_temp <- data.frame()
jointprob_missing_train_sig_temp <- data.frame()
jointprob_missing_train_result_temp <- data.frame()
 


for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train[[length(jointprob_missing_train) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 
          #       for (m in 1: 30) { 
          
          # option 1 - non parametric
          #         if ( m %in% class9idxprob) {
          #           jointprob_missing_train[i,m] <- as.numeric ( approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
          #                                                                Upcandidates_attribute_train_missing[i,m] )$y )
          #         }
          
          # another option 1 - non parametric but smoothing
          #         if ( m %in% class9idxprob) {
          #           jointprob_missing_train[i,m] <- as.numeric ( approx( density_smooth_hist_mat_c[[m]]$x, density_smooth_hist_mat_c[[m]]$y,
          #                                                                 Upcandidates_attribute_train_missing[i,m] )$y )
          #           jointprob_missing_train[i,m] [is.na(jointprob_missing_train[i,m])] <- buf 
          #         }
          
          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c[[m]], 
                                                                      normal_nonmat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                                                      Upcandidates_attribute_train_missing[[i]][j,m] )$y )
            jointprob_missing_train_temp[j,m] [is.na(jointprob_missing_train_temp[j,m])] <- buf_missing 
          }
          
          
          #         option 3 - hitogram
          #             if ( m %in% class9idxprob) {
          #            
          #                   if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[[i]][m,j] & 
          #                                           Upcandidates_attribute_train_missing[[i]][m,j]   <  histdensity_c[[m]][,2])) > 0 )
          #                     
          #                   {
          #                     jointprob_missing_train_temp[m,j]  <- 
          #                       histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_train_missing[[i]][m,j] &
          #                                                    Upcandidates_attribute_train_missing[[i]][m,j] < histdensity_c[[m]][,2]),3]  
          #                   }
          #                
          #             
          #                   else {
          #                     jointprob_missing_train_temp[m,j] <- buf
          #                   }
          #              }
          #      
          else {
            jointprob_missing_train_temp[j,m] <- 99999
          }
        }
        #         
        #         for (n in 1:50){   
        #           if (length (which(histdensity_c_sig[[n]][,1] < Upcandidates_attribute_train_missing_sig[[i]][n,j] &
        #               Upcandidates_attribute_train_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2])) > 0) 
        #                 {
        #                   jointprob_missing_train_sig_temp[n,j] <-             
        #                     histdensity_c_sig[[n]][ which (histdensity_c_sig[[n]][,1] <  Upcandidates_attribute_train_missing_sig[[i]][n,j] &
        #                       Upcandidates_attribute_train_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2]),3]
        #                 }
        # 
        #           else {
        #             jointprob_missing_train_sig_temp[n,j] <- bufsig
        #           }
        #          
        #         }
        #         
        for (n in 1: 50) {
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                                                                         normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <- 0.0000000001
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf
        

        
        #         jointprob_missing_train[i,31] <- (Upcandidates_attribute_train_missing[i,31]) 
        
        
        
        # option 1
        #   jointprob_missing_train[i,32] <-  jointprob_missing_train[i,1] * jointprob_missing_train[i,2] * jointprob_missing_train[i,3] *  jointprob_missing_train[i,4] * 
        #     jointprob_missing_train[i,5] * jointprob_missing_train[i,6] * jointprob_missing_train[i,7] *  jointprob_missing_train[i,12] * 
        #     jointprob_missing_train[i,13] * jointprob_missing_train[i,14] *  jointprob_missing_train[i,15] * jointprob_missing_train[i,16] *
        #     jointprob_missing_train[i,17] * jointprob_missing_train[i,18] *  jointprob_missing_train[i,19] *  jointprob_missing_train[i,20] *
        #     jointprob_missing_train[i,21] *   jointprob_missing_train[i,30]  *
        #     jointprob_missing_train[i,31] 
        #   
        
        # option 2
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * weight[1] +  
          log10( jointprob_missing_train_temp[j,2]) * weight[2] +
          log10( jointprob_missing_train_temp[j,3]) * weight[3] +
          log10( jointprob_missing_train_temp[j,4]) * weight[4] +
          log10( jointprob_missing_train_temp[j,5]) * weight[5] +
          log10( jointprob_missing_train_temp[j,6]) * weight[6] +
          log10( jointprob_missing_train_temp[j,7]) * weight[7] +
          log10( jointprob_missing_train_temp[j,13]) * weight[13] +
          log10( jointprob_missing_train_temp[j,14]) * weight[14] +
          log10( jointprob_missing_train_temp[j,15]) * weight[15] +
          log10( jointprob_missing_train_temp[j,16]) * weight[16] +
          log10( jointprob_missing_train_temp[j,17]) * weight[17] +
          log10( jointprob_missing_train_temp[j,18]) * weight[18] +
          log10( jointprob_missing_train_temp[j,19]) * weight[19] +
          log10( jointprob_missing_train_temp[j,20]) * weight[20] +
          log10( jointprob_missing_train_temp[j,21]) * weight[21] +
          log10( jointprob_missing_train_temp[j,30]) * weight[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_train[i,31] ) )
        #                                               log10( jointprob_missing_train[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
        #       jointprob_missing_train[i,33] <-  log10( 1/ jointprob_missing_train[i,31]) * weight[31]  
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * weight[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        
      }
    
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train[[length( jointprob_missing_train) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_train[[length(jointprob_missing_train) + 1 ]] <-  NA
    jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
    jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- NA
  }
  
}

# joint prob - norm


# train - matching
jointprob_matching_train_n <- list()
jointprob_matching_train_sig <- list()
jointprob_matching_train_result_n <- list()


jointprob_matching_train_temp <- data.frame()
jointprob_matching_train_sig_temp <- data.frame()
jointprob_matching_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_n[[length(jointprob_matching_train_n) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 

          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_n[[m]], 
             normal_mat_n[[m]]  * multiplier_hist_mat_n[[m]][ which.min(is.na( multiplier_hist_mat_n[[m]] ) ) ] ,
            (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_mat[m] ) / ( max_train_mat[m] - min_train_mat[m])) )$y )
            
            jointprob_matching_train_temp[j,m] [is.na(jointprob_matching_train_temp[j,m])] <- buf_matching 
          }

          else {
            jointprob_matching_train_temp[j,m] <- 99999
          }
        }

        for (n in 1: 50) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                                                                          normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                          Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <- 0.0000000001
        
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <- buf_sig 
        jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf
        jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf
        

        
        jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * weight[1] +  
          log10( jointprob_matching_train_temp[j,2]) * weight[2] +
          log10( jointprob_matching_train_temp[j,3]) * weight[3] +
          log10( jointprob_matching_train_temp[j,4]) * weight[4] +
          log10( jointprob_matching_train_temp[j,5]) * weight[5] +
          log10( jointprob_matching_train_temp[j,6]) * weight[6] +
          log10( jointprob_matching_train_temp[j,7]) * weight[7] +
          log10( jointprob_matching_train_temp[j,13]) * weight[13] +
          log10( jointprob_matching_train_temp[j,14]) * weight[14] +
          log10( jointprob_matching_train_temp[j,15]) * weight[15] +
          log10( jointprob_matching_train_temp[j,16]) * weight[16] +
          log10( jointprob_matching_train_temp[j,17]) * weight[17] +
          log10( jointprob_matching_train_temp[j,18]) * weight[18] +
          log10( jointprob_matching_train_temp[j,19]) * weight[19] +
          log10( jointprob_matching_train_temp[j,20]) * weight[20] +
          log10( jointprob_matching_train_temp[j,21]) * weight[21] +
          log10( jointprob_matching_train_temp[j,30]) * weight[30] 
  
        jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * weight[31]
        jointprob_matching_train_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
        jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
        
      }
      
      
      
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- jointprob_matching_train_result_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train_n[[length( jointprob_matching_train_n) +1]] <-  jointprob_matching_train_temp
      
      jointprob_matching_train_temp <- data.frame()
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_train_n[[length(jointprob_matching_train_n) + 1 ]] <-  NA
    jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
    jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- NA
  }
  
}

# train - missing

jointprob_missing_train_n <- list()
jointprob_missing_train_sig <- list()
jointprob_missing_train_result_n <- list()

jointprob_missing_train_temp <- data.frame()
jointprob_missing_train_sig_temp <- data.frame()
jointprob_missing_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_n[[length(jointprob_missing_train_n) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 

          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n[[m]], 
                 normal_nonmat_n[[m]]  * multiplier_hist_mat_n[[m]][ which.min(is.na( multiplier_hist_mat_n[[m]] ) ) ] ,
                 (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_nonmat[m] ) / ( max_train_nonmat[m] - min_train_nonmat[m])) )$y )
            jointprob_missing_train_temp[j,m] [is.na(jointprob_missing_train_temp[j,m])] <- buf_missing 
          }

          else {
            jointprob_missing_train_temp[j,m] <- 99999
          }
        }

        for (n in 1: 50) {
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                                                                         normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <- 0.0000000001
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf
        
    
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * weight[1] +  
          log10( jointprob_missing_train_temp[j,2]) * weight[2] +
          log10( jointprob_missing_train_temp[j,3]) * weight[3] +
          log10( jointprob_missing_train_temp[j,4]) * weight[4] +
          log10( jointprob_missing_train_temp[j,5]) * weight[5] +
          log10( jointprob_missing_train_temp[j,6]) * weight[6] +
          log10( jointprob_missing_train_temp[j,7]) * weight[7] +
          log10( jointprob_missing_train_temp[j,13]) * weight[13] +
          log10( jointprob_missing_train_temp[j,14]) * weight[14] +
          log10( jointprob_missing_train_temp[j,15]) * weight[15] +
          log10( jointprob_missing_train_temp[j,16]) * weight[16] +
          log10( jointprob_missing_train_temp[j,17]) * weight[17] +
          log10( jointprob_missing_train_temp[j,18]) * weight[18] +
          log10( jointprob_missing_train_temp[j,19]) * weight[19] +
          log10( jointprob_missing_train_temp[j,20]) * weight[20] +
          log10( jointprob_missing_train_temp[j,21]) * weight[21] +
          log10( jointprob_missing_train_temp[j,30]) * weight[30] 
  
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * weight[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        
      }
      
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train_n[[length( jointprob_missing_train_n) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_train_n[[length(jointprob_missing_train_n) + 1 ]] <-  NA
    jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
    jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- NA
  }
  
}


jointprob_missing_train <- jointprob_missing_train_n
jointprob_matching_train <- jointprob_matching_train_n

# train matching and missing prob normalization
jointprob_matching_train_normalized <- list()
jointprob_missing_train_normalized <- list()

jointprob_matching_train_normalized_temp <- data.frame()
jointprob_missing_train_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_train_missing)){
  
 if ( !is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      for (j in 1 : length(Upcandidates_attribute_train_missing[[i]][,1] )){
        for (k in 1 : length(Upcandidates_attribute_train_missing[[i]] )){
        
          jointprob_matching_train_normalized_temp[j,k] = 
            jointprob_matching_train[[i]][j,k]  / (  jointprob_matching_train[[i]][j,k] +  jointprob_missing_train[[i]][j,k] )
          
          jointprob_missing_train_normalized_temp[j,k] = 
            jointprob_missing_train[[i]][j,k]  / (  jointprob_matching_train[[i]][j,k] +  jointprob_missing_train[[i]][j,k] )
          
        }  
      } 
   }
      

 else{
   jointprob_matching_train_normalized_temp[j,k]  <- NA
   jointprob_missing_train_normalized_temp[j,k]  <- NA
 }
 
 
 jointprob_matching_train_normalized[[length( jointprob_matching_train_normalized) + 1 ]]  <- jointprob_matching_train_normalized_temp
 jointprob_missing_train_normalized[[length( jointprob_missing_train_normalized) + 1 ]]  <- jointprob_missing_train_normalized_temp
 
}



# normalized train matching
jointprob_matching_train_result_normalized_temp <- data.frame()
jointprob_matching_train_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
    
        for (n in 1: 50) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_sig 
        }
        
        
        
      
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <- buf_sig 
  
    
        # option 2
        
        jointprob_matching_train_result_normalized_temp[j,1] <-   log10( jointprob_matching_train_normalized[[i]][j,1]) * weight[1] +  
          log10( jointprob_matching_train_normalized[[i]][j,2]) * weight[2] +
          log10( jointprob_matching_train_normalized[[i]][j,3]) * weight[3] +
          log10( jointprob_matching_train_normalized[[i]][j,4]) * weight[4] +
          log10( jointprob_matching_train_normalized[[i]][j,5]) * weight[5] +
          log10( jointprob_matching_train_normalized[[i]][j,6]) * weight[6] +
          log10( jointprob_matching_train_normalized[[i]][j,7]) * weight[7] +
          log10( jointprob_matching_train_normalized[[i]][j,13]) * weight[13] +
          log10( jointprob_matching_train_normalized[[i]][j,14]) * weight[14] +
          log10( jointprob_matching_train_normalized[[i]][j,15]) * weight[15] +
          log10( jointprob_matching_train_normalized[[i]][j,16]) * weight[16] +
          log10( jointprob_matching_train_normalized[[i]][j,17]) * weight[17] +
          log10( jointprob_matching_train_normalized[[i]][j,18]) * weight[18] +
          log10( jointprob_matching_train_normalized[[i]][j,19]) * weight[19] +
          log10( jointprob_matching_train_normalized[[i]][j,20]) * weight[20] +
          log10( jointprob_matching_train_normalized[[i]][j,21]) * weight[21] +
          log10( jointprob_matching_train_normalized[[i]][j,30]) * weight[30] 
        
        jointprob_matching_train_result_normalized_temp[j,2] <- log10( jointprob_matching_train[[i]][j,31]) * weight[31]
        jointprob_matching_train_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_matching_train_result_normalized_temp[j,3] <- jointprob_matching_train_result_normalized_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_matching_train_result_normalized_temp[j,4] <-  jointprob_matching_train_result_normalized_temp[j,1] + 
                                                                    jointprob_matching_train_result_normalized_temp[j,2] 
        jointprob_matching_train_result_normalized_temp[j,5] <-  jointprob_matching_train_result_normalized_temp[j,1] +
                                                                    jointprob_matching_train_result_normalized_temp[j,3]  
        
      }
     
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- 
               jointprob_matching_train_result_normalized_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train[[length( jointprob_matching_train) +1]] <-  jointprob_matching_train_temp
      
    
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_normalized_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
  
    jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
    jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- NA
  }
  
}


# normalized train missing

jointprob_missing_train_result_normalized_temp <- data.frame()
jointprob_missing_train_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result_normalization[[length(jointprob_missing_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        
        for (n in 1: 50) {
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                        normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                        Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_sig 
        }
        
        
        
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_sig 
        
        
        # option 2
        
        jointprob_missing_train_result_normalized_temp[j,1] <-   log10( jointprob_missing_train_normalized[[i]][j,1]) * weight[1] +  
          log10( jointprob_missing_train_normalized[[i]][j,2]) * weight[2] +
          log10( jointprob_missing_train_normalized[[i]][j,3]) * weight[3] +
          log10( jointprob_missing_train_normalized[[i]][j,4]) * weight[4] +
          log10( jointprob_missing_train_normalized[[i]][j,5]) * weight[5] +
          log10( jointprob_missing_train_normalized[[i]][j,6]) * weight[6] +
          log10( jointprob_missing_train_normalized[[i]][j,7]) * weight[7] +
          log10( jointprob_missing_train_normalized[[i]][j,13]) * weight[13] +
          log10( jointprob_missing_train_normalized[[i]][j,14]) * weight[14] +
          log10( jointprob_missing_train_normalized[[i]][j,15]) * weight[15] +
          log10( jointprob_missing_train_normalized[[i]][j,16]) * weight[16] +
          log10( jointprob_missing_train_normalized[[i]][j,17]) * weight[17] +
          log10( jointprob_missing_train_normalized[[i]][j,18]) * weight[18] +
          log10( jointprob_missing_train_normalized[[i]][j,19]) * weight[19] +
          log10( jointprob_missing_train_normalized[[i]][j,20]) * weight[20] +
          log10( jointprob_missing_train_normalized[[i]][j,21]) * weight[21] +
          log10( jointprob_missing_train_normalized[[i]][j,30]) * weight[30] 
        
        jointprob_missing_train_result_normalized_temp[j,2] <- log10( jointprob_missing_train[[i]][j,31]) * weight[31]
        jointprob_missing_train_result_normalized_temp[j,3] <- 0

    for ( n in 1: length(sigweight)){
      jointprob_missing_train_result_normalized_temp[j,3] <- jointprob_missing_train_result_normalized_temp[j,3] + 
        log10(jointprob_missing_train_sig_temp[j,n]) * sigweight[n]
    }
    
    jointprob_missing_train_result_normalized_temp[j,4] <-  jointprob_missing_train_result_normalized_temp[j,1] + 
      jointprob_missing_train_result_normalized_temp[j,2] 
    jointprob_missing_train_result_normalized_temp[j,5] <-  jointprob_missing_train_result_normalized_temp[j,1] +
      jointprob_missing_train_result_normalized_temp[j,3]  
    
          }

  

    jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- 
      jointprob_missing_train_result_normalized_temp
    jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
    jointprob_missing_train[[length( jointprob_missing_train) +1]] <-  jointprob_missing_train_temp
    
    
    jointprob_missing_train_sig_temp <- data.frame()
    jointprob_missing_train_result_normalized_temp <- data.frame()
    
        }
      }
    
    else # class is not 9
      
    {
      
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- NA
    }

}




# test - matching


# find missing
Upcandidates_attribute_test_missing <- list()
Upcandidates_attribute_test_missing_sig<- list()
Upcandidates_attribute_test_missing_temp <- data.frame()
Upcandidates_attribute_test_missing_sig_temp <- data.frame()

Attribute_difftemp_test_missing <- list()
Attribute_difftemp_test_missing_sig <- list()
Attribute_diff_nonnormal_test_missing <- list()
Attribute_diff_nonnormal_test_missing_sig <- list()



for (i in 1: length(Upcandidates_test)) {  
  
  for (j in 1:31 ) { 
    for (k in 1: length(idxjointprob_test[1,])) {
      
      if (idxjointprob_test[i,k] != 999 & !is.na(idxjointprob_test[i,k]) ) {
        Upcandidates_attribute_test_missing_temp[k,j] <- 
          Attribute_diff_nonnormal_test[[i]][[ idxjointprob_test[i,k] ]] [j]            
      }
      
      else {
        Upcandidates_attribute_test_missing_temp[k,j] <- NA  
      }  
    }
  }
  
  for (j in 1 :50){
    for (k in 1: length(idxjointprob_test[1,])) {
      
      if (idxjointprob_test[i,k] != 999 & !is.na(idxjointprob_test[i,k]) ) {
        Upcandidates_attribute_test_missing_sig_temp[k,j] <-  
          Attribute_sig_nonnormal_test[[i]][[ idxjointprob_test[i,k] ]][[1]][j]      
      }
      else {
        Upcandidates_attribute_test_missing_sig_temp[k,j] <- NA  
      }
    }
  }
  
  Upcandidates_attribute_test_missing[[length (Upcandidates_attribute_test_missing) + 1]] <-  
    Upcandidates_attribute_test_missing_temp
  Upcandidates_attribute_test_missing_sig[[length (Upcandidates_attribute_test_missing_sig) + 1]] <-  
    Upcandidates_attribute_test_missing_sig_temp
  
}

# joint prob 
jointprob_matching_test <- list()
jointprob_matching_test_sig <- list()
jointprob_matching_test_result <- list()


jointprob_matching_test_temp <- data.frame()
jointprob_matching_test_sig_temp <- data.frame()
jointprob_matching_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test[[length(jointprob_matching_test) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 
          #       for (m in 1: 30) { 
          
          # option 1 - non parametric
          #         if ( m %in% class9idxprob) {
          #           jointprob_matching_test[i,m] <- as.numeric ( approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
          #                                                                Upcandidates_attribute_test_missing[i,m] )$y )
          #         }
          
          # another option 1 - non parametric but smoothing
          #         if ( m %in% class9idxprob) {
          #           jointprob_matching_test[i,m] <- as.numeric ( approx( density_smooth_hist_mat_c[[m]]$x, density_smooth_hist_mat_c[[m]]$y,
          #                                                                 Upcandidates_attribute_test_missing[i,m] )$y )
          #           jointprob_matching_test[i,m] [is.na(jointprob_matching_test[i,m])] <- buf 
          #         }
          
          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_c[[m]], 
                                        normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                        Upcandidates_attribute_test_missing[[i]][j,m] )$y )
            jointprob_matching_test_temp[j,m] [is.na(jointprob_matching_test_temp[j,m])] <- buf_matching 
          }
          
          
          #         option 3 - hitogram
          #             if ( m %in% class9idxprob) {
          #            
          #                   if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[[i]][m,j] & 
          #                                           Upcandidates_attribute_test_missing[[i]][m,j]   <  histdensity_c[[m]][,2])) > 0 )
          #                     
          #                   {
          #                     jointprob_matching_test_temp[m,j]  <- 
          #                       histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[[i]][m,j] &
          #                                                    Upcandidates_attribute_test_missing[[i]][m,j] < histdensity_c[[m]][,2]),3]  
          #                   }
          #                
          #             
          #                   else {
          #                     jointprob_matching_test_temp[m,j] <- buf
          #                   }
          #              }
          #      
          else {
            jointprob_matching_test_temp[j,m] <- 99999
          }
        }
        #         
        #         for (n in 1:50){   
        #           if (length (which(histdensity_c_sig[[n]][,1] < Upcandidates_attribute_test_missing_sig[[i]][n,j] &
        #               Upcandidates_attribute_test_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2])) > 0) 
        #                 {
        #                   jointprob_matching_test_sig_temp[n,j] <-             
        #                     histdensity_c_sig[[n]][ which (histdensity_c_sig[[n]][,1] <  Upcandidates_attribute_test_missing_sig[[i]][n,j] &
        #                       Upcandidates_attribute_test_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2]),3]
        #                 }
        # 
        #           else {
        #             jointprob_matching_test_sig_temp[n,j] <- bufsig
        #           }
        #          
        #         }
        #         
        for (n in 1: 50) {
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                                                                         normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <- 0.0000000001
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf
        
        #         jointprob_matching_test[i,31] <- (Upcandidates_attribute_test_missing[i,31]) 
        
        
        
        # option 1
        #   jointprob_matching_test[i,32] <-  jointprob_matching_test[i,1] * jointprob_matching_test[i,2] * jointprob_matching_test[i,3] *  jointprob_matching_test[i,4] * 
        #     jointprob_matching_test[i,5] * jointprob_matching_test[i,6] * jointprob_matching_test[i,7] *  jointprob_matching_test[i,12] * 
        #     jointprob_matching_test[i,13] * jointprob_matching_test[i,14] *  jointprob_matching_test[i,15] * jointprob_matching_test[i,16] *
        #     jointprob_matching_test[i,17] * jointprob_matching_test[i,18] *  jointprob_matching_test[i,19] *  jointprob_matching_test[i,20] *
        #     jointprob_matching_test[i,21] *   jointprob_matching_test[i,30]  *
        #     jointprob_matching_test[i,31] 
        #   
        
        # option 2
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * weight[1] +  
          log10( jointprob_matching_test_temp[j,2]) * weight[2] +
          log10( jointprob_matching_test_temp[j,3]) * weight[3] +
          log10( jointprob_matching_test_temp[j,4]) * weight[4] +
          log10( jointprob_matching_test_temp[j,5]) * weight[5] +
          log10( jointprob_matching_test_temp[j,6]) * weight[6] +
          log10( jointprob_matching_test_temp[j,7]) * weight[7] +
          log10( jointprob_matching_test_temp[j,13]) * weight[13] +
          log10( jointprob_matching_test_temp[j,14]) * weight[14] +
          log10( jointprob_matching_test_temp[j,15]) * weight[15] +
          log10( jointprob_matching_test_temp[j,16]) * weight[16] +
          log10( jointprob_matching_test_temp[j,17]) * weight[17] +
          log10( jointprob_matching_test_temp[j,18]) * weight[18] +
          log10( jointprob_matching_test_temp[j,19]) * weight[19] +
          log10( jointprob_matching_test_temp[j,20]) * weight[20] +
          log10( jointprob_matching_test_temp[j,21]) * weight[21] +
          log10( jointprob_matching_test_temp[j,30]) * weight[30] 
        #                                               log10(    1 - ( alphasig * jointprob_matching_test[i,31] ) )
        #                                               log10( jointprob_matching_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_matching_test[i,33] <-  log10( 1/ jointprob_matching_test[i,31]) * weight[31]  
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * weight[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        
      }
    
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test[[length( jointprob_matching_test) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_test[[length(jointprob_matching_test) + 1 ]] <-  NA
    jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
    jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- NA
  }
  
}

# test - missing

jointprob_missing_test <- list()
jointprob_missing_test_sig <- list()
jointprob_missing_test_result <- list()

jointprob_missing_test_temp <- data.frame()
jointprob_missing_test_sig_temp <- data.frame()
jointprob_missing_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test[[length(jointprob_missing_test) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 
          #       for (m in 1: 30) { 
          
          # option 1 - non parametric
          #         if ( m %in% class9idxprob) {
          #           jointprob_missing_test[i,m] <- as.numeric ( approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
          #                                                                Upcandidates_attribute_test_missing[i,m] )$y )
          #         }
          
          # another option 1 - non parametric but smoothing
          #         if ( m %in% class9idxprob) {
          #           jointprob_missing_test[i,m] <- as.numeric ( approx( density_smooth_hist_mat_c[[m]]$x, density_smooth_hist_mat_c[[m]]$y,
          #                                                                 Upcandidates_attribute_test_missing[i,m] )$y )
          #           jointprob_missing_test[i,m] [is.na(jointprob_missing_test[i,m])] <- buf 
          #         }
          
          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c[[m]], 
                                         normal_nonmat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                         Upcandidates_attribute_test_missing[[i]][j,m] )$y )
            jointprob_missing_test_temp[j,m] [is.na(jointprob_missing_test_temp[j,m])] <- buf_missing 
          }
          
          
          #         option 3 - hitogram
          #             if ( m %in% class9idxprob) {
          #            
          #                   if (  length ( which( histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[[i]][m,j] & 
          #                                           Upcandidates_attribute_test_missing[[i]][m,j]   <  histdensity_c[[m]][,2])) > 0 )
          #                     
          #                   {
          #                     jointprob_missing_test_temp[m,j]  <- 
          #                       histdensity_c[[m]][ which (histdensity_c[[m]][,1] <  Upcandidates_attribute_test_missing[[i]][m,j] &
          #                                                    Upcandidates_attribute_test_missing[[i]][m,j] < histdensity_c[[m]][,2]),3]  
          #                   }
          #                
          #             
          #                   else {
          #                     jointprob_missing_test_temp[m,j] <- buf
          #                   }
          #              }
          #      
          else {
            jointprob_missing_test_temp[j,m] <- 99999
          }
        }
        #         
        #         for (n in 1:50){   
        #           if (length (which(histdensity_c_sig[[n]][,1] < Upcandidates_attribute_test_missing_sig[[i]][n,j] &
        #               Upcandidates_attribute_test_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2])) > 0) 
        #                 {
        #                   jointprob_missing_test_sig_temp[n,j] <-             
        #                     histdensity_c_sig[[n]][ which (histdensity_c_sig[[n]][,1] <  Upcandidates_attribute_test_missing_sig[[i]][n,j] &
        #                       Upcandidates_attribute_test_missing_sig[[i]][n,j] < histdensity_c_sig[[n]][,2]),3]
        #                 }
        # 
        #           else {
        #             jointprob_missing_test_sig_temp[n,j] <- bufsig
        #           }
        #          
        #         }
        #         
        for (n in 1: 50) {
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                                                                        normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- 0.0000000001
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf
        
        #         jointprob_missing_test[i,31] <- (Upcandidates_attribute_test_missing[i,31]) 
        
        
        
        # option 1
        #   jointprob_missing_test[i,32] <-  jointprob_missing_test[i,1] * jointprob_missing_test[i,2] * jointprob_missing_test[i,3] *  jointprob_missing_test[i,4] * 
        #     jointprob_missing_test[i,5] * jointprob_missing_test[i,6] * jointprob_missing_test[i,7] *  jointprob_missing_test[i,12] * 
        #     jointprob_missing_test[i,13] * jointprob_missing_test[i,14] *  jointprob_missing_test[i,15] * jointprob_missing_test[i,16] *
        #     jointprob_missing_test[i,17] * jointprob_missing_test[i,18] *  jointprob_missing_test[i,19] *  jointprob_missing_test[i,20] *
        #     jointprob_missing_test[i,21] *   jointprob_missing_test[i,30]  *
        #     jointprob_missing_test[i,31] 
        #   
        
        # option 2
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * weight[1] +  
          log10( jointprob_missing_test_temp[j,2]) * weight[2] +
          log10( jointprob_missing_test_temp[j,3]) * weight[3] +
          log10( jointprob_missing_test_temp[j,4]) * weight[4] +
          log10( jointprob_missing_test_temp[j,5]) * weight[5] +
          log10( jointprob_missing_test_temp[j,6]) * weight[6] +
          log10( jointprob_missing_test_temp[j,7]) * weight[7] +
          log10( jointprob_missing_test_temp[j,13]) * weight[13] +
          log10( jointprob_missing_test_temp[j,14]) * weight[14] +
          log10( jointprob_missing_test_temp[j,15]) * weight[15] +
          log10( jointprob_missing_test_temp[j,16]) * weight[16] +
          log10( jointprob_missing_test_temp[j,17]) * weight[17] +
          log10( jointprob_missing_test_temp[j,18]) * weight[18] +
          log10( jointprob_missing_test_temp[j,19]) * weight[19] +
          log10( jointprob_missing_test_temp[j,20]) * weight[20] +
          log10( jointprob_missing_test_temp[j,21]) * weight[21] +
          log10( jointprob_missing_test_temp[j,30]) * weight[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_test[i,31] ) )
        #                                               log10( jointprob_missing_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_missing_test[i,33] <-  log10( 1/ jointprob_missing_test[i,31]) * weight[31]  
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * weight[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        
      }
    
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test[[length( jointprob_missing_test) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_test[[length(jointprob_missing_test) + 1 ]] <-  NA
    jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
    jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- NA
  }
  
}


# normalized


# joint prob 
jointprob_matching_test_n <- list()
jointprob_matching_test_sig <- list()
jointprob_matching_test_result_n <- list()


jointprob_matching_test_temp <- data.frame()
jointprob_matching_test_sig_temp <- data.frame()
jointprob_matching_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_n[[length(jointprob_matching_test_n) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 
       
          
          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_n[[m]], 
                   normal_mat_n[[m]]  * multiplier_hist_mat_n[[m]][ which.min(is.na( multiplier_hist_mat_n[[m]] ) ) ] ,
                   Upcandidates_attribute_test_missing[[i]][j,m] )$y )
            jointprob_matching_test_temp[j,m] [is.na(jointprob_matching_test_temp[j,m])] <- buf_matching 
          }
          
          
       
          else {
            jointprob_matching_test_temp[j,m] <- 99999
          }
        }
 
              
        for (n in 1: 50) {
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                  normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                  Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <- 0.0000000001
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf
        
    
        # option 2
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * weight[1] +  
          log10( jointprob_matching_test_temp[j,2]) * weight[2] +
          log10( jointprob_matching_test_temp[j,3]) * weight[3] +
          log10( jointprob_matching_test_temp[j,4]) * weight[4] +
          log10( jointprob_matching_test_temp[j,5]) * weight[5] +
          log10( jointprob_matching_test_temp[j,6]) * weight[6] +
          log10( jointprob_matching_test_temp[j,7]) * weight[7] +
          log10( jointprob_matching_test_temp[j,13]) * weight[13] +
          log10( jointprob_matching_test_temp[j,14]) * weight[14] +
          log10( jointprob_matching_test_temp[j,15]) * weight[15] +
          log10( jointprob_matching_test_temp[j,16]) * weight[16] +
          log10( jointprob_matching_test_temp[j,17]) * weight[17] +
          log10( jointprob_matching_test_temp[j,18]) * weight[18] +
          log10( jointprob_matching_test_temp[j,19]) * weight[19] +
          log10( jointprob_matching_test_temp[j,20]) * weight[20] +
          log10( jointprob_matching_test_temp[j,21]) * weight[21] +
          log10( jointprob_matching_test_temp[j,30]) * weight[30] 
    
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * weight[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        
      }
      
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test_n[[length( jointprob_matching_test_n) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_matching_test_n[[length(jointprob_matching_test_n) + 1 ]] <-  NA
    jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
    jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- NA
  }
  
}

# test - missing

jointprob_missing_test_n <- list()
jointprob_missing_test_sig <- list()
jointprob_missing_test_result_n <- list()

jointprob_missing_test_temp <- data.frame()
jointprob_missing_test_sig_temp <- data.frame()
jointprob_missing_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_n[[length(jointprob_missing_test_n) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        for (m in 1: 31) { 
      
          
          # option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n[[m]], 
                   normal_nonmat_n[[m]]  * multiplier_hist_mat_n[[m]][ which.min(is.na( multiplier_hist_mat_n[[m]] ) ) ] ,
                   Upcandidates_attribute_test_missing[[i]][j,m] )$y )
            jointprob_missing_test_temp[j,m] [is.na(jointprob_missing_test_temp[j,m])] <- buf_missing 
          }
          
   
          else {
            jointprob_missing_test_temp[j,m] <- 99999
          }
        }
   
        for (n in 1: 50) {
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                  normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                  Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- 0.0000000001
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf
        
     
        
        # option 2
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * weight[1] +  
          log10( jointprob_missing_test_temp[j,2]) * weight[2] +
          log10( jointprob_missing_test_temp[j,3]) * weight[3] +
          log10( jointprob_missing_test_temp[j,4]) * weight[4] +
          log10( jointprob_missing_test_temp[j,5]) * weight[5] +
          log10( jointprob_missing_test_temp[j,6]) * weight[6] +
          log10( jointprob_missing_test_temp[j,7]) * weight[7] +
          log10( jointprob_missing_test_temp[j,13]) * weight[13] +
          log10( jointprob_missing_test_temp[j,14]) * weight[14] +
          log10( jointprob_missing_test_temp[j,15]) * weight[15] +
          log10( jointprob_missing_test_temp[j,16]) * weight[16] +
          log10( jointprob_missing_test_temp[j,17]) * weight[17] +
          log10( jointprob_missing_test_temp[j,18]) * weight[18] +
          log10( jointprob_missing_test_temp[j,19]) * weight[19] +
          log10( jointprob_missing_test_temp[j,20]) * weight[20] +
          log10( jointprob_missing_test_temp[j,21]) * weight[21] +
          log10( jointprob_missing_test_temp[j,30]) * weight[30] 
    
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * weight[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        
      }
      
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test_n[[length( jointprob_missing_test_n) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    jointprob_missing_test_n[[length(jointprob_missing_test_n) + 1 ]] <-  NA
    jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
    jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- NA
  }
  
}

jointprob_missing_test <- jointprob_missing_test_n
jointprob_matching_test <- jointprob_matching_test_n
jointprob_missing_test_result <- jointprob_missing_test_result_n
jointprob_matching_test_result <- jointprob_matching_test_result_n

# test matching and missing prob normalization
jointprob_matching_test_normalized <- list()
jointprob_missing_test_normalized <- list()

jointprob_matching_test_normalized_temp <- data.frame()
jointprob_missing_test_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( !is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
    for (j in 1 : length(Upcandidates_attribute_test_missing[[i]][,1] )){
      for (k in 1 : length(Upcandidates_attribute_test_missing[[i]] )){
        
        jointprob_matching_test_normalized_temp[j,k] = 
          jointprob_matching_test[[i]][j,k]  / (  jointprob_matching_test[[i]][j,k] +  jointprob_missing_test[[i]][j,k] )
        
        jointprob_missing_test_normalized_temp[j,k] = 
          jointprob_missing_test[[i]][j,k]  / (  jointprob_matching_test[[i]][j,k] +  jointprob_missing_test[[i]][j,k] )
        
      }  
    } 
  }
  
  
  else{
    jointprob_matching_test_normalized_temp[j,k]  <- NA
    jointprob_missing_test_normalized_temp[j,k]  <- NA
  }
  
  
  jointprob_matching_test_normalized[[length( jointprob_matching_test_normalized) + 1 ]]  <- jointprob_matching_test_normalized_temp
  jointprob_missing_test_normalized[[length( jointprob_missing_test_normalized) + 1 ]]  <- jointprob_missing_test_normalized_temp
  
}


# normalized test matching
jointprob_matching_test_result_normalized_temp <- data.frame()
jointprob_matching_test_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        
        for (n in 1: 50) {
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                        normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                         Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_sig 
        }

        
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_sig 
        
        
        # option 2
        
        jointprob_matching_test_result_normalized_temp[j,1] <-   log10( jointprob_matching_test_normalized[[i]][j,1]) * weight[1] +  
          log10( jointprob_matching_test_normalized[[i]][j,2]) * weight[2] +
          log10( jointprob_matching_test_normalized[[i]][j,3]) * weight[3] +
          log10( jointprob_matching_test_normalized[[i]][j,4]) * weight[4] +
          log10( jointprob_matching_test_normalized[[i]][j,5]) * weight[5] +
          log10( jointprob_matching_test_normalized[[i]][j,6]) * weight[6] +
          log10( jointprob_matching_test_normalized[[i]][j,7]) * weight[7] +
          log10( jointprob_matching_test_normalized[[i]][j,13]) * weight[13] +
          log10( jointprob_matching_test_normalized[[i]][j,14]) * weight[14] +
          log10( jointprob_matching_test_normalized[[i]][j,15]) * weight[15] +
          log10( jointprob_matching_test_normalized[[i]][j,16]) * weight[16] +
          log10( jointprob_matching_test_normalized[[i]][j,17]) * weight[17] +
          log10( jointprob_matching_test_normalized[[i]][j,18]) * weight[18] +
          log10( jointprob_matching_test_normalized[[i]][j,19]) * weight[19] +
          log10( jointprob_matching_test_normalized[[i]][j,20]) * weight[20] +
          log10( jointprob_matching_test_normalized[[i]][j,21]) * weight[21] +
          log10( jointprob_matching_test_normalized[[i]][j,30]) * weight[30] 
        
        jointprob_matching_test_result_normalized_temp[j,2] <- log10( jointprob_matching_test[[i]][j,31]) * weight[31]
        jointprob_matching_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_matching_test_result_normalized_temp[j,3] <- jointprob_matching_test_result_normalized_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_matching_test_result_normalized_temp[j,4] <-  jointprob_matching_test_result_normalized_temp[j,1] + 
          jointprob_matching_test_result_normalized_temp[j,2] 
        jointprob_matching_test_result_normalized_temp[j,5] <-  jointprob_matching_test_result_normalized_temp[j,1] +
          jointprob_matching_test_result_normalized_temp[j,3]  
        
      }
  
      
      
      
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- 
        jointprob_matching_test_result_normalized_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test[[length( jointprob_matching_test) +1]] <-  jointprob_matching_test_temp
      
      
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_normalized_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    
    jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
    jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- NA
  }
  
}



# normalized test missing
jointprob_missing_test_result_normalized_temp <- data.frame()
jointprob_missing_test_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:5){
        
        for (n in 1: 50) {
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig[[n]],  
                                                                        normal_nonmat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_sig 
        }
        
        
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_sig 
        
        
        # option 2
        
        jointprob_missing_test_result_normalized_temp[j,1] <-   log10( jointprob_missing_test_normalized[[i]][j,1]) * weight[1] +  
          log10( jointprob_missing_test_normalized[[i]][j,2]) * weight[2] +
          log10( jointprob_missing_test_normalized[[i]][j,3]) * weight[3] +
          log10( jointprob_missing_test_normalized[[i]][j,4]) * weight[4] +
          log10( jointprob_missing_test_normalized[[i]][j,5]) * weight[5] +
          log10( jointprob_missing_test_normalized[[i]][j,6]) * weight[6] +
          log10( jointprob_missing_test_normalized[[i]][j,7]) * weight[7] +
          log10( jointprob_missing_test_normalized[[i]][j,13]) * weight[13] +
          log10( jointprob_missing_test_normalized[[i]][j,14]) * weight[14] +
          log10( jointprob_missing_test_normalized[[i]][j,15]) * weight[15] +
          log10( jointprob_missing_test_normalized[[i]][j,16]) * weight[16] +
          log10( jointprob_missing_test_normalized[[i]][j,17]) * weight[17] +
          log10( jointprob_missing_test_normalized[[i]][j,18]) * weight[18] +
          log10( jointprob_missing_test_normalized[[i]][j,19]) * weight[19] +
          log10( jointprob_missing_test_normalized[[i]][j,20]) * weight[20] +
          log10( jointprob_missing_test_normalized[[i]][j,21]) * weight[21] +
          log10( jointprob_missing_test_normalized[[i]][j,30]) * weight[30] 
        
        jointprob_missing_test_result_normalized_temp[j,2] <- log10( jointprob_missing_test[[i]][j,31]) * weight[31]
        jointprob_missing_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprob_missing_test_result_normalized_temp[j,3] <- jointprob_missing_test_result_normalized_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight[n]
        }
        
        jointprob_missing_test_result_normalized_temp[j,4] <-  jointprob_missing_test_result_normalized_temp[j,1] + 
          jointprob_missing_test_result_normalized_temp[j,2] 
        jointprob_missing_test_result_normalized_temp[j,5] <-  jointprob_missing_test_result_normalized_temp[j,1] +
          jointprob_missing_test_result_normalized_temp[j,3]  
        
      }
     
      
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- 
        jointprob_missing_test_result_normalized_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test[[length( jointprob_missing_test) +1]] <-  jointprob_missing_test_temp
      
      
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_normalized_temp <- data.frame()
      
    }
  }
  
  else # class is not 9
    
  {
    
    jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
    jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- NA
  }
  
}



####### result normalization
w <- 0
ResultMissing_test_all <- data.frame()
ResultMissing_train_all <- data.frame()

# weightwim <- 1
# weightsig1 <- 1
# weightsig2 <- 1

for ( weightwim in seq(from=0, to=3, by=0.5)) {
for ( weightsig1 in seq(from=0, to=3, by=0.5)) {
for ( weightsig2 in seq(from=0, to=3, by=0.5)) {

#  result normalization - train
for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    for (j in 1: 5 ) {
      
      jointprob_matching_train_result[[i]][j,6] <- (jointprob_matching_train_result[[i]][j,1] -
                                                      (min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) - 
             min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ))
      
      jointprob_missing_train_result[[i]][j,6] <- (jointprob_missing_train_result[[i]][j,1] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) - 
             min(unlist(jointprob_matching_train_result[[i]][,1]) , unlist(jointprob_missing_train_result[[i]][,1])) ))
      
      jointprob_matching_train_result[[i]][j,7] <- (jointprob_matching_train_result[[i]][j,2] -
                                                      ( min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) - 
             min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ))
      
      jointprob_missing_train_result[[i]][j,7] <- (jointprob_missing_train_result[[i]][j,2] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) - 
             min(unlist(jointprob_matching_train_result[[i]][,2]) , unlist(jointprob_missing_train_result[[i]][,2])) ))
      
      jointprob_matching_train_result[[i]][j,8] <- (jointprob_matching_train_result[[i]][j,3] -
                                                      ( min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) - 
             min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ))
      
      jointprob_missing_train_result[[i]][j,8] <- (jointprob_missing_train_result[[i]][j,3] -
                                                     ( min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) - 
             min(unlist(jointprob_matching_train_result[[i]][,3]) , unlist(jointprob_missing_train_result[[i]][,3])) ))
      
      jointprob_matching_train_result[[i]][is.na( jointprob_matching_train_result[[i]])] <- 0
      jointprob_missing_train_result[[i]][is.na( jointprob_missing_train_result[[i]])] <- 0
      
      jointprob_matching_train_result[[i]][j,9] <- jointprob_matching_train_result[[i]][j,6] * weightwim +
       jointprob_matching_train_result[[i]][j,7] * weightsig1
      
      jointprob_missing_train_result[[i]][j,9] <- jointprob_missing_train_result[[i]][j,6]  * weightwim  +
       jointprob_missing_train_result[[i]][j,7] * weightsig1
      
      jointprob_matching_train_result[[i]][j,10] <- jointprob_matching_train_result[[i]][j,6] * weightwim  +
       jointprob_matching_train_result[[i]][j,8] * weightsig2
      
      jointprob_missing_train_result[[i]][j,10] <- jointprob_missing_train_result[[i]][j,6] * weightwim  +
       jointprob_missing_train_result[[i]][j,8] * weightsig2
      
      jointprob_matching_train_result[[i]][j,11] <- jointprob_matching_train_result[[i]][j,6] * weightwim  +
        jointprob_matching_train_result[[i]][j,7] * weightsig1 + jointprob_matching_train_result[[i]][j,8] * weightsig2
      
      jointprob_missing_train_result[[i]][j,11] <- jointprob_missing_train_result[[i]][j,6] * weightwim  +
        jointprob_missing_train_result[[i]][j,7] * weightsig1 + jointprob_missing_train_result[[i]][j,8] * weightsig2
      
      jointprob_matching_train_result[[i]][j,12] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 11]  
      jointprob_missing_train_result[[i]][j,12] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 11] 
      
      
    }
  } 
}



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) == 9 ) { 
    for (j in 1: 5 ) {
      
      jointprob_matching_train_result_normalized[[i]][j,6] <- (jointprob_matching_train_result_normalized[[i]][j,1] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ))
      
      jointprob_missing_train_result_normalized[[i]][j,6] <- (jointprob_missing_train_result_normalized[[i]][j,1] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,1]) , unlist(jointprob_missing_train_result_normalized[[i]][,1])) ))
      
      jointprob_matching_train_result_normalized[[i]][j,7] <- (jointprob_matching_train_result_normalized[[i]][j,2] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ))
      
      jointprob_missing_train_result_normalized[[i]][j,7] <- (jointprob_missing_train_result_normalized[[i]][j,2] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,2]) , unlist(jointprob_missing_train_result_normalized[[i]][,2])) ))
      
      jointprob_matching_train_result_normalized[[i]][j,8] <- (jointprob_matching_train_result_normalized[[i]][j,3] -
                                                                 ( min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ))
      
      jointprob_missing_train_result_normalized[[i]][j,8] <- (jointprob_missing_train_result_normalized[[i]][j,3] -
                                                                ( min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) - 
             min(unlist(jointprob_matching_train_result_normalized[[i]][,3]) , unlist(jointprob_missing_train_result_normalized[[i]][,3])) ))
      
      jointprob_matching_train_result_normalized[[i]][is.na( jointprob_matching_train_result_normalized[[i]])] <- 0
      jointprob_missing_train_result_normalized[[i]][is.na( jointprob_missing_train_result_normalized[[i]])] <- 0
      
      jointprob_matching_train_result_normalized[[i]][j,9] <- jointprob_matching_train_result_normalized[[i]][j,6] * weightwim +
      jointprob_matching_train_result_normalized[[i]][j,7] * weightsig1
      
      jointprob_missing_train_result_normalized[[i]][j,9] <- jointprob_missing_train_result_normalized[[i]][j,6] * weightwim +
        jointprob_missing_train_result_normalized[[i]][j,7] * weightsig1
      
      jointprob_matching_train_result_normalized[[i]][j,10] <- jointprob_matching_train_result_normalized[[i]][j,6] * weightwim + 
      jointprob_matching_train_result_normalized[[i]][j,8] * weightsig2
      
      jointprob_missing_train_result_normalized[[i]][j,10] <- jointprob_missing_train_result_normalized[[i]][j,6] * weightwim  +
       jointprob_missing_train_result_normalized[[i]][j,8] * weightsig2
      
      jointprob_matching_train_result_normalized[[i]][j,11] <- jointprob_matching_train_result_normalized[[i]][j,6] * weightwim + 
        jointprob_matching_train_result_normalized[[i]][j,7] * weightsig1 + jointprob_matching_train_result_normalized[[i]][j,8] * weightsig2
      
      jointprob_missing_train_result_normalized[[i]][j,11] <- jointprob_missing_train_result_normalized[[i]][j,6] * weightwim  +
        jointprob_missing_train_result_normalized[[i]][j,7] * weightsig1 + jointprob_missing_train_result_normalized[[i]][j,8] * weightsig2
      
      jointprob_matching_train_result_normalized[[i]][j,12] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 11]  
      jointprob_missing_train_result_normalized[[i]][j,12] <-  jointprob_train_result[[i]][idxjointprob_train[i,j] , 11]    
      
      
    }
  } 
}


#  result normalization - test
for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if (! is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
    
    for (j in 1: 5 ) {
      
      jointprob_matching_test_result[[i]][j,6] <- (jointprob_matching_test_result[[i]][j,1] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ))
      
      jointprob_missing_test_result[[i]][j,6] <- (jointprob_missing_test_result[[i]][j,1] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) - 
             min(unlist(jointprob_matching_test_result[[i]][,1]) , unlist(jointprob_missing_test_result[[i]][,1])) ))
      
      jointprob_matching_test_result[[i]][j,7] <- (jointprob_matching_test_result[[i]][j,2] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ))
      
      jointprob_missing_test_result[[i]][j,7] <- (jointprob_missing_test_result[[i]][j,2] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) - 
             min(unlist(jointprob_matching_test_result[[i]][,2]) , unlist(jointprob_missing_test_result[[i]][,2])) ))
      
      jointprob_matching_test_result[[i]][j,8] <- (jointprob_matching_test_result[[i]][j,3] -
                                                     ( min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ))
      
      jointprob_missing_test_result[[i]][j,8] <- (jointprob_missing_test_result[[i]][j,3] -
                                                    ( min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ) )/ 
        (( max(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) - 
             min(unlist(jointprob_matching_test_result[[i]][,3]) , unlist(jointprob_missing_test_result[[i]][,3])) ))
      
      jointprob_matching_test_result[[i]][is.na( jointprob_matching_test_result[[i]])] <- 0
      jointprob_missing_test_result[[i]][is.na( jointprob_missing_test_result[[i]])] <- 0
      
      jointprob_matching_test_result[[i]][j,9] <- jointprob_matching_test_result[[i]][j,6] * weightwim +
       jointprob_matching_test_result[[i]][j,7] * weightsig1
            
      jointprob_missing_test_result[[i]][j,9] <- jointprob_missing_test_result[[i]][j,6] * weightwim +
       jointprob_missing_test_result[[i]][j,7] * weightsig1
      
      jointprob_matching_test_result[[i]][j,10] <- jointprob_matching_test_result[[i]][j,6] * weightwim +
       jointprob_matching_test_result[[i]][j,8]  * weightsig2
      
      jointprob_missing_test_result[[i]][j,10] <- jointprob_missing_test_result[[i]][j,6] * weightwim +
       jointprob_missing_test_result[[i]][j,8] * weightsig2
      
      jointprob_matching_test_result[[i]][j,11] <- jointprob_matching_test_result[[i]][j,6] * weightwim +
        jointprob_matching_test_result[[i]][j,7]  * weightsig1 + jointprob_matching_test_result[[i]][j,8]  * weightsig2
      
      jointprob_missing_test_result[[i]][j,11] <- jointprob_missing_test_result[[i]][j,6] * weightwim +
        jointprob_missing_test_result[[i]][j,7] * weightsig1 + jointprob_missing_test_result[[i]][j,8] * weightsig2
      
      jointprob_matching_test_result[[i]][j,12] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 11]  
      jointprob_missing_test_result[[i]][j,12] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 11] 
      
 
     }
   }
  } 
}



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    if ( !is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
     
      for (j in 1: 5 ) {
        
        jointprob_matching_test_result_normalized[[i]][j,6] <- (jointprob_matching_test_result_normalized[[i]][j,1] -
                                                                  ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ))
        
        jointprob_missing_test_result_normalized[[i]][j,6] <- (jointprob_missing_test_result_normalized[[i]][j,1] -
                                                                 ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,1]) , unlist(jointprob_missing_test_result_normalized[[i]][,1])) ))
        
        jointprob_matching_test_result_normalized[[i]][j,7] <- (jointprob_matching_test_result_normalized[[i]][j,2] -
                                                                  ( min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ))
        
        jointprob_missing_test_result_normalized[[i]][j,7] <- (jointprob_missing_test_result_normalized[[i]][j,2] -
          ( min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,2]) , unlist(jointprob_missing_test_result_normalized[[i]][,2])) ))
        
        jointprob_matching_test_result_normalized[[i]][j,8] <- (jointprob_matching_test_result_normalized[[i]][j,3] -
          ( min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ))
        
        jointprob_missing_test_result_normalized[[i]][j,8] <- (jointprob_missing_test_result_normalized[[i]][j,3] -
          ( min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ) )/ 
          (( max(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) - 
               min(unlist(jointprob_matching_test_result_normalized[[i]][,3]) , unlist(jointprob_missing_test_result_normalized[[i]][,3])) ))
        
        jointprob_matching_test_result_normalized[[i]][is.na( jointprob_matching_test_result_normalized[[i]])] <- 0
        jointprob_missing_test_result_normalized[[i]][is.na( jointprob_missing_test_result_normalized[[i]])] <- 0
        
        jointprob_matching_test_result_normalized[[i]][j,9] <- jointprob_matching_test_result_normalized[[i]][j,6] * weightwim +
        jointprob_matching_test_result_normalized[[i]][j,7] * weightsig1
        
        jointprob_missing_test_result_normalized[[i]][j,9] <- jointprob_missing_test_result_normalized[[i]][j,6]  * weightwim +
          jointprob_missing_test_result_normalized[[i]][j,7] * weightsig1
        
        jointprob_matching_test_result_normalized[[i]][j,10] <- jointprob_matching_test_result_normalized[[i]][j,6] * weightwim +
         jointprob_matching_test_result_normalized[[i]][j,8] * weightsig2
        
        jointprob_missing_test_result_normalized[[i]][j,10] <- jointprob_missing_test_result_normalized[[i]][j,6] * weightwim +
         jointprob_missing_test_result_normalized[[i]][j,8] * weightsig2
        
        jointprob_matching_test_result_normalized[[i]][j,11] <- jointprob_matching_test_result_normalized[[i]][j,6] * weightwim +
          jointprob_matching_test_result_normalized[[i]][j,7] * weightsig1 +jointprob_matching_test_result_normalized[[i]][j,8] * weightsig2
        
        jointprob_missing_test_result_normalized[[i]][j,11] <- jointprob_missing_test_result_normalized[[i]][j,6] * weightwim +
          jointprob_missing_test_result_normalized[[i]][j,7] * weightsig1 + jointprob_missing_test_result_normalized[[i]][j,8] * weightsig2
        
        jointprob_matching_test_result_normalized[[i]][j,12] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 11]  
        jointprob_missing_test_result_normalized[[i]][j,12] <-  jointprob_test_result[[i]][idxjointprob_test[i,j] , 11]    
        
     
      }
    }
  } 
}




# performance

ResultMisMatching_train <-  ResultMisMatching_train[,-9:-65]
ResultMisMatching_train <- cbind( ResultMisMatching_train, NA ,NA, NA , NA , NA,  NA ,NA, NA , NA , NA ,  NA , NA )


for (i in 1:length(jointprob_matching_train_result) ){
  
  if ( as.numeric ( Downtarget_attributes_train [i,2] ) == 9 ) { 
    
   
    if (!is.na(jointprob_matching_train_result[[i]][1,1])) {
      # wim only
      if ( jointprob_matching_train_result[[i]][1,6] >  jointprob_missing_train_result[[i]][1,6] ) {
        ResultMisMatching_train[i,9] <- ResultMisMatching_train[i,4] 
      }
      else {
        ResultMisMatching_train[i,9] <-  999
      }
      
      # sig diffsum only
      if ( jointprob_matching_train_result[[i]][2,7] >  jointprob_missing_train_result[[i]][2,7] ) {
        ResultMisMatching_train[i,10] <- ResultMisMatching_train[i,5] 
      }
      else {
        ResultMisMatching_train[i,10] <-  999
      }
      
      # sig feature only
      if ( jointprob_matching_train_result[[i]][3,8] >  jointprob_missing_train_result[[i]][3,8] ) {
        ResultMisMatching_train[i,11] <- ResultMisMatching_train[i,6] 
      }
      else {
        ResultMisMatching_train[i,11] <-  999
      }
      
      # wim + sigdiffsum
      if ( jointprob_matching_train_result[[i]][4,9] >  jointprob_missing_train_result[[i]][4,9] ) {
        ResultMisMatching_train[i,12] <- ResultMisMatching_train[i,7] 
      }
      else {
        ResultMisMatching_train[i,12] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_train_result[[i]][5,10] >  jointprob_missing_train_result[[i]][5,10] ) {
        ResultMisMatching_train[i,13] <- ResultMisMatching_train[i,8] 
      }
      else {
        ResultMisMatching_train[i,13] <-  999
      }
      
      
      # normalization
      # wim only
      if ( jointprob_matching_train_result_normalized[[i]][1,6] >  jointprob_missing_train_result_normalized[[i]][1,6] ) {
        ResultMisMatching_train[i,14] <- ResultMisMatching_train[i,4] 
      }
      else {
        ResultMisMatching_train[i,14] <-  999
      }
      
      # sig diffsum only
      if ( jointprob_matching_train_result_normalized[[i]][2,7] >  jointprob_missing_train_result_normalized[[i]][2,7] ) {
        ResultMisMatching_train[i,15] <- ResultMisMatching_train[i,5] 
      }
      else {
        ResultMisMatching_train[i,15] <-  999
      }
      
      # sig feature only
      if ( jointprob_matching_train_result_normalized[[i]][3,8] >  jointprob_missing_train_result_normalized[[i]][3,8] ) {
        ResultMisMatching_train[i,16] <- ResultMisMatching_train[i,6] 
      }
      else {
        ResultMisMatching_train[i,16] <-  999
      }
      
      # wim + sigdiffsum
      if ( jointprob_matching_train_result_normalized[[i]][4,9] >  jointprob_missing_train_result_normalized[[i]][4,9] ) {
        ResultMisMatching_train[i,17] <- ResultMisMatching_train[i,7] 
      }
      else {
        ResultMisMatching_train[i,17] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_train_result_normalized[[i]][5,10] >  jointprob_missing_train_result_normalized[[i]][5,10] ) {
        ResultMisMatching_train[i,18] <- ResultMisMatching_train[i,8] 
      }
      else {
        ResultMisMatching_train[i,18] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_train_result[[i]][5,11] >  jointprob_missing_train_result[[i]][5,11] ) {
        ResultMisMatching_train[i,19] <- ResultMisMatching_train[i,8] 
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
           
    }
    
    else{    
      ResultMisMatching_train[i,9] <-  999
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
       }
  }
  
  else{    
    ResultMisMatching_train[i,9] <-  999
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
  }
}


ResultMisMatching_test <-  ResultMisMatching_test[,-9:-65]
ResultMisMatching_test <- cbind( ResultMisMatching_test, NA ,NA, NA , NA , NA,  NA ,NA, NA , NA , NA ,  NA , NA )


for (i in 1:length(jointprob_matching_test_result) ){
  
  if ( as.numeric ( Downtarget_attributes_test [i,2] ) == 9 ) { 
    
    
    if (!is.na(jointprob_matching_test_result[[i]][1][1])) {
      # wim only
      if ( jointprob_matching_test_result[[i]][1,6] >  jointprob_missing_test_result[[i]][1,6] ) {
        ResultMisMatching_test[i,9] <- ResultMisMatching_test[i,4] 
      }
      else {
        ResultMisMatching_test[i,9] <-  999
      }
      
      # sig diffsum only
      if ( jointprob_matching_test_result[[i]][2,7] >  jointprob_missing_test_result[[i]][2,7] ) {
        ResultMisMatching_test[i,10] <- ResultMisMatching_test[i,5] 
      }
      else {
        ResultMisMatching_test[i,10] <-  999
      }
      
      # sig feature only
      if ( jointprob_matching_test_result[[i]][3,8] >  jointprob_missing_test_result[[i]][3,8] ) {
        ResultMisMatching_test[i,11] <- ResultMisMatching_test[i,6] 
      }
      else {
        ResultMisMatching_test[i,11] <-  999
      }
      
      # wim + sigdiffsum
      if ( jointprob_matching_test_result[[i]][4,9] >  jointprob_missing_test_result[[i]][4,9] ) {
        ResultMisMatching_test[i,12] <- ResultMisMatching_test[i,7] 
      }
      else {
        ResultMisMatching_test[i,12] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_test_result[[i]][5,10] >  jointprob_missing_test_result[[i]][5,10] ) {
        ResultMisMatching_test[i,13] <- ResultMisMatching_test[i,8] 
      }
      else {
        ResultMisMatching_test[i,13] <-  999
      }
      
      
      # normalization
      # wim only
      if ( jointprob_matching_test_result_normalized[[i]][1,6] >  jointprob_missing_test_result_normalized[[i]][1,6] ) {
        ResultMisMatching_test[i,14] <- ResultMisMatching_test[i,4] 
      }
      else {
        ResultMisMatching_test[i,14] <-  999
      }
      
      # sig diffsum only
      if ( jointprob_matching_test_result_normalized[[i]][2,7] >  jointprob_missing_test_result_normalized[[i]][2,7] ) {
        ResultMisMatching_test[i,15] <- ResultMisMatching_test[i,5] 
      }
      else {
        ResultMisMatching_test[i,15] <-  999
      }
      
      # sig feature only
      if ( jointprob_matching_test_result_normalized[[i]][3,8] >  jointprob_missing_test_result_normalized[[i]][3,8] ) {
        ResultMisMatching_test[i,16] <- ResultMisMatching_test[i,6] 
      }
      else {
        ResultMisMatching_test[i,16] <-  999
      }
      
      # wim + sigdiffsum
      if ( jointprob_matching_test_result_normalized[[i]][4,9] >  jointprob_missing_test_result_normalized[[i]][4,9] ) {
        ResultMisMatching_test[i,17] <- ResultMisMatching_test[i,7] 
      }
      else {
        ResultMisMatching_test[i,17] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_test_result_normalized[[i]][5,10] >  jointprob_missing_test_result_normalized[[i]][5,10] ) {
        ResultMisMatching_test[i,18] <- ResultMisMatching_test[i,8] 
      }
      else {
        ResultMisMatching_test[i,18] <-  999
      }
      
      # wim + sigfeature
      if ( jointprob_matching_test_result[[i]][5,11] >  jointprob_missing_test_result[[i]][5,11] ) {
        ResultMisMatching_test[i,19] <- ResultMisMatching_test[i,8] 
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
      
    }
    
    else{    
      ResultMisMatching_test[i,9] <-  999
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
    }
  }
  
  else{    
    ResultMisMatching_test[i,9] <-  999
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
  }
}


# non-normalized train

TargetTable_train <- subset( ResultMisMatching_train , ResultMisMatching_train[,1] == 9 )
TargetTable_test <- subset( ResultMisMatching_test , ResultMisMatching_test[,1] == 9 )

Target_obj_train  <- TargetTable_train[,2]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 


CVeh_train <- matching_obj_train[1]
Veh_train <- length(TargetTable_train[,1])

for (i in 1:12) {
  matching_NN_train[i] <- sum ( as.numeric ((TargetTable_train [,2]) == as.numeric (TargetTable_train [,i+8])) &
                                  as.numeric (TargetTable_train [,2]) != 999)
  missing_NN_train [i] <- sum(   (as.numeric( TargetTable_train[,i+8])) > 1000 ) 
  MVeh_train[i] <-  missing_NN_train [i]
  CMVeh_train[i] <-  matching_NN_train[i]
  MMVeh_train[i] <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                                 !=  as.numeric( TargetTable_train[,i+8])   ))
  
  SIMR_train[i] <- CMVeh_train[i] / CVeh_train[1]
  SCMR_train[i] <- CMVeh_train[i] / MVeh_train[i]
  SER_train[i] <- MMVeh_train[i] / Veh_train[1]
  
  ResultMissing_train[i,] <- data.frame( matching_obj_train[1], missing_obj_train[1],              
                                     matching_NN_train[[i]],  missing_NN_train[[i]],
                                     CMVeh_train[[i]], CVeh_train[[1]], MVeh_train[[i]],
                                     SIMR_train[[i]], SCMR_train[[i]], MMVeh_train[[i]], Veh_train[[1]], SER_train[[i]] )
  
}



# performance
# non-normalized test
Target_obj_test  <- TargetTable_test[,2]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 


CVeh_test <- matching_obj_test[1]
Veh_test <- length(TargetTable_test[,1])

for (i in 1:12) {
  matching_NN_test[i] <- sum ( as.numeric ((TargetTable_test [,2]) == as.numeric (TargetTable_test [,i+8])) &
                                 as.numeric (TargetTable_test [,2]) != 999)
  missing_NN_test[i] <- sum(   (as.numeric( TargetTable_test[,i+8])) > 1000 ) 
  MVeh_test[i] <- missing_NN_test[i]
  CMVeh_test[i] <-  matching_NN_test[i]
  MMVeh_test[i] <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                                   !=  as.numeric( TargetTable_test[,i+8])   ))
  
  SIMR_test[i] <- CMVeh_test[i] / CVeh_test[1]
  SCMR_test[i] <- CMVeh_test[i] / MVeh_test[i]
  SER_test[i] <- MMVeh_test[i] / Veh_test[1]
  
  ResultMissing_test[i,] <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                        matching_NN_test[[i]],  missing_NN_test[[i]],
                                        CMVeh_test[[i]], CVeh_test[[1]], MVeh_test[[i]],
                                        SIMR_test[[i]], SCMR_test[[i]], MMVeh_test[[i]], Veh_test[[1]], SER_test[[1]] )
  
}


w <- w+1
ResultMissing_train_weight <- cbind (weightwim, weightsig1, weightsig2, ResultMissing_train[12,] )
ResultMissing_test_weight <- cbind (weightwim, weightsig1, weightsig2, ResultMissing_test[12,] )
ResultMissing_train_all <-  rbind(ResultMissing_train_all , ResultMissing_train_weight )
ResultMissing_test_all <-  rbind(ResultMissing_test_all , ResultMissing_test_weight )


}}}

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_05082014")
### end

TestSample <- cbind(ResultMisMatching_test[,1],ResultMisMatching_test[,2],ResultMisMatching_test[,14:20] )
View(ResultMissing_test_all)
write.table(ResultMissing_train_all , "./ResultMissing_train_all_v2 .txt", sep="\t",row.names=FALSE)

