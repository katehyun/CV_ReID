
############### start here ############
buf_matching_sig <- 0.00001 #(original)
buf_missing_sig <- 0.01
buf_matching <- 0.01 #original 0.01
buf_missing <- 0.01 


# find missing
Upcandidates_attribute_missing <- list()
Upcandidates_attribute_missing_sig<- list()
Upcandidates_attribute_missing_temp <- data.frame()
Upcandidates_attribute_missing_sig_temp <- data.frame()

Attribute_difftemp_missing <- list()
Attribute_difftemp_missing_sig <- list()
Attribute_diff_nonnormal_missing <- list()
Attribute_diff_nonnormal_missing_sig <- list()



for (i in 1: length(Upcandidates)) {  
  
  for (j in 1:wimfeatlen ) { 
    for (k in 1: length(idxjointprob[1,])) {
      
      if (idxjointprob[i,k] != 999 & !is.na(idxjointprob[i,k]) ) {
        Upcandidates_attribute_missing_temp[k,j] <- 
          Attribute_diff_nonnormal[[i]][[ idxjointprob[i,k] ]] [j]            
      }
      
      else {
        Upcandidates_attribute_missing_temp[k,j] <- NA  
      }  
    }
  }
  
  for (j in 1 :  sigfeatlen){
    for (k in 1: length(idxjointprob[1,])) {
      
      if (idxjointprob[i,k] != 999 & !is.na(idxjointprob[i,k]) ) {
        Upcandidates_attribute_missing_sig_temp[k,j] <-  
          Attribute_sig_nonnormal[[i]][[ idxjointprob[i,k] ]][[1]][j]      
      }
      else {
        Upcandidates_attribute_missing_sig_temp[k,j] <- NA  
      }
    }
  }
  
  Upcandidates_attribute_missing[[length (Upcandidates_attribute_missing) + 1]] <-   Upcandidates_attribute_missing_temp
  Upcandidates_attribute_missing_sig[[length (Upcandidates_attribute_missing_sig) + 1]] <-  Upcandidates_attribute_missing_sig_temp
  
}



#  matching
jointprob_matching <- list()
jointprob_matching_sig <- list()
jointprob_matching_result <- list()

jointprob_matching_temp <- data.frame()
jointprob_matching_sig_temp <- data.frame()
jointprob_matching_result_temp <- data.frame()

for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching[[length(jointprob_matching) + 1 ]] <-  NA
      jointprob_matching_sig[[length(jointprob_matching_sig) + 1 ]] <-  NA
      jointprob_matching_result[[length(jointprob_matching_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 

          if ( m %in% classallidxprob) {
            jointprob_matching_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_tt[[m]], 
                                         normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
                                         Upcandidates_attribute_missing[[i]][j,m] )$y )
            jointprob_matching_temp[j,m] [is.na(jointprob_matching_temp[j,m])] <- buf_matching 
          }
          

          else {
            jointprob_matching_temp[j,m] <- 99999
          }
        }
     
        for (n in 1: sigfeatlen) {
          
          jointprob_matching_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                           normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                           Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_sig_temp[j,n] [is.na(  jointprob_matching_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_temp[j,31] [is.na(jointprob_matching_temp[j,31] )] <-  buf_matching_sig 
        
        jointprob_matching_sig_temp[is.na(jointprob_matching_sig_temp)] <- buf_matching_sig 
        jointprob_matching_sig_temp[jointprob_matching_sig_temp == 0 ] <-  buf_matching_sig 
        jointprob_matching_temp[is.na (jointprob_matching_temp)] <- buf_matching
        jointprob_matching_temp[ jointprob_matching_temp == 0] <- buf_matching
        

        
        jointprob_matching_result_temp[j,1] <-  log10( jointprob_matching_temp[j,1]) * wimweight_tt[1] +  
                                                log10( jointprob_matching_temp[j,2]) * wimweight_tt[2] +
                                                log10( jointprob_matching_temp[j,3]) * wimweight_tt[3] +
                                                log10( jointprob_matching_temp[j,4]) * wimweight_tt[4] +
                                                log10( jointprob_matching_temp[j,5]) * wimweight_tt[5] +
                                                log10( jointprob_matching_temp[j,6]) * wimweight_tt[6] +
                                                log10( jointprob_matching_temp[j,7]) * wimweight_tt[7] +
                                                log10( jointprob_matching_temp[j,13]) * wimweight_tt[13] +
                                                log10( jointprob_matching_temp[j,14]) * wimweight_tt[14] +
                                                log10( jointprob_matching_temp[j,15]) * wimweight_tt[15] +
                                                log10( jointprob_matching_temp[j,16]) * wimweight_tt[16] +
                                                log10( jointprob_matching_temp[j,17]) * wimweight_tt[17] +
                                                log10( jointprob_matching_temp[j,18]) * wimweight_tt[18] +
                                                log10( jointprob_matching_temp[j,19]) * wimweight_tt[19] +
                                                log10( jointprob_matching_temp[j,20]) * wimweight_tt[20] +
                                                log10( jointprob_matching_temp[j,21]) * wimweight_tt[21] +
                                                log10( jointprob_matching_temp[j,30]) * wimweight_tt[30] 
                                       
        jointprob_matching_result_temp[j,2] <- log10( jointprob_matching_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_temp[j,3] <- jointprob_matching_result_temp[j,3] + 
            log10(jointprob_matching_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_result_temp[j,4] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,2] 
        jointprob_matching_result_temp[j,5] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,3]  
        jointprob_matching_result_temp[j,6] <-  jointprob_matching_result_temp[j,1]  + 
          jointprob_matching_result_temp[j,2]  + jointprob_matching_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_result[[length(jointprob_matching_result) + 1]] <- jointprob_matching_result_temp
      jointprob_matching_sig[[length( jointprob_matching_sig) +1]] <-  jointprob_matching_sig_temp
      jointprob_matching[[length( jointprob_matching) +1]] <-  jointprob_matching_temp
      
      jointprob_matching_temp <- data.frame()
      jointprob_matching_sig_temp <- data.frame()
      jointprob_matching_result_temp <- data.frame()
      
    }
  }
  
  
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching[[length(jointprob_matching) + 1 ]] <-  NA
      jointprob_matching_sig[[length(jointprob_matching_sig) + 1 ]] <-  NA
      jointprob_matching_result[[length(jointprob_matching_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_su[[m]], 
                                         normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
                                        Upcandidates_attribute_missing[[i]][j,m] )$y )
            jointprob_matching_temp[j,m] [is.na(jointprob_matching_temp[j,m])] <- buf_matching 
          }
          
          
          else {
            jointprob_matching_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: sigfeatlen) {
          
          jointprob_matching_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                           normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                           Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_sig_temp[j,n] [is.na(  jointprob_matching_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_temp[j,31] [is.na(jointprob_matching_temp[j,31] )] <-  buf_matching_sig 
        
        jointprob_matching_sig_temp[is.na(jointprob_matching_sig_temp)] <- buf_matching_sig 
        jointprob_matching_sig_temp[jointprob_matching_sig_temp == 0 ] <-  buf_matching_sig 
        jointprob_matching_temp[is.na (jointprob_matching_temp)] <- buf_matching
        jointprob_matching_temp[ jointprob_matching_temp == 0] <- buf_matching
        

        jointprob_matching_result_temp[j,1] <-  log10( jointprob_matching_temp[j,1]) * wimweight_su[1] +  
                                                log10( jointprob_matching_temp[j,2]) * wimweight_su[2] +
                                                log10( jointprob_matching_temp[j,3]) * wimweight_su[3] +
                                                log10( jointprob_matching_temp[j,4]) * wimweight_su[4] +
                                                log10( jointprob_matching_temp[j,5]) * wimweight_su[5] +
                                                log10( jointprob_matching_temp[j,6]) * wimweight_su[6] +
                                                log10( jointprob_matching_temp[j,7]) * wimweight_su[7] +
                                                log10( jointprob_matching_temp[j,13]) * wimweight_su[13] +
                                                log10( jointprob_matching_temp[j,14]) * wimweight_su[14] +
                                                log10( jointprob_matching_temp[j,15]) * wimweight_su[15] +
                                                log10( jointprob_matching_temp[j,16]) * wimweight_su[16] +
                                                log10( jointprob_matching_temp[j,17]) * wimweight_su[17] +
                                                log10( jointprob_matching_temp[j,18]) * wimweight_su[18] +
                                                log10( jointprob_matching_temp[j,19]) * wimweight_su[19] +
                                                log10( jointprob_matching_temp[j,20]) * wimweight_su[20] +
                                                log10( jointprob_matching_temp[j,21]) * wimweight_su[21] +
                                                log10( jointprob_matching_temp[j,30]) * wimweight_su[30] 
        
        jointprob_matching_result_temp[j,2] <- log10( jointprob_matching_temp[j,31]) * wimweight_su[31]
        jointprob_matching_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_temp[j,3] <- jointprob_matching_result_temp[j,3] + 
            log10(jointprob_matching_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_result_temp[j,4] <-  jointprob_matching_temp[j,1]  + jointprob_matching_temp[j,2] 
        jointprob_matching_result_temp[j,5] <-  jointprob_matching_temp[j,1]  + jointprob_matching_temp[j,3]  
        jointprob_matching_result_temp[j,6] <-  jointprob_matching_temp[j,1]  + 
          jointprob_matching_result_temp[j,2]  + jointprob_matching_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_result[[length(jointprob_matching_result) + 1]] <- jointprob_matching_result_temp
      jointprob_matching_sig[[length( jointprob_matching_sig) +1]] <-  jointprob_matching_sig_temp
      jointprob_matching[[length( jointprob_matching) +1]] <-  jointprob_matching_temp
      
      jointprob_matchingn_temp <- data.frame()
      jointprob_matching_sig_temp <- data.frame()
      jointprob_matching_result_temp <- data.frame()
      
    }
  }

}

# train - missing

jointprob_missing <- list()
jointprob_missing_sig <- list()
jointprob_missing_result <- list()

jointprob_missing_temp <- data.frame()
jointprob_missing_sig_temp <- data.frame()
jointprob_missing_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_missing)){
  
  if (as.numeric (Downheader_new$FHWAclass [i] ) >= 8) { # TT
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing[[length(jointprob_missing) + 1 ]] <-  NA
      jointprob_missing_sig[[length(jointprob_missing_sig) + 1 ]] <-  NA
      jointprob_missing_result[[length(jointprob_missing_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          if ( m %in% classallidxprob) {
            jointprob_missing_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_tt[[m]], 
                                        normal_nonmat_c_tt[[m]]  * multiplier_hist_nonmat_c_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[m]] ) ) ] ,
                                         Upcandidates_attribute_missing[[i]][j,m] )$y )
            jointprob_missing_temp[j,m] [is.na(jointprob_missing_temp[j,m])] <- buf_missing 
          }
          
          
          else {
            jointprob_missing_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                                         normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_nonmat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[n]] ) ) ] ,
                                         Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_sig_temp[j,n] [is.na(  jointprob_missing_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_temp[j,31] [is.na(jointprob_missing_temp[j,31] )] <-  buf_missing_sig 
        
        jointprob_missing_sig_temp[is.na(jointprob_missing_sig_temp)] <- buf_missing_sig 
        jointprob_missing_sig_temp[jointprob_missing_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_temp[is.na (jointprob_missing_temp)] <- buf_missing
        jointprob_missing_temp[ jointprob_missing_temp == 0] <- buf_missing
        
        
        
        jointprob_missing_result_temp[j,1] <-   log10( jointprob_missing_temp[j,1]) * wimweight_tt[1] +  
                                                log10( jointprob_missing_temp[j,2]) * wimweight_tt[2] +
                                                log10( jointprob_missing_temp[j,3]) * wimweight_tt[3] +
                                                log10( jointprob_missing_temp[j,4]) * wimweight_tt[4] +
                                                log10( jointprob_missing_temp[j,5]) * wimweight_tt[5] +
                                                log10( jointprob_missing_temp[j,6]) * wimweight_tt[6] +
                                                log10( jointprob_missing_temp[j,7]) * wimweight_tt[7] +
                                                log10( jointprob_missing_temp[j,13]) * wimweight_tt[13] +
                                                log10( jointprob_missing_temp[j,14]) * wimweight_tt[14] +
                                                log10( jointprob_missing_temp[j,15]) * wimweight_tt[15] +
                                                log10( jointprob_missing_temp[j,16]) * wimweight_tt[16] +
                                                log10( jointprob_missing_temp[j,17]) * wimweight_tt[17] +
                                                log10( jointprob_missing_temp[j,18]) * wimweight_tt[18] +
                                                log10( jointprob_missing_temp[j,19]) * wimweight_tt[19] +
                                                log10( jointprob_missing_temp[j,20]) * wimweight_tt[20] +
                                                log10( jointprob_missing_temp[j,21]) * wimweight_tt[21] +
                                                log10( jointprob_missing_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_missing_result_temp[j,2] <- log10( jointprob_missing_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_temp[j,3] <- jointprob_missing_result_temp[j,3] + 
            log10(jointprob_missing_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_result_temp[j,4] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,2] 
        jointprob_missing_result_temp[j,5] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,3]  
        jointprob_missing_result_temp[j,6] <-  jointprob_missing_result_temp[j,1]  + 
          jointprob_missing_result_temp[j,2]  + jointprob_missing_result_temp[j,3]  
        
      }
      
      jointprob_missing_result[[length(jointprob_missing_result) + 1]] <- jointprob_missing_result_temp
      jointprob_missing_sig[[length( jointprob_missing_sig) +1]] <-  jointprob_missing_sig_temp
      jointprob_missing[[length( jointprob_missing) +1]] <-  jointprob_missing_temp
      
      jointprob_missing_temp <- data.frame()
      jointprob_missing_sig_temp <- data.frame()
      jointprob_missing_result_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downheader_new$FHWAclass  [i] )  < 8) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing[[length(jointprob_missing) + 1 ]] <-  NA
      jointprob_missing_sig[[length(jointprob_missing_sig) + 1 ]] <-  NA
      jointprob_missing_result[[length(jointprob_missing_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 
          
          if ( m %in% classallidxprob) {
            jointprob_missing_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_su[[m]], 
                                        normal_nonmat_c_su[[m]]  * multiplier_hist_nonmat_c_su[[m]][ which.min(is.na( multiplier_hist_nonmat_c_su[[m]] ) ) ] ,
                                        Upcandidates_attribute_missing[[i]][j,m] )$y )
            jointprob_missing_temp[j,m] [is.na(jointprob_missing_temp[j,m])] <- buf_missing 
          }
          
          
          else {
            jointprob_missing_temp[j,m] <- 99999
          }
        }
        
        for (n in 1:  sigfeatlen) {
          
          jointprob_missing_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                                          normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_nonmat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_su[[n]] ) ) ] ,
                                          Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_sig_temp[j,n] [is.na(  jointprob_missing_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_temp[j,31] [is.na(jointprob_missing_temp[j,31] )] <-  buf_missing_sig 
        
        jointprob_missing_sig_temp[is.na(jointprob_missing_sig_temp)] <- buf_missing_sig 
        jointprob_missing_sig_temp[jointprob_missing_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_temp[is.na (jointprob_missing_temp)] <- buf_missing
        jointprob_missing_temp[ jointprob_missing_temp == 0] <- buf_missing
        
        
        
        
        jointprob_missing_result_temp[j,1] <-   log10( jointprob_missing_temp[j,1]) * wimweight_su[1] +  
                                                log10( jointprob_missing_temp[j,2]) * wimweight_su[2] +
                                                log10( jointprob_missing_temp[j,3]) * wimweight_su[3] +
                                                log10( jointprob_missing_temp[j,4]) * wimweight_su[4] +
                                                log10( jointprob_missing_temp[j,5]) * wimweight_su[5] +
                                                log10( jointprob_missing_temp[j,6]) * wimweight_su[6] +
                                                log10( jointprob_missing_temp[j,7]) * wimweight_su[7] +
                                                log10( jointprob_missing_temp[j,13]) * wimweight_su[13] +
                                                log10( jointprob_missing_temp[j,14]) * wimweight_su[14] +
                                                log10( jointprob_missing_temp[j,15]) * wimweight_su[15] +
                                                log10( jointprob_missing_temp[j,16]) * wimweight_su[16] +
                                                log10( jointprob_missing_temp[j,17]) * wimweight_su[17] +
                                                log10( jointprob_missing_temp[j,18]) * wimweight_su[18] +
                                                log10( jointprob_missing_temp[j,19]) * wimweight_su[19] +
                                                log10( jointprob_missing_temp[j,20]) * wimweight_su[20] +
                                                log10( jointprob_missing_temp[j,21]) * wimweight_su[21] +
                                                log10( jointprob_missing_temp[j,30]) * wimweight_su[30] 
        
        jointprob_missing_result_temp[j,2] <- log10( jointprob_missing_temp[j,31]) * wimweight_su[31]
        jointprob_missing_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_temp[j,3] <- jointprob_missing_result_temp[j,3] + log10(jointprob_missing_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_result_temp[j,4] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,2] 
        jointprob_missing_result_temp[j,5] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,3]  
        jointprob_missing_result_temp[j,6] <-  jointprob_missing_result_temp[j,1]  + 
          jointprob_missing_result_temp[j,2]  + jointprob_missing_result_temp[j,3]  
        
      }
      
      jointprob_missing_result[[length(jointprob_missing_result) + 1]] <- jointprob_missing_result_temp
      jointprob_missing_sig[[length( jointprob_missing_sig) +1]] <-  jointprob_missing_sig_temp
      jointprob_missing[[length( jointprob_missing) +1]] <-  jointprob_missing_temp
      
      jointprob_missing_temp <- data.frame()
      jointprob_missing_sig_temp <- data.frame()
      jointprob_missing_result_temp <- data.frame()
      
    }
  }
}




# joint prob - norm


# train - matching
jointprob_matching_n <- list()
jointprob_matching_sig <- list()
jointprob_matching_result_n <- list()



jointprob_matching_temp <- data.frame()
jointprob_matching_sig_temp <- data.frame()
jointprob_matching_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) >= 8 ) { 
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching_n[[length(jointprob_matching_n) + 1 ]] <-  NA
      jointprob_matching_sig[[length(jointprob_matching_sig) + 1 ]] <-  NA
      jointprob_matching_result_n[[length(jointprob_matching_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_tt[[m]], 
                               normal_mat_n_tt[[m]]  * multiplier_hist_mat_n_tt[[m]][ which.min(is.na( multiplier_hist_mat_n_tt[[m]] ) ) ] ,
                               (( Upcandidates_attribute_missing[[i]][j,m] - min_train_mat_tt[m] ) / ( max_train_mat_tt[m] - min_train_mat_tt[m])) )$y )
            
            jointprob_matching_temp[j,m] [is.na(jointprob_matching_temp[j,m])] <- buf_matching 
          }
          
          else {
            jointprob_matching_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_matching_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                        normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                        Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_sig_temp[j,n] [is.na(  jointprob_matching_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_temp[j,31] [is.na(jointprob_matching_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_sig_temp[is.na(jointprob_matching_sig_temp)] <- buf_matching_sig 
        jointprob_matching_sig_temp[jointprob_matching_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_temp[is.na (jointprob_matching_temp)] <- buf_matching
        jointprob_matching_temp[ jointprob_matching_temp == 0] <- buf_matching
        
        
        
        jointprob_matching_result_temp[j,1] <-  log10( jointprob_matching_temp[j,1]) * wimweight_tt[1] +  
                                                log10( jointprob_matching_temp[j,2]) * wimweight_tt[2] +
                                                log10( jointprob_matching_temp[j,3]) * wimweight_tt[3] +
                                                log10( jointprob_matching_temp[j,4]) * wimweight_tt[4] +
                                                log10( jointprob_matching_temp[j,5]) * wimweight_tt[5] +
                                                log10( jointprob_matching_temp[j,6]) * wimweight_tt[6] +
                                                log10( jointprob_matching_temp[j,7]) * wimweight_tt[7] +
                                                log10( jointprob_matching_temp[j,13]) * wimweight_tt[13] +
                                                log10( jointprob_matching_temp[j,14]) * wimweight_tt[14] +
                                                log10( jointprob_matching_temp[j,15]) * wimweight_tt[15] +
                                                log10( jointprob_matching_temp[j,16]) * wimweight_tt[16] +
                                                log10( jointprob_matching_temp[j,17]) * wimweight_tt[17] +
                                                log10( jointprob_matching_temp[j,18]) * wimweight_tt[18] +
                                                log10( jointprob_matching_temp[j,19]) * wimweight_tt[19] +
                                                log10( jointprob_matching_temp[j,20]) * wimweight_tt[20] +
                                                log10( jointprob_matching_temp[j,21]) * wimweight_tt[21] +
                                                log10( jointprob_matching_temp[j,30]) * wimweight_tt[30] 
                                              
        jointprob_matching_result_temp[j,2] <- log10( jointprob_matching_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_temp[j,3] <- jointprob_matching_result_temp[j,3] + 
            log10(jointprob_matching_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_result_temp[j,4] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,2] 
        jointprob_matching_result_temp[j,5] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,3]  
        jointprob_matching_result_temp[j,6] <-  jointprob_matching_result_temp[j,1]  + 
          jointprob_matching_result_temp[j,2]  + jointprob_matching_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_result_n[[length(jointprob_matching_result_n) + 1]] <- jointprob_matching_result_temp
      jointprob_matching_sig[[length( jointprob_matching_sig) +1]] <-  jointprob_matching_sig_temp
      jointprob_matching_n[[length( jointprob_matching_n) +1]] <-  jointprob_matching_temp
      
      jointprob_matching_temp <- data.frame()
      jointprob_matching_sig_temp <- data.frame()
      jointprob_matching_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching_n[[length(jointprob_matching_n) + 1 ]] <-  NA
      jointprob_matching_sig[[length(jointprob_matching_sig) + 1 ]] <-  NA
      jointprob_matching_result_n[[length(jointprob_matching_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_su[[m]], 
                                                                 normal_mat_n_su[[m]]  * multiplier_hist_mat_n_su[[m]][ which.min(is.na( multiplier_hist_mat_n_su[[m]] ) ) ] ,
                                                                 (( Upcandidates_attribute_missing[[i]][j,m] - min_train_mat_su[m] ) / ( max_train_mat_su[m] - min_train_mat_su[m])) )$y )
            
            jointprob_matching_temp[j,m] [is.na(jointprob_matching_temp[j,m])] <- buf_matching 
          }
          
          else {
            jointprob_matching_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_matching_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                                                    normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                    Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_sig_temp[j,n] [is.na(  jointprob_matching_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_temp[j,31] [is.na(jointprob_matching_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_sig_temp[is.na(jointprob_matching_sig_temp)] <- buf_matching_sig 
        jointprob_matching_sig_temp[jointprob_matching_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_temp[is.na (jointprob_matching_temp)] <- buf_matching
        jointprob_matching_temp[ jointprob_matching_temp == 0] <- buf_matching
        
        
        
        jointprob_matching_result_temp[j,1] <-  log10( jointprob_matching_temp[j,1]) * wimweight_su[1] +  
                                                log10( jointprob_matching_temp[j,2]) * wimweight_su[2] +
                                                log10( jointprob_matching_temp[j,3]) * wimweight_su[3] +
                                                log10( jointprob_matching_temp[j,4]) * wimweight_su[4] +
                                                log10( jointprob_matching_temp[j,5]) * wimweight_su[5] +
                                                log10( jointprob_matching_temp[j,6]) * wimweight_su[6] +
                                                log10( jointprob_matching_temp[j,7]) * wimweight_su[7] +
                                                log10( jointprob_matching_temp[j,13]) * wimweight_su[13] +
                                                log10( jointprob_matching_temp[j,14]) * wimweight_su[14] +
                                                log10( jointprob_matching_temp[j,15]) * wimweight_su[15] +
                                                log10( jointprob_matching_temp[j,16]) * wimweight_su[16] +
                                                log10( jointprob_matching_temp[j,17]) * wimweight_su[17] +
                                                log10( jointprob_matching_temp[j,18]) * wimweight_su[18] +
                                                log10( jointprob_matching_temp[j,19]) * wimweight_su[19] +
                                                log10( jointprob_matching_temp[j,20]) * wimweight_su[20] +
                                                log10( jointprob_matching_temp[j,21]) * wimweight_su[21] +
                                                log10( jointprob_matching_temp[j,30]) * wimweight_su[30] 
                                              
        jointprob_matching_result_temp[j,2] <- log10( jointprob_matching_temp[j,31]) * wimweight_su[31]
        jointprob_matching_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_temp[j,3] <- jointprob_matching_result_temp[j,3] + 
            log10(jointprob_matching_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_result_temp[j,4] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,2] 
        jointprob_matching_result_temp[j,5] <-  jointprob_matching_result_temp[j,1]  + jointprob_matching_result_temp[j,3]  
        jointprob_matching_result_temp[j,6] <-  jointprob_matching_result_temp[j,1]  + 
          jointprob_matching_result_temp[j,2]  + jointprob_matching_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_result_n[[length(jointprob_matching_result_n) + 1]] <- jointprob_matching_result_temp
      jointprob_matching_sig[[length( jointprob_matching_sig) +1]] <-  jointprob_matching_sig_temp
      jointprob_matching_n[[length( jointprob_matching_n) +1]] <-  jointprob_matching_temp
      
      jointprob_matching_temp <- data.frame()
      jointprob_matching_sig_temp <- data.frame()
      jointprob_matching_result_temp <- data.frame()
      
    }
  }
  
  
}


# train - missing

jointprob_missing_n <- list()
jointprob_missing_sig <- list()
jointprob_missing_result_n <- list()

jointprob_missing_temp <- data.frame()
jointprob_missing_sig_temp <- data.frame()
jointprob_missing_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( as.numeric (Downheader_new$FHWAclass [i]) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing_n[[length(jointprob_missing_n) + 1 ]] <-  NA
      jointprob_missing_sig[[length(jointprob_missing_sig) + 1 ]] <-  NA
      jointprob_missing_result_n[[length(jointprob_missing_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_tt[[m]], 
                                                                normal_nonmat_n_tt[[m]]  * multiplier_hist_nonmat_n_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[m]] ) ) ] ,
                                                                (( Upcandidates_attribute_missing[[i]][j,m] - min_train_nonmat_tt[m] ) / ( max_train_nonmat_tt[m] - min_train_nonmat_tt[m])) )$y )
            jointprob_missing_temp[j,m] [is.na(jointprob_missing_temp[j,m])] <- buf_missing 
          }
          
          else {
            jointprob_missing_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                                                                   normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                                                   Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_sig_temp[j,n] [is.na(  jointprob_missing_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_temp[j,31] [is.na(jointprob_missing_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_sig_temp[is.na(jointprob_missing_sig_temp)] <- buf_missing_sig 
        jointprob_missing_sig_temp[jointprob_missing_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_temp[is.na (jointprob_missing_temp)] <- buf_missing
        jointprob_missing_temp[ jointprob_missing_temp == 0] <- buf_missing
        
        
        
        jointprob_missing_result_temp[j,1] <-   log10( jointprob_missing_temp[j,1]) * wimweight_tt[1] +  
                                                log10( jointprob_missing_temp[j,2]) * wimweight_tt[2] +
                                                log10( jointprob_missing_temp[j,3]) * wimweight_tt[3] +
                                                log10( jointprob_missing_temp[j,4]) * wimweight_tt[4] +
                                                log10( jointprob_missing_temp[j,5]) * wimweight_tt[5] +
                                                log10( jointprob_missing_temp[j,6]) * wimweight_tt[6] +
                                                log10( jointprob_missing_temp[j,7]) * wimweight_tt[7] +
                                                log10( jointprob_missing_temp[j,13]) * wimweight_tt[13] +
                                                log10( jointprob_missing_temp[j,14]) * wimweight_tt[14] +
                                                log10( jointprob_missing_temp[j,15]) * wimweight_tt[15] +
                                                log10( jointprob_missing_temp[j,16]) * wimweight_tt[16] +
                                                log10( jointprob_missing_temp[j,17]) * wimweight_tt[17] +
                                                log10( jointprob_missing_temp[j,18]) * wimweight_tt[18] +
                                                log10( jointprob_missing_temp[j,19]) * wimweight_tt[19] +
                                                log10( jointprob_missing_temp[j,20]) * wimweight_tt[20] +
                                                log10( jointprob_missing_temp[j,21]) * wimweight_tt[21] +
                                                log10( jointprob_missing_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_missing_result_temp[j,2] <- log10( jointprob_missing_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_temp[j,3] <- jointprob_missing_result_temp[j,3] + 
            log10(jointprob_missing_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_result_temp[j,4] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,2] 
        jointprob_missing_result_temp[j,5] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,3]  
        jointprob_missing_result_temp[j,6] <-  jointprob_missing_result_temp[j,1]  +
          jointprob_missing_result_temp[j,2] +  jointprob_missing_result_temp[j,3] 
        
      }
      
      jointprob_missing_result_n[[length(jointprob_missing_result_n) + 1]] <- jointprob_missing_result_temp
      jointprob_missing_sig[[length( jointprob_missing_sig) +1]] <-  jointprob_missing_sig_temp
      jointprob_missing_n[[length( jointprob_missing_n) +1]] <-  jointprob_missing_temp
      
      jointprob_missing_temp <- data.frame()
      jointprob_missing_sig_temp <- data.frame()
      jointprob_missing_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing_n[[length(jointprob_missing_n) + 1 ]] <-  NA
      jointprob_missing_sig[[length(jointprob_missing_sig) + 1 ]] <-  NA
      jointprob_missing_result_n[[length(jointprob_missing_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_su[[m]], 
                                                                normal_nonmat_n_su[[m]]  * multiplier_hist_nonmat_n_su[[m]][ which.min(is.na( multiplier_hist_nonmat_n_su[[m]] ) ) ] ,
                                                                (( Upcandidates_attribute_missing[[i]][j,m] - min_train_nonmat_su[m] ) / ( max_train_nonmat_su[m] - min_train_nonmat_su[m])) )$y )
            jointprob_missing_temp[j,m] [is.na(jointprob_missing_temp[j,m])] <- buf_missing 
          }
          
          else {
            jointprob_missing_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                                                                   normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                   Upcandidates_attribute_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_sig_temp[j,n] [is.na(  jointprob_missing_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_temp[j,31] [is.na(jointprob_missing_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_sig_temp[is.na(jointprob_missing_sig_temp)] <- buf_missing_sig 
        jointprob_missing_sig_temp[jointprob_missing_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_temp[is.na (jointprob_missing_temp)] <- buf_missing
        jointprob_missing_temp[ jointprob_missing_temp == 0] <- buf_missing
        
        
        
        jointprob_missing_result_temp[j,1] <-   log10( jointprob_missing_temp[j,1]) * wimweight_su[1] +  
                                                log10( jointprob_missing_temp[j,2]) * wimweight_su[2] +
                                                log10( jointprob_missing_temp[j,3]) * wimweight_su[3] +
                                                log10( jointprob_missing_temp[j,4]) * wimweight_su[4] +
                                                log10( jointprob_missing_temp[j,5]) * wimweight_su[5] +
                                                log10( jointprob_missing_temp[j,6]) * wimweight_su[6] +
                                                log10( jointprob_missing_temp[j,7]) * wimweight_su[7] +
                                                log10( jointprob_missing_temp[j,13]) * wimweight_su[13] +
                                                log10( jointprob_missing_temp[j,14]) * wimweight_su[14] +
                                                log10( jointprob_missing_temp[j,15]) * wimweight_su[15] +
                                                log10( jointprob_missing_temp[j,16]) * wimweight_su[16] +
                                                log10( jointprob_missing_temp[j,17]) * wimweight_su[17] +
                                                log10( jointprob_missing_temp[j,18]) * wimweight_su[18] +
                                                log10( jointprob_missing_temp[j,19]) * wimweight_su[19] +
                                                log10( jointprob_missing_temp[j,20]) * wimweight_su[20] +
                                                log10( jointprob_missing_temp[j,21]) * wimweight_su[21] +
                                                log10( jointprob_missing_temp[j,30]) * wimweight_su[30] 
        
        jointprob_missing_result_temp[j,2] <- log10( jointprob_missing_temp[j,31]) * wimweight_su[31]
        jointprob_missing_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_temp[j,3] <- jointprob_missing_result_temp[j,3] + 
            log10(jointprob_missing_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_result_temp[j,4] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,2] 
        jointprob_missing_result_temp[j,5] <-  jointprob_missing_result_temp[j,1]  + jointprob_missing_result_temp[j,3]  
        jointprob_missing_result_temp[j,6] <-  jointprob_missing_result_temp[j,1]  +
          jointprob_missing_result_temp[j,2] +  jointprob_missing_result_temp[j,3] 
        
      }
      
      jointprob_missing_result_n[[length(jointprob_missing_result_n) + 1]] <- jointprob_missing_result_temp
      jointprob_missing_sig[[length( jointprob_missing_sig) +1]] <-  jointprob_missing_sig_temp
      jointprob_missing_n[[length( jointprob_missing_n) +1]] <-  jointprob_missing_temp
      
      jointprob_missing_temp <- data.frame()
      jointprob_missing_sig_temp <- data.frame()
      jointprob_missing_result_temp <- data.frame()
      
    }
  }
  
  
}




# train matching and missing prob normalization
jointprob_matching_normalized <- list()
jointprob_missing_normalized <- list()

jointprob_matching_normalized_temp <- data.frame()
jointprob_missing_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( !is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
    for (j in 1 : length(Upcandidates_attribute_missing[[i]][,1] )){
      for (k in 1 : length(Upcandidates_attribute_missing[[i]] )){
        
        jointprob_matching_normalized_temp[j,k] = 
          jointprob_matching[[i]][j,k]  / (  jointprob_matching[[i]][j,k] +  jointprob_missing[[i]][j,k] )
        
        jointprob_missing_normalized_temp[j,k] = 
          jointprob_missing[[i]][j,k]  / (  jointprob_matching[[i]][j,k] +  jointprob_missing[[i]][j,k] )
        
      }  
    } 
  }
  
  
  else{
    jointprob_matching_normalized_temp[j,k]  <- NA
    jointprob_missing_normalized_temp[j,k]  <- NA
  }
  
  
  jointprob_matching_normalized[[length( jointprob_matching_normalized) + 1 ]]  <- jointprob_matching_normalized_temp
  jointprob_missing_normalized[[length( jointprob_missing_normalized) + 1 ]]  <- jointprob_missing_normalized_temp
  
}


# train matching and missing prob normalization
jointprob_matching_sig_normalized <- list()
jointprob_missing_sig_normalized <- list()

jointprob_matching_sig_normalized_temp <- data.frame()
jointprob_missing_sig_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( !is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
    for (j in 1 : length(Upcandidates_attribute_missing[[i]][,1] )){
      for (k in 1 : length(Upcandidates_attribute_missing_sig[[i]] )){
        
        jointprob_matching_sig_normalized_temp[j,k] = 
          jointprob_matching_sig[[i]][j,k]  / (  jointprob_matching_sig[[i]][j,k] +  jointprob_missing_sig[[i]][j,k] )
        
        jointprob_missing_sig_normalized_temp[j,k] = 
          jointprob_missing_sig[[i]][j,k]  / (  jointprob_matching_sig[[i]][j,k] +  jointprob_missing_sig[[i]][j,k] )
        
      }  
    } 
  }
  
  
  else{
    jointprob_matching_sig_normalized_temp[j,k]  <- NA
    jointprob_missing_sig_normalized_temp[j,k]  <- NA
  }
  
  
  jointprob_matching_sig_normalized[[length( jointprob_matching_sig_normalized) + 1 ]]  <- jointprob_matching_sig_normalized_temp
  jointprob_missing_sig_normalized[[length( jointprob_missing_sig_normalized) + 1 ]]  <- jointprob_missing_sig_normalized_temp
  
}



# normalized train matching
jointprob_matching_result_normalized_temp <- data.frame()
jointprob_matching_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( as.numeric (Downheader_new$FHWAclass [i]) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching_result_normalized[[length(jointprob_matching_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        
        # option 2
        
        jointprob_matching_result_normalized_temp[j,1] <-   log10( jointprob_matching_normalized[[i]][j,1]) * wimweight_tt[1] +  
                                                            log10( jointprob_matching_normalized[[i]][j,2]) * wimweight_tt[2] +
                                                            log10( jointprob_matching_normalized[[i]][j,3]) * wimweight_tt[3] +
                                                            log10( jointprob_matching_normalized[[i]][j,4]) * wimweight_tt[4] +
                                                            log10( jointprob_matching_normalized[[i]][j,5]) * wimweight_tt[5] +
                                                            log10( jointprob_matching_normalized[[i]][j,6]) * wimweight_tt[6] +
                                                            log10( jointprob_matching_normalized[[i]][j,7]) * wimweight_tt[7] +
                                                            log10( jointprob_matching_normalized[[i]][j,13]) * wimweight_tt[13] +
                                                            log10( jointprob_matching_normalized[[i]][j,14]) * wimweight_tt[14] +
                                                            log10( jointprob_matching_normalized[[i]][j,15]) * wimweight_tt[15] +
                                                            log10( jointprob_matching_normalized[[i]][j,16]) * wimweight_tt[16] +
                                                            log10( jointprob_matching_normalized[[i]][j,17]) * wimweight_tt[17] +
                                                            log10( jointprob_matching_normalized[[i]][j,18]) * wimweight_tt[18] +
                                                            log10( jointprob_matching_normalized[[i]][j,19]) * wimweight_tt[19] +
                                                            log10( jointprob_matching_normalized[[i]][j,20]) * wimweight_tt[20] +
                                                            log10( jointprob_matching_normalized[[i]][j,21]) * wimweight_tt[21] +
                                                            log10( jointprob_matching_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_matching_result_normalized_temp[j,2] <- log10( jointprob_matching[[i]][j,31]) * wimweight_tt[31]
        jointprob_matching_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_normalized_temp[j,3] <- jointprob_matching_result_normalized_temp[j,3] + 
            log10(jointprob_matching_sig_normalized[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_result_normalized_temp[j,4] <-  jointprob_matching_result_normalized_temp[j,1] + 
          jointprob_matching_result_normalized_temp[j,2] 
        jointprob_matching_result_normalized_temp[j,5] <-  jointprob_matching_result_normalized_temp[j,1] +
          jointprob_matching_result_normalized_temp[j,3]  
        jointprob_matching_result_normalized_temp[j,6] <-  jointprob_matching_result_normalized_temp[j,1] +
          jointprob_matching_result_normalized_temp[j,2]  + jointprob_matching_result_normalized_temp[j,3]  
        
      }
      
      jointprob_matching_result_normalized[[length(jointprob_matching_result_normalized) + 1]] <- 
        jointprob_matching_result_normalized_temp
      jointprob_matching_result_normalized_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downheader_new$FHWAclass [i]) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_matching_result_normalized[[length(jointprob_matching_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        
        # option 2
        
        jointprob_matching_result_normalized_temp[j,1] <-   log10( jointprob_matching_normalized[[i]][j,1]) * wimweight_su[1] +  
                                                            log10( jointprob_matching_normalized[[i]][j,2]) * wimweight_su[2] +
                                                            log10( jointprob_matching_normalized[[i]][j,3]) * wimweight_su[3] +
                                                            log10( jointprob_matching_normalized[[i]][j,4]) * wimweight_su[4] +
                                                            log10( jointprob_matching_normalized[[i]][j,5]) * wimweight_su[5] +
                                                            log10( jointprob_matching_normalized[[i]][j,6]) * wimweight_su[6] +
                                                            log10( jointprob_matching_normalized[[i]][j,7]) * wimweight_su[7] +
                                                            log10( jointprob_matching_normalized[[i]][j,13]) * wimweight_su[13] +
                                                            log10( jointprob_matching_normalized[[i]][j,14]) * wimweight_su[14] +
                                                            log10( jointprob_matching_normalized[[i]][j,15]) * wimweight_su[15] +
                                                            log10( jointprob_matching_normalized[[i]][j,16]) * wimweight_su[16] +
                                                            log10( jointprob_matching_normalized[[i]][j,17]) * wimweight_su[17] +
                                                            log10( jointprob_matching_normalized[[i]][j,18]) * wimweight_su[18] +
                                                            log10( jointprob_matching_normalized[[i]][j,19]) * wimweight_su[19] +
                                                            log10( jointprob_matching_normalized[[i]][j,20]) * wimweight_su[20] +
                                                            log10( jointprob_matching_normalized[[i]][j,21]) * wimweight_su[21] +
                                                            log10( jointprob_matching_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_matching_result_normalized_temp[j,2] <- log10( jointprob_matching[[i]][j,31]) * wimweight_su[31]
        jointprob_matching_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_result_normalized_temp[j,3] <- jointprob_matching_result_normalized_temp[j,3] + 
            log10(jointprob_matching_sig_normalized[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_result_normalized_temp[j,4] <-  jointprob_matching_result_normalized_temp[j,1] + 
          jointprob_matching_result_normalized_temp[j,2] 
        jointprob_matching_result_normalized_temp[j,5] <-  jointprob_matching_result_normalized_temp[j,1] +
          jointprob_matching_result_normalized_temp[j,3]  
        jointprob_matching_result_normalized_temp[j,6] <-  jointprob_matching_result_normalized_temp[j,1] +
          jointprob_matching_result_normalized_temp[j,2]  + jointprob_matching_result_normalized_temp[j,3]  
        
      }
      
      jointprob_matching_result_normalized[[length(jointprob_matching_result_normalized) + 1]] <- 
        jointprob_matching_result_normalized_temp
      jointprob_matching_result_normalized_temp <- data.frame()
      
    }
  }

  
}


# normalized train missing

jointprob_missing_result_normalized_temp <- data.frame()
jointprob_missing_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_missing)){
  
  if ( as.numeric (Downheader_new$FHWAclass [i] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing_result_normalized[[length(jointprob_missing_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        # option 2
        
        jointprob_missing_result_normalized_temp[j,1] <-  log10( jointprob_missing_normalized[[i]][j,1]) * wimweight_tt[1] +  
                                                          log10( jointprob_missing_normalized[[i]][j,2]) * wimweight_tt[2] +
                                                          log10( jointprob_missing_normalized[[i]][j,3]) * wimweight_tt[3] +
                                                          log10( jointprob_missing_normalized[[i]][j,4]) * wimweight_tt[4] +
                                                          log10( jointprob_missing_normalized[[i]][j,5]) * wimweight_tt[5] +
                                                          log10( jointprob_missing_normalized[[i]][j,6]) * wimweight_tt[6] +
                                                          log10( jointprob_missing_normalized[[i]][j,7]) * wimweight_tt[7] +
                                                          log10( jointprob_missing_normalized[[i]][j,13]) * wimweight_tt[13] +
                                                          log10( jointprob_missing_normalized[[i]][j,14]) * wimweight_tt[14] +
                                                          log10( jointprob_missing_normalized[[i]][j,15]) * wimweight_tt[15] +
                                                          log10( jointprob_missing_normalized[[i]][j,16]) * wimweight_tt[16] +
                                                          log10( jointprob_missing_normalized[[i]][j,17]) * wimweight_tt[17] +
                                                          log10( jointprob_missing_normalized[[i]][j,18]) * wimweight_tt[18] +
                                                          log10( jointprob_missing_normalized[[i]][j,19]) * wimweight_tt[19] +
                                                          log10( jointprob_missing_normalized[[i]][j,20]) * wimweight_tt[20] +
                                                          log10( jointprob_missing_normalized[[i]][j,21]) * wimweight_tt[21] +
                                                          log10( jointprob_missing_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_missing_result_normalized_temp[j,2] <- log10( jointprob_missing[[i]][j,31]) * wimweight_tt[31]
        jointprob_missing_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_normalized_temp[j,3] <- jointprob_missing_result_normalized_temp[j,3] + 
            log10(jointprob_missing_sig[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_result_normalized_temp[j,4] <-  jointprob_missing_result_normalized_temp[j,1] + 
          jointprob_missing_result_normalized_temp[j,2] 
        jointprob_missing_result_normalized_temp[j,5] <-  jointprob_missing_result_normalized_temp[j,1] +
          jointprob_missing_result_normalized_temp[j,3]  
        jointprob_missing_result_normalized_temp[j,6] <-  jointprob_missing_result_normalized_temp[j,1] +
          jointprob_missing_result_normalized_temp[j,2] + jointprob_missing_result_normalized_temp[j,3] 
        
      }
      
      
      jointprob_missing_result_normalized[[length(jointprob_missing_result_normalized) + 1]] <- 
        jointprob_missing_result_normalized_temp
      
      jointprob_missing_result_normalized_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downheader_new$FHWAclass [i]) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_missing[[i]][1,1] )  ) {
      jointprob_missing_result_normalized[[length(jointprob_missing_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        # option 2
        
        jointprob_missing_result_normalized_temp[j,1] <-  log10( jointprob_missing_normalized[[i]][j,1]) * wimweight_su[1] +  
                                                          log10( jointprob_missing_normalized[[i]][j,2]) * wimweight_su[2] +
                                                          log10( jointprob_missing_normalized[[i]][j,3]) * wimweight_su[3] +
                                                          log10( jointprob_missing_normalized[[i]][j,4]) * wimweight_su[4] +
                                                          log10( jointprob_missing_normalized[[i]][j,5]) * wimweight_su[5] +
                                                          log10( jointprob_missing_normalized[[i]][j,6]) * wimweight_su[6] +
                                                          log10( jointprob_missing_normalized[[i]][j,7]) * wimweight_su[7] +
                                                          log10( jointprob_missing_normalized[[i]][j,13]) * wimweight_su[13] +
                                                          log10( jointprob_missing_normalized[[i]][j,14]) * wimweight_su[14] +
                                                          log10( jointprob_missing_normalized[[i]][j,15]) * wimweight_su[15] +
                                                          log10( jointprob_missing_normalized[[i]][j,16]) * wimweight_su[16] +
                                                          log10( jointprob_missing_normalized[[i]][j,17]) * wimweight_su[17] +
                                                          log10( jointprob_missing_normalized[[i]][j,18]) * wimweight_su[18] +
                                                          log10( jointprob_missing_normalized[[i]][j,19]) * wimweight_su[19] +
                                                          log10( jointprob_missing_normalized[[i]][j,20]) * wimweight_su[20] +
                                                          log10( jointprob_missing_normalized[[i]][j,21]) * wimweight_su[21] +
                                                          log10( jointprob_missing_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_missing_result_normalized_temp[j,2] <- log10( jointprob_missing[[i]][j,31]) * wimweight_su[31]
        jointprob_missing_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_result_normalized_temp[j,3] <- jointprob_missing_result_normalized_temp[j,3] + 
            log10(jointprob_missing_sig[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_result_normalized_temp[j,4] <-  jointprob_missing_result_normalized_temp[j,1] + 
          jointprob_missing_result_normalized_temp[j,2] 
        jointprob_missing_result_normalized_temp[j,5] <-  jointprob_missing_result_normalized_temp[j,1] +
          jointprob_missing_result_normalized_temp[j,3]  
        jointprob_missing_result_normalized_temp[j,6] <-  jointprob_missing_result_normalized_temp[j,1] +
          jointprob_missing_result_normalized_temp[j,2] + jointprob_missing_result_normalized_temp[j,3] 
        
      }
      
      
      jointprob_missing_result_normalized[[length(jointprob_missing_result_normalized) + 1]] <- 
        jointprob_missing_result_normalized_temp
      
      jointprob_missing_result_normalized_temp <- data.frame()
      
    }
  }
  

}



####### result normalization
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


for (i in 1:length(Upcandidates_attribute_missing)){
  
  
  if (!is.na(jointprob_matching_result[[i]][1][1])) {
    
    for (j in 1: 6 ) {
      
      jointprob_matching_result[[i]][j,7] <- (jointprob_matching_result[[i]][j,1] -
                                               ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_missing_result[[i]][j,7] <- (jointprob_missing_result[[i]][j,1] -
                                               ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_matching_result[[i]][j,8] <- (jointprob_matching_result[[i]][j,2] -
                                                ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_missing_result[[i]][j,8] <- (jointprob_missing_result[[i]][j,2] -
                                               ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_matching_result[[i]][j,9] <- (jointprob_matching_result[[i]][j,3] -
                                                ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_missing_result[[i]][j,9] <- (jointprob_missing_result[[i]][j,3] -
                                               ( min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result[[i]][,1:3]) , unlist(jointprob_missing_result[[i]][,1:3])) )
      
      jointprob_matching_result[[i]][is.na( jointprob_matching_result[[i]])] <- 0
      jointprob_missing_result[[i]][is.na( jointprob_missing_result[[i]])] <- 0
      
      jointprob_matching_result[[i]][j,10] <- jointprob_matching_result[[i]][j,7] * weightwim +
        jointprob_matching_result[[i]][j,8] * weightsig1
      
      jointprob_missing_result[[i]][j,10] <- jointprob_missing_result[[i]][j,7]  * weightwim  +
        jointprob_missing_result[[i]][j,8] * weightsig1
      
      jointprob_matching_result[[i]][j,11] <- jointprob_matching_result[[i]][j,7] * weightwim  +
        jointprob_matching_result[[i]][j,9] * weightsig2
      
      jointprob_missing_result[[i]][j,11] <- jointprob_missing_result[[i]][j,7] * weightwim  +
        jointprob_missing_result[[i]][j,9] * weightsig2
      
      jointprob_matching_result[[i]][j,12] <- jointprob_matching_result[[i]][j,7] * weightwim  +
        jointprob_matching_result[[i]][j,8] * weightsig1 + jointprob_matching_result[[i]][j,9] * weightsig2
      
      jointprob_missing_result[[i]][j,12] <- jointprob_missing_result[[i]][j,7] * weightwim  +
        jointprob_missing_result[[i]][j,8] * weightsig1 + jointprob_missing_result[[i]][j,9] * weightsig2
      
      jointprob_matching_result[[i]][j,13] <-  jointprob_result[[i]][idxjointprob[i,j] , 13]  
      jointprob_missing_result[[i]][j,13] <-  jointprob_result[[i]][idxjointprob[i,j] , 13] 
      
      
    }
  } 
}




for (i in 1:length(Upcandidates_attribute_missing)){
  
  
  if (!is.na(jointprob_matching_result_normalized[[i]][1][1])) {
    
    for (j in 1: 6 ) {
      
      jointprob_matching_result_normalized[[i]][j,7] <- (jointprob_matching_result_normalized[[i]][j,1] -
                                                           ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_missing_result_normalized[[i]][j,7] <- (jointprob_missing_result_normalized[[i]][j,1] -
                                                          ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_matching_result_normalized[[i]][j,8] <- (jointprob_matching_result_normalized[[i]][j,2] -
                                                           ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_missing_result_normalized[[i]][j,8] <- (jointprob_missing_result_normalized[[i]][j,2] -
                                                          ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_matching_result_normalized[[i]][j,9] <- (jointprob_matching_result_normalized[[i]][j,3] -
                                                           ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_missing_result_normalized[[i]][j,9] <- (jointprob_missing_result_normalized[[i]][j,3] -
                                                          ( min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) ) )/ 
        ( max(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) - 
            min(unlist(jointprob_matching_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_result_normalized[[i]][,1:3])) )
      
      jointprob_matching_result_normalized[[i]][is.na( jointprob_matching_result_normalized[[i]])] <- 0
      jointprob_missing_result_normalized[[i]][is.na( jointprob_missing_result_normalized[[i]])] <- 0
      
      jointprob_matching_result_normalized[[i]][j,10] <- jointprob_matching_result_normalized[[i]][j,7] * weightwim +
        jointprob_matching_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_missing_result_normalized[[i]][j,10] <- jointprob_missing_result_normalized[[i]][j,7] * weightwim +
        jointprob_missing_result_normalized[[i]][j,8] * weightsig1
      
      jointprob_matching_result_normalized[[i]][j,11] <- jointprob_matching_result_normalized[[i]][j,7] * weightwim + 
        jointprob_matching_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_result_normalized[[i]][j,11] <- jointprob_missing_result_normalized[[i]][j,7] * weightwim  +
        jointprob_missing_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_result_normalized[[i]][j,12] <- jointprob_matching_result_normalized[[i]][j,7] * weightwim + 
        jointprob_matching_result_normalized[[i]][j,8] * weightsig1 + jointprob_matching_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_missing_result_normalized[[i]][j,12] <- jointprob_missing_result_normalized[[i]][j,7] * weightwim  +
        jointprob_missing_result_normalized[[i]][j,8] * weightsig1 + jointprob_missing_result_normalized[[i]][j,9] * weightsig2
      
      jointprob_matching_result_normalized[[i]][j,13] <-  jointprob_result[[i]][idxjointprob[i,j] , 13]  
      jointprob_missing_result_normalized[[i]][j,13] <-  jointprob_result[[i]][idxjointprob[i,j] , 13]    
      
      
    }
  
  } 
}





# performance

ResultMisMatching <- ResultMisMatching_all 
ResultMisMatching <-ResultMisMatching[,-10:-65]
ResultMisMatching <- cbind( ResultMisMatching, NA ,NA, NA , NA , NA,  NA ,NA, NA , NA , NA ,  NA , NA )


for (i in 1:length(jointprob_matching_result) ){
#   for (i in 1:50 ){

  
  if (!is.na(jointprob_matching_result[[i]][1][1])) {
    # wim only
    if ( jointprob_matching_result[[i]][1,7] >  jointprob_missing_result[[i]][1,7] ) {
      ResultMisMatching[i,10] <- ResultMisMatching[i,4] 
    }
    else {
      ResultMisMatching[i,10] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_result[[i]][2,8] >  jointprob_missing_result[[i]][2,8] ) {
      ResultMisMatching[i,11] <- ResultMisMatching[i,5] 
    }
    else {
      ResultMisMatching[i,11] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_result[[i]][3,9] >  jointprob_missing_result[[i]][3,9] ) {
      ResultMisMatching[i,12] <- ResultMisMatching[i,6] 
    }
    else {
      ResultMisMatching[i,12] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_result[[i]][4,10] >  jointprob_missing_result[[i]][4,10] ) {
      ResultMisMatching[i,13] <- ResultMisMatching[i,7] 
    }
    else {
      ResultMisMatching[i,13] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_result[[i]][5,11] >  jointprob_missing_result[[i]][5,11] ) {
      ResultMisMatching[i,14] <- ResultMisMatching[i,8] 
    }
    else {
      ResultMisMatching[i,14] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_result[[i]][6,12] >  jointprob_missing_result[[i]][6,12] ) {
      ResultMisMatching[i,15] <- ResultMisMatching[i,9] 
    }
    else {
      ResultMisMatching[i,15] <-  999
    }
    
    
    # normalization
    # wim only
    if ( jointprob_matching_result_normalized[[i]][1,7] >  jointprob_missing_result_normalized[[i]][1,7] ) {
      ResultMisMatching[i,16] <- ResultMisMatching[i,4] 
    }
    else {
      ResultMisMatching[i,16] <-  999
    }
    
    # sig diffsum only
    if ( jointprob_matching_result_normalized[[i]][2,8] >  jointprob_missing_result_normalized[[i]][2,8] ) {
      ResultMisMatching[i,17] <- ResultMisMatching[i,5] 
    }
    else {
      ResultMisMatching[i,17] <-  999
    }
    
    # sig feature only
    if ( jointprob_matching_result_normalized[[i]][3,9] >  jointprob_missing_result_normalized[[i]][3,9] ) {
      ResultMisMatching[i,18] <- ResultMisMatching[i,6] 
    }
    else {
      ResultMisMatching[i,18] <-  999
    }
    
    # wim + sigdiffsum
    if ( jointprob_matching_result_normalized[[i]][4,10] >  jointprob_missing_result_normalized[[i]][4,10] ) {
      ResultMisMatching[i,19] <- ResultMisMatching[i,7] 
    }
    else {
      ResultMisMatching[i,19] <-  999
    }
    
    # wim + sigfeature
    if ( jointprob_matching_result_normalized[[i]][5,11] >  jointprob_missing_result_normalized[[i]][5,11] ) {
      ResultMisMatching[i,20] <- ResultMisMatching[i,8] 
    }
    else {
      ResultMisMatching[i,20] <-  999
    }
    
    
    # wim + sigfeature
    if ( jointprob_matching_result_normalized[[i]][6,12] >  jointprob_missing_result_normalized[[i]][6,12] ) {
      ResultMisMatching[i,21] <- ResultMisMatching[i,9] 
    }
    else {
      ResultMisMatching[i,21] <-  999
    }
    
  }
  
  else{    
    
    ResultMisMatching[i,10] <-  999
    ResultMisMatching[i,11] <-  999
    ResultMisMatching[i,12] <-  999
    ResultMisMatching[i,13] <-  999
    ResultMisMatching[i,14] <-  999
    ResultMisMatching[i,15] <-  999
    ResultMisMatching[i,16] <-  999
    ResultMisMatching[i,17] <-  999
    ResultMisMatching[i,18] <-  999
    ResultMisMatching[i,19] <-  999
    ResultMisMatching[i,20] <-  999
    ResultMisMatching[i,21] <-  999
  }

}

# remove duplicate
remove <- c(999)


for (i in 1:length(ResultMisMatching[,1]) ){
  

  
  if (!ResultMisMatching[i,10] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,10] [ duplicated(ResultMisMatching[1:i,10] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,10] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][1,7] > jointprob_matching_result[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching[duplicateindex[2], 10] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 10] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,11] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,11] [ duplicated(ResultMisMatching[1:i,11] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <-  which( ResultMisMatching[1:i,11] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][2,8] > jointprob_matching_result[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching[duplicateindex[2], 11] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 11] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching[i,12] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,12] [ duplicated(ResultMisMatching[1:i,12] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,12] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][3,9] > jointprob_matching_result[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching[duplicateindex[2], 12] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 12] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,13] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,13] [ duplicated(ResultMisMatching[1:i,13] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,13] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][4,10] > jointprob_matching_result[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching[duplicateindex[2], 13] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 13] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,14] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,14] [ duplicated(ResultMisMatching[1:i,14] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,14] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][5,11] > jointprob_matching_result[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching[duplicateindex[2], 14] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 14] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,15] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,15] [ duplicated(ResultMisMatching[1:i,15] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,15] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result[[ duplicateindex[1] ]][6,12] > jointprob_matching_result[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching[duplicateindex[2], 15] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 15] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching[i,16] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,16] [ duplicated(ResultMisMatching[1:i,16] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,16] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][1,7] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][1,7]){
        ResultMisMatching[duplicateindex[2], 16] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 16] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching[i,17] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,17] [ duplicated(ResultMisMatching[1:i,17] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,17] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][2,8] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][2,8]){
        ResultMisMatching[duplicateindex[2], 17] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 17] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,18] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,18] [ duplicated(ResultMisMatching[1:i,18] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,18] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][3,9] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][3,9]){
        ResultMisMatching[duplicateindex[2], 18] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 18] <- 999
      }
    }
  }
  
  
  
  if (!ResultMisMatching[i,19] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,19] [ duplicated(ResultMisMatching[1:i,19] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,19] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][4,10] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][4,10]){
        ResultMisMatching[duplicateindex[2], 19] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 19] <- 999
      }
    }
  }
  
  
  if (!ResultMisMatching[i,20] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,20] [ duplicated(ResultMisMatching[1:i,20] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,20] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][5,11] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][5,11]){
        ResultMisMatching[duplicateindex[2], 20] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 20] <- 999
      }
    }
  }
  
  if (!ResultMisMatching[i,21] == 999) {
    duplicatenumbers_temp <-  unique(ResultMisMatching[1:i,21] [ duplicated(ResultMisMatching[1:i,21] )])         
    duplicatenumbers <-duplicatenumbers_temp [! duplicatenumbers_temp %in% remove]
    duplicateindex <- which( ResultMisMatching[1:i,21] %in%   duplicatenumbers ) 
    
    if (length(duplicateindex) > 0) {           
      if (jointprob_matching_result_normalized[[ duplicateindex[1] ]][6,12] > jointprob_matching_result_normalized[[ duplicateindex[2] ]][6,12]){
        ResultMisMatching[duplicateindex[2], 21] <- 999
      }
      else{
        ResultMisMatching[duplicateindex[1], 21] <- 999
      }
    }
  }
  
  
}

colnames(ResultMisMatching) <- c( "FHWAclass" , "a_magdif" , "DownID" ,
                                  "UpID1_1",  "UpID1_2",  "UpID1_3",  "UpID1_4",  "UpID1_5",  "UpID1_6",
                                  "UpID2_1",  "UpID2_2",  "UpID2_3",  "UpID2_4",  "UpID2_5",  "UpID2_6",
                                  "UpID3_1",  "UpID3_2",  "UpID3_3",  "UpID3_4",  "UpID3_5",  "UpID3_6")




ResultMisMatching_a <- cbind( Downheader_new$ts_fieldunit[match(ResultMisMatching[,3],  Downheader_new$sig_id  )]  , ResultMisMatching)
colnames(ResultMisMatching_a) <- c( "ts", "FHWAclass" , "a_magdif" , "DownID" ,
                                  "UpID1_1",  "UpID1_2",  "UpID1_3",  "UpID1_4",  "UpID1_5",  "UpID1_6",
                                  "UpID2_1",  "UpID2_2",  "UpID2_3",  "UpID2_4",  "UpID2_5",  "UpID2_6",
                                  "UpID3_1",  "UpID3_2",  "UpID3_3",  "UpID3_4",  "UpID3_5",  "UpID3_6")


options(scipen=999) 

len = 2 * 24 #3 days * 24 hrs
ts <- 1434524400000  # milisecond
mintoms <- 60000 #min to milisecond (1min = 60000ms)
tsseq  <- vector()
for ( i in 0 : len ){
  timeinterval <- ts + i * 60 * mintoms
  tsseq <- cbind(tsseq , timeinterval)
}

# tsseq <- c(ts + 0*60*mintoms, ts + 1*60*mintoms, ts + 2*60*mintoms, ts + 3*60*mintoms, ts + 4*60*mintoms, ts + 5*60*mintoms,
#            ts + 6*60*mintoms, ts + 7*60*mintoms, ts + 8*60*mintoms, ts + 9*60*mintoms, ts + 10*60*mintoms, ts + 11*60*mintoms, 
#            ts + 12*60*mintoms, ts + 13*60*mintoms, ts + 14*60*mintoms, ts + 15*60*mintoms, ts + 16*60*mintoms, ts + 17*60*mintoms,
#            ts + 18*60*mintoms, ts + 19*60*mintoms, ts + 20*60*mintoms, ts + 21*60*mintoms, ts + 22*60*mintoms, ts + 23*60*mintoms,
#            ts + 24*60*mintoms)

col <- 16

Total_len_all <- data.frame()
Matched_len_all <- data.frame()
Matched_rate_all <- data.frame()
Total_len_tt <- data.frame()
Matched_len_tt <- data.frame()
Matched_rate_tt <- data.frame()
Total_len_su <- data.frame()
Matched_len_su <- data.frame()
Matched_rate_su <- data.frame()
tsidx <- vector()

for ( i in 1: (length(tsseq)-1)){
  
  tsidx <- which( ResultMisMatching_a[,1] > tsseq[i] & ResultMisMatching_a[,1] <= tsseq[i+1]   )
 
  ResultMisMatching_temp <- ResultMisMatching_a[tsidx,]
  TargetTable_train_all <- subset( ResultMisMatching_temp , as.numeric(ResultMisMatching_temp[,2]) >= 4 ) # CHANGE
  TargetTable_train_su <- subset( ResultMisMatching_temp , as.numeric(ResultMisMatching_temp[,2]) < 8 ) # CHANGE
  TargetTable_train_tt <- subset( ResultMisMatching_temp , as.numeric(ResultMisMatching_temp[,2]) >= 8 ) # CHANGE
  
  
  Total_len_all <- length(TargetTable_train_all[,col ]) 
  Matched_len_all <- sum(TargetTable_train_all[,col ]!=999) 
  Matched_rate_all <-rbind( Matched_rate_all,  
                            cbind(tsseq[i] , Total_len_all , Matched_len_all, Matched_len_all  / Total_len_all ) )
  
  Total_len_su <- length(TargetTable_train_su[,col ]) 
  Matched_len_su <- sum(TargetTable_train_su[,col ]!=999)
  Matched_rate_su <- rbind(Matched_rate_su, 
                           cbind(tsseq[i] , Total_len_su, Matched_len_su , Matched_len_su  / Total_len_su ) )
  
  Total_len_tt <- length(TargetTable_train_tt[,col ]) 
  Matched_len_tt <-sum(TargetTable_train_tt[,col ]!=999) 
  Matched_rate_tt <- rbind(Matched_rate_tt, 
                           cbind(tsseq[i] , Total_len_tt, Matched_len_tt, Matched_len_tt  / Total_len_tt ) )

}


Matched_rate_all_byday <- data.frame()
Matched_rate_all_byday[,1] <-  Matched_rate_all[1:24,]
Matched_rate_all_byday[,2] <-  Matched_rate_all[25:48,]


Matched_rate_su_byday[,1] <-  Matched_rate_su[1:24,]
Matched_rate_su_byday[,2] <-  Matched_rate_su[25:48,]


Matched_rate_tt_byday[,1] <-  Matched_rate_tt[1:24,]
Matched_rate_tt_byday[,2] <-  Matched_rate_tt[25:48,]




save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Missing_07072015")
load(("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Missing_07062015"))
