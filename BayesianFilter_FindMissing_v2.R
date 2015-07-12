load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_06012015")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_06232015")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_tt.RData")

# feature weight v1 (entropy only)

th <- 1
wimweight_tt <- vector()
wimweight_tt [1:7] <- th  * wimIGweights_tt[[1]][1:7]
wimweight_tt [8:11] <- th  * 0
wimweight_tt [12:21] <- th  * wimIGweights_tt[[1]][8:17]
wimweight_tt [22:29] <- th  * 0
wimweight_tt [30] <- th  * wimIGweights_tt[[1]][18]
wimweight_tt [31] <- 1

wimweight_su <- vector()
wimweight_su [1:7] <- th  * wimIGweights_su[[1]][1:7]
wimweight_su [8:11] <- th  * 0
wimweight_su [12:21] <- th  * wimIGweights_su[[1]][8:17]
wimweight_su [22:29] <- th  * 0
wimweight_su [30] <- th  * wimIGweights_su[[1]][18]
wimweight_su [31] <- 1

sigweight_su <- unlist(SIGIGweights_su) 
sigweight_tt <- unlist(SIGIGweights_tt)

# feature weight v2 (variance only)
wimweight_su <- wimfeatidx_su
wimweight_tt <- wimfeatidx_tt
sigweight_su <- sigfeatidx_su
sigweight_tt <- sigfeatidx_tt

names(wimfeatidx_tt) <- c("time", "length" , "weight", 
                          "axsp12" , "axsp23" , "axsp34", "axsp45",  "axsp56" , "axsp67", "axsp78", "axsp89",
                          "axwt1l" , "axwt1r" ,  "axwt2l" , "axwt2r" , "axwt3l" , "axwt3r" , "axwt4l" , "axwt4r" ,
                          "axwt5l" , "axwt5r" ,"axwt6l" , "axwt6r" , "axwt7l" , "axwt7r" ,"axwt8l" , "axwt8r" ,"axwt9l" , "axwt9r" ,
                          "duration" , "sigdif" )

names(wimfeatidx_su) <- c("time", "length" , "weight", 
                          "axsp12" , "axsp23" , "axsp34", "axsp45",  "axsp56" , "axsp67", "axsp78", "axsp89",
                          "axwt1l" , "axwt1r" ,  "axwt2l" , "axwt2r" , "axwt3l" , "axwt3r" , "axwt4l" , "axwt4r" ,
                          "axwt5l" , "axwt5r" ,"axwt6l" , "axwt6r" , "axwt7l" , "axwt7r" ,"axwt8l" , "axwt8r" ,"axwt9l" , "axwt9r" ,
                          "duration" , "sigdif" )




# feature weight v3 ( entropy  + variance  but signiture only uses variance and wim uses both)

sigweight_su <- sigfeatidx_su
sigweight_tt <- sigfeatidx_tt

# sigfeatidx_tt[sigfeatidx_tt==4] <- 0
# sigweight_tt  <- as.vector ( as.matrix (sigfeatidx_tt * SIGIGweights_tt ))

wimfeatidx_tt[ wimfeatidx_tt == 4] <- 0

th <- 1
wimweightV2_tt <- vector()
wimweightV2_tt [1:7] <- th  * wimIGweights_tt[[1]][1:7]
wimweightV2_tt [8:11] <- th  * 0
wimweightV2_tt [12:21] <- th  * wimIGweights_tt[[1]][8:17]
wimweightV2_tt [22:29] <- th  * 0
wimweightV2_tt [30] <- th  * wimIGweights_tt[[1]][18]
wimweightV2_tt [31] <- 1

wimweight_tt <- wimfeatidx_tt * wimweightV2_tt


# sigfeatidx_su[sigfeatidx_su==4] <- 0
# sigweight_su  <- as.vector ( as.matrix (sigfeatidx_su * SIGIGweights_su ))

wimfeatidx_su[ wimfeatidx_su == 4] <- 0

th <- 1
wimweightV2_su <- vector()
wimweightV2_su [1:7] <- th  * wimIGweights_su[[1]][1:7]
wimweightV2_su [8:11] <- th  * 0
wimweightV2_su [12:21] <- th  * wimIGweights_su[[1]][8:17]
wimweightV2_su [22:29] <- th  * 0
wimweightV2_su [30] <- th  * wimIGweights_su[[1]][18]
wimweightV2_su [31] <- 1

wimweight_su <- wimfeatidx_su * wimweightV2_su


# feature weight v4 ( entropy  + variance for both wim and sig)

sigweight_su <- sigfeatidx_su
sigweight_tt <- sigfeatidx_tt

sigfeatidx_tt[sigfeatidx_tt==4] <- 0
sigweight_tt  <- as.vector ( as.matrix (sigfeatidx_tt * SIGIGweights_tt ))

wimfeatidx_tt[ wimfeatidx_tt == 4] <- 0

th <- 1
wimweightV2_tt <- vector()
wimweightV2_tt [1:7] <- th  * wimIGweights_tt[[1]][1:7]
wimweightV2_tt [8:11] <- th  * 0
wimweightV2_tt [12:21] <- th  * wimIGweights_tt[[1]][8:17]
wimweightV2_tt [22:29] <- th  * 0
wimweightV2_tt [30] <- th  * wimIGweights_tt[[1]][18]
wimweightV2_tt [31] <- 1

wimweight_tt <- wimfeatidx_tt * wimweightV2_tt


sigfeatidx_su[sigfeatidx_su==4] <- 0
sigweight_su  <- as.vector ( as.matrix (sigfeatidx_su * SIGIGweights_su ))

wimfeatidx_su[ wimfeatidx_su == 4] <- 0

th <- 1
wimweightV2_su <- vector()
wimweightV2_su [1:7] <- th  * wimIGweights_su[[1]][1:7]
wimweightV2_su [8:11] <- th  * 0
wimweightV2_su [12:21] <- th  * wimIGweights_su[[1]][8:17]
wimweightV2_su [22:29] <- th  * 0
wimweightV2_su [30] <- th  * wimIGweights_su[[1]][18]
wimweightV2_su [31] <- 1

wimweight_su <- wimfeatidx_su * wimweightV2_su


############### start here ############
buf_matching_sig <- 0.00001 #(original)
buf_missing_sig <- 0.01
buf_matching <- 0.01 #original 0.01
buf_missing <- 0.01 


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
  
  for (j in 1:wimfeatlen ) { 
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


# train - matching
jointprob_matching_train <- list()
jointprob_matching_train_sig <- list()
jointprob_matching_train_result <- list()


jointprob_matching_train_temp <- data.frame()
jointprob_matching_train_sig_temp <- data.frame()
jointprob_matching_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train[[length(jointprob_matching_train) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 
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
          if ( m %in% classallidxprob) {
            jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_tt[[m]], 
                     normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
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
        for (n in 1: sigfeatlen) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                     normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                     Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <-  buf_matching_sig 
        
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_matching_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <-  buf_matching_sig 
        jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf_matching
        jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf_matching
        
        
        
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
        
        jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * wimweight_tt[1] +  
                                                        log10( jointprob_matching_train_temp[j,2]) * wimweight_tt[2] +
                                                        log10( jointprob_matching_train_temp[j,3]) * wimweight_tt[3] +
                                                        log10( jointprob_matching_train_temp[j,4]) * wimweight_tt[4] +
                                                        log10( jointprob_matching_train_temp[j,5]) * wimweight_tt[5] +
                                                        log10( jointprob_matching_train_temp[j,6]) * wimweight_tt[6] +
                                                        log10( jointprob_matching_train_temp[j,7]) * wimweight_tt[7] +
                                                        log10( jointprob_matching_train_temp[j,13]) * wimweight_tt[13] +
                                                        log10( jointprob_matching_train_temp[j,14]) * wimweight_tt[14] +
                                                        log10( jointprob_matching_train_temp[j,15]) * wimweight_tt[15] +
                                                        log10( jointprob_matching_train_temp[j,16]) * wimweight_tt[16] +
                                                        log10( jointprob_matching_train_temp[j,17]) * wimweight_tt[17] +
                                                        log10( jointprob_matching_train_temp[j,18]) * wimweight_tt[18] +
                                                        log10( jointprob_matching_train_temp[j,19]) * wimweight_tt[19] +
                                                        log10( jointprob_matching_train_temp[j,20]) * wimweight_tt[20] +
                                                        log10( jointprob_matching_train_temp[j,21]) * wimweight_tt[21] +
                                                        log10( jointprob_matching_train_temp[j,30]) * wimweight_tt[30] 
        #                                               log10(    1 - ( alphasig * jointprob_matching_train[i,31] ) )
        #                                               log10( jointprob_matching_train[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
        #       jointprob_matching_train[i,33] <-  log10( 1/ jointprob_matching_train[i,31]) * weight[31]  
        jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
        jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
        jointprob_matching_train_result_temp[j,6] <-  jointprob_matching_train_result_temp[j,1]  + 
                                                      jointprob_matching_train_result_temp[j,2]  + jointprob_matching_train_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- jointprob_matching_train_result_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train[[length( jointprob_matching_train) +1]] <-  jointprob_matching_train_temp
      
      jointprob_matching_train_temp <- data.frame()
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_temp <- data.frame()
      
    }
  }
  
  
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train[[length(jointprob_matching_train) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 

          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_su[[m]], 
                  normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
                  Upcandidates_attribute_train_missing[[i]][j,m] )$y )
            jointprob_matching_train_temp[j,m] [is.na(jointprob_matching_train_temp[j,m])] <- buf_matching 
          }
          

          else {
            jointprob_matching_train_temp[j,m] <- 99999
          }
        }
     
        for (n in 1: sigfeatlen) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                   normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                   Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <-  buf_matching_sig 
        
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_matching_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <-  buf_matching_sig 
        jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf_matching
        jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf_matching
        
        
        
      
        
        jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_train_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_matching_train_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_matching_train_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_matching_train_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_matching_train_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_matching_train_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_matching_train_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_matching_train_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_matching_train_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_matching_train_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_matching_train_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_matching_train_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_matching_train_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_matching_train_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_matching_train_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_matching_train_temp[j,30]) * wimweight_su[30] 
  
        jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * wimweight_su[31]
        jointprob_matching_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
        jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
        jointprob_matching_train_result_temp[j,6] <-  jointprob_matching_train_result_temp[j,1]  + 
          jointprob_matching_train_result_temp[j,2]  + jointprob_matching_train_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- jointprob_matching_train_result_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train[[length( jointprob_matching_train) +1]] <-  jointprob_matching_train_temp
      
      jointprob_matching_train_temp <- data.frame()
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_temp <- data.frame()
      
    }
  }
#   else # class is not 9
#     
#   {
#     jointprob_matching_train[[length(jointprob_matching_train) + 1 ]] <-  NA
#     jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
#     jointprob_matching_train_result[[length(jointprob_matching_train_result) + 1]] <- NA
#   }
  
}

# train - missing

jointprob_missing_train <- list()
jointprob_missing_train_sig <- list()
jointprob_missing_train_result <- list()

jointprob_missing_train_temp <- data.frame()
jointprob_missing_train_sig_temp <- data.frame()
jointprob_missing_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8) { # TT
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train[[length(jointprob_missing_train) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_tt[[m]], 
                      normal_nonmat_c_tt[[m]]  * multiplier_hist_nonmat_c_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[m]] ) ) ] ,
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
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                                                                         normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_nonmat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <-  buf_missing_sig 
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_missing_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf_missing
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf_missing
        
        
        
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
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_train_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_train_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_train_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_train_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_train_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_train_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_train_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_train_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_train_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_train_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_train_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_train_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_train_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_train_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_train_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_train_temp[j,30]) * wimweight_tt[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_train[i,31] ) )
        #                                               log10( jointprob_missing_train[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
        #       jointprob_missing_train[i,33] <-  log10( 1/ jointprob_missing_train[i,31]) * weight[31]  
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        jointprob_missing_train_result_temp[j,6] <-  jointprob_missing_train_result_temp[j,1]  + 
                                                     jointprob_missing_train_result_temp[j,2]  + jointprob_missing_train_result_temp[j,3]  
        
      }
      
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train[[length( jointprob_missing_train) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downtarget_attributes_train [i,2] )  < 8) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train[[length(jointprob_missing_train) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_su[[m]], 
                                                                      normal_nonmat_c_su[[m]]  * multiplier_hist_nonmat_c_su[[m]][ which.min(is.na( multiplier_hist_nonmat_c_su[[m]] ) ) ] ,
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
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                                                                         normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_nonmat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_su[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <-  buf_missing_sig 
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_missing_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf_missing
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf_missing
        
        
        
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
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_train_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_missing_train_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_missing_train_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_missing_train_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_missing_train_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_missing_train_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_missing_train_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_missing_train_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_missing_train_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_missing_train_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_missing_train_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_missing_train_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_missing_train_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_missing_train_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_missing_train_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_missing_train_temp[j,30]) * wimweight_su[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_train[i,31] ) )
        #                                               log10( jointprob_missing_train[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_train_missing[i,31]) * weight[31] 
        #       jointprob_missing_train[i,33] <-  log10( 1/ jointprob_missing_train[i,31]) * weight[31]  
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * wimweight_su[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        jointprob_missing_train_result_temp[j,6] <-  jointprob_missing_train_result_temp[j,1]  + 
          jointprob_missing_train_result_temp[j,2]  + jointprob_missing_train_result_temp[j,3]  
        
      }
      
      jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train[[length( jointprob_missing_train) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
#   else # class is not 9
#     
#   {
#     jointprob_missing_train[[length(jointprob_missing_train) + 1 ]] <-  NA
#     jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
#     jointprob_missing_train_result[[length(jointprob_missing_train_result) + 1]] <- NA
#   }
#   
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
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { 
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_n[[length(jointprob_matching_train_n) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: wimfeatlen) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_tt[[m]], 
                   normal_mat_n_tt[[m]]  * multiplier_hist_mat_n_tt[[m]][ which.min(is.na( multiplier_hist_mat_n_tt[[m]] ) ) ] ,
                   (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_mat_tt[m] ) / ( max_train_mat_tt[m] - min_train_mat_tt[m])) )$y )
            
            jointprob_matching_train_temp[j,m] [is.na(jointprob_matching_train_temp[j,m])] <- buf_matching 
          }
          
          else {
            jointprob_matching_train_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                   normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                   Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_matching_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf_matching
        jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf_matching
        
        
        
        jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_matching_train_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_matching_train_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_matching_train_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_matching_train_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_matching_train_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_matching_train_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_matching_train_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_matching_train_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_matching_train_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_matching_train_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_matching_train_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_matching_train_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_matching_train_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_matching_train_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_matching_train_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_matching_train_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
        jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
        jointprob_matching_train_result_temp[j,6] <-  jointprob_matching_train_result_temp[j,1]  + 
                                                      jointprob_matching_train_result_temp[j,2]  + jointprob_matching_train_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- jointprob_matching_train_result_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train_n[[length( jointprob_matching_train_n) +1]] <-  jointprob_matching_train_temp
      
      jointprob_matching_train_temp <- data.frame()
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_n[[length(jointprob_matching_train_n) + 1 ]] <-  NA
      jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_train_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_su[[m]], 
                     normal_mat_n_su[[m]]  * multiplier_hist_mat_n_su[[m]][ which.min(is.na( multiplier_hist_mat_n_su[[m]] ) ) ] ,
                     (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_mat_su[m] ) / ( max_train_mat_su[m] - min_train_mat_su[m])) )$y )
            
            jointprob_matching_train_temp[j,m] [is.na(jointprob_matching_train_temp[j,m])] <- buf_matching 
          }
          
          else {
            jointprob_matching_train_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_matching_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                        normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                        Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_train_sig_temp[j,n] [is.na(  jointprob_matching_train_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_train_temp[j,31] [is.na(jointprob_matching_train_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_train_sig_temp[is.na(jointprob_matching_train_sig_temp)] <- buf_matching_sig 
        jointprob_matching_train_sig_temp[jointprob_matching_train_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_train_temp[is.na (jointprob_matching_train_temp)] <- buf_matching
        jointprob_matching_train_temp[ jointprob_matching_train_temp == 0] <- buf_matching
        
        
        
        jointprob_matching_train_result_temp[j,1] <-   log10( jointprob_matching_train_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_train_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_matching_train_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_matching_train_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_matching_train_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_matching_train_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_matching_train_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_matching_train_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_matching_train_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_matching_train_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_matching_train_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_matching_train_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_matching_train_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_matching_train_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_matching_train_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_matching_train_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_matching_train_temp[j,30]) * wimweight_su[30] 
        
        jointprob_matching_train_result_temp[j,2] <- log10( jointprob_matching_train_temp[j,31]) * wimweight_su[31]
        jointprob_matching_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_temp[j,3] <- jointprob_matching_train_result_temp[j,3] + 
            log10(jointprob_matching_train_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_train_result_temp[j,4] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,2] 
        jointprob_matching_train_result_temp[j,5] <-  jointprob_matching_train_result_temp[j,1]  + jointprob_matching_train_result_temp[j,3]  
        jointprob_matching_train_result_temp[j,6] <-  jointprob_matching_train_result_temp[j,1]  + 
          jointprob_matching_train_result_temp[j,2]  + jointprob_matching_train_result_temp[j,3] 
        
      }
      
      
      
      jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- jointprob_matching_train_result_temp
      jointprob_matching_train_sig[[length( jointprob_matching_train_sig) +1]] <-  jointprob_matching_train_sig_temp
      jointprob_matching_train_n[[length( jointprob_matching_train_n) +1]] <-  jointprob_matching_train_temp
      
      jointprob_matching_train_temp <- data.frame()
      jointprob_matching_train_sig_temp <- data.frame()
      jointprob_matching_train_result_temp <- data.frame()
      
    }
  }
#   else # class is not 9
#     
#   {
#     jointprob_matching_train_n[[length(jointprob_matching_train_n) + 1 ]] <-  NA
#     jointprob_matching_train_sig[[length(jointprob_matching_train_sig) + 1 ]] <-  NA
#     jointprob_matching_train_result_n[[length(jointprob_matching_train_result_n) + 1]] <- NA
#   }
  
}

# train - missing

jointprob_missing_train_n <- list()
jointprob_missing_train_sig <- list()
jointprob_missing_train_result_n <- list()

jointprob_missing_train_temp <- data.frame()
jointprob_missing_train_sig_temp <- data.frame()
jointprob_missing_train_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_n[[length(jointprob_missing_train_n) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_tt[[m]], 
                      normal_nonmat_n_tt[[m]]  * multiplier_hist_nonmat_n_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[m]] ) ) ] ,
                      (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_nonmat_tt[m] ) / ( max_train_nonmat_tt[m] - min_train_nonmat_tt[m])) )$y )
            jointprob_missing_train_temp[j,m] [is.na(jointprob_missing_train_temp[j,m])] <- buf_missing 
          }
          
          else {
            jointprob_missing_train_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                   normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                   Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_missing_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf_missing
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf_missing
        
        
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_train_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_train_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_train_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_train_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_train_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_train_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_train_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_train_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_train_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_train_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_train_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_train_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_train_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_train_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_train_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_train_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        jointprob_missing_train_result_temp[j,6] <-  jointprob_missing_train_result_temp[j,1]  +
                                                     jointprob_missing_train_result_temp[j,2] +  jointprob_missing_train_result_temp[j,3] 
        
      }
      
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train_n[[length( jointprob_missing_train_n) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_n[[length(jointprob_missing_train_n) + 1 ]] <-  NA
      jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_train_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_su[[m]], 
                      normal_nonmat_n_su[[m]]  * multiplier_hist_nonmat_n_su[[m]][ which.min(is.na( multiplier_hist_nonmat_n_su[[m]] ) ) ] ,
                      (( Upcandidates_attribute_train_missing[[i]][j,m] - min_train_nonmat_su[m] ) / ( max_train_nonmat_su[m] - min_train_nonmat_su[m])) )$y )
            jointprob_missing_train_temp[j,m] [is.na(jointprob_missing_train_temp[j,m])] <- buf_missing 
          }
          
          else {
            jointprob_missing_train_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_train_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                   normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                   Upcandidates_attribute_train_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_train_sig_temp[j,n] [is.na(  jointprob_missing_train_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_train_temp[j,31] [is.na(jointprob_missing_train_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_train_sig_temp[is.na(jointprob_missing_train_sig_temp)] <- buf_missing_sig 
        jointprob_missing_train_sig_temp[jointprob_missing_train_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_train_temp[is.na (jointprob_missing_train_temp)] <- buf_missing
        jointprob_missing_train_temp[ jointprob_missing_train_temp == 0] <- buf_missing
        
        
        
        jointprob_missing_train_result_temp[j,1] <-   log10( jointprob_missing_train_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_train_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_missing_train_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_missing_train_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_missing_train_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_missing_train_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_missing_train_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_missing_train_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_missing_train_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_missing_train_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_missing_train_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_missing_train_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_missing_train_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_missing_train_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_missing_train_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_missing_train_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_missing_train_temp[j,30]) * wimweight_su[30] 
        
        jointprob_missing_train_result_temp[j,2] <- log10( jointprob_missing_train_temp[j,31]) * wimweight_su[31]
        jointprob_missing_train_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_temp[j,3] <- jointprob_missing_train_result_temp[j,3] + 
            log10(jointprob_missing_train_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_train_result_temp[j,4] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,2] 
        jointprob_missing_train_result_temp[j,5] <-  jointprob_missing_train_result_temp[j,1]  + jointprob_missing_train_result_temp[j,3]  
        jointprob_missing_train_result_temp[j,6] <-  jointprob_missing_train_result_temp[j,1]  +
          jointprob_missing_train_result_temp[j,2] +  jointprob_missing_train_result_temp[j,3] 
        
      }
      
      jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- jointprob_missing_train_result_temp
      jointprob_missing_train_sig[[length( jointprob_missing_train_sig) +1]] <-  jointprob_missing_train_sig_temp
      jointprob_missing_train_n[[length( jointprob_missing_train_n) +1]] <-  jointprob_missing_train_temp
      
      jointprob_missing_train_temp <- data.frame()
      jointprob_missing_train_sig_temp <- data.frame()
      jointprob_missing_train_result_temp <- data.frame()
      
    }
  }
  
  
#   else # class is not 9
#     
#   {
#     jointprob_missing_train_n[[length(jointprob_missing_train_n) + 1 ]] <-  NA
#     jointprob_missing_train_sig[[length(jointprob_missing_train_sig) + 1 ]] <-  NA
#     jointprob_missing_train_result_n[[length(jointprob_missing_train_result_n) + 1]] <- NA
#   }
#   
}

# jointprob_missing_train_result <- jointprob_missing_train_result_n
# jointprob_matching_train_result <- jointprob_matching_train_result_n
# jointprob_missing_train <- jointprob_missing_train_n
# jointprob_matching_train <- jointprob_matching_train_n




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


# train matching and missing prob normalization
jointprob_matching_train_sig_normalized <- list()
jointprob_missing_train_sig_normalized <- list()

jointprob_matching_train_sig_normalized_temp <- data.frame()
jointprob_missing_train_sig_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( !is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
    for (j in 1 : length(Upcandidates_attribute_train_missing[[i]][,1] )){
      for (k in 1 : length(Upcandidates_attribute_train_missing_sig[[i]] )){
        
        jointprob_matching_train_sig_normalized_temp[j,k] = 
          jointprob_matching_train_sig[[i]][j,k]  / (  jointprob_matching_train_sig[[i]][j,k] +  jointprob_missing_train_sig[[i]][j,k] )
        
        jointprob_missing_train_sig_normalized_temp[j,k] = 
          jointprob_missing_train_sig[[i]][j,k]  / (  jointprob_matching_train_sig[[i]][j,k] +  jointprob_missing_train_sig[[i]][j,k] )
        
      }  
    } 
  }
  
  
  else{
    jointprob_matching_train_sig_normalized_temp[j,k]  <- NA
    jointprob_missing_train_sig_normalized_temp[j,k]  <- NA
  }
  
  
  jointprob_matching_train_sig_normalized[[length( jointprob_matching_train_sig_normalized) + 1 ]]  <- jointprob_matching_train_sig_normalized_temp
  jointprob_missing_train_sig_normalized[[length( jointprob_missing_train_sig_normalized) + 1 ]]  <- jointprob_missing_train_sig_normalized_temp
  
}

# normalized train matching
jointprob_matching_train_result_normalized_temp <- data.frame()
jointprob_matching_train_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){

        
        # option 2
        
        jointprob_matching_train_result_normalized_temp[j,1] <-   log10( jointprob_matching_train_normalized[[i]][j,1]) * wimweight_tt[1] +  
          log10( jointprob_matching_train_normalized[[i]][j,2]) * wimweight_tt[2] +
          log10( jointprob_matching_train_normalized[[i]][j,3]) * wimweight_tt[3] +
          log10( jointprob_matching_train_normalized[[i]][j,4]) * wimweight_tt[4] +
          log10( jointprob_matching_train_normalized[[i]][j,5]) * wimweight_tt[5] +
          log10( jointprob_matching_train_normalized[[i]][j,6]) * wimweight_tt[6] +
          log10( jointprob_matching_train_normalized[[i]][j,7]) * wimweight_tt[7] +
          log10( jointprob_matching_train_normalized[[i]][j,13]) * wimweight_tt[13] +
          log10( jointprob_matching_train_normalized[[i]][j,14]) * wimweight_tt[14] +
          log10( jointprob_matching_train_normalized[[i]][j,15]) * wimweight_tt[15] +
          log10( jointprob_matching_train_normalized[[i]][j,16]) * wimweight_tt[16] +
          log10( jointprob_matching_train_normalized[[i]][j,17]) * wimweight_tt[17] +
          log10( jointprob_matching_train_normalized[[i]][j,18]) * wimweight_tt[18] +
          log10( jointprob_matching_train_normalized[[i]][j,19]) * wimweight_tt[19] +
          log10( jointprob_matching_train_normalized[[i]][j,20]) * wimweight_tt[20] +
          log10( jointprob_matching_train_normalized[[i]][j,21]) * wimweight_tt[21] +
          log10( jointprob_matching_train_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_matching_train_result_normalized_temp[j,2] <- log10( jointprob_matching_train[[i]][j,31]) * wimweight_tt[31]
        jointprob_matching_train_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_normalized_temp[j,3] <- jointprob_matching_train_result_normalized_temp[j,3] + 
            log10(jointprob_matching_train_sig_normalized[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_train_result_normalized_temp[j,4] <-  jointprob_matching_train_result_normalized_temp[j,1] + 
          jointprob_matching_train_result_normalized_temp[j,2] 
        jointprob_matching_train_result_normalized_temp[j,5] <-  jointprob_matching_train_result_normalized_temp[j,1] +
          jointprob_matching_train_result_normalized_temp[j,3]  
        jointprob_matching_train_result_normalized_temp[j,6] <-  jointprob_matching_train_result_normalized_temp[j,1] +
          jointprob_matching_train_result_normalized_temp[j,2]  + jointprob_matching_train_result_normalized_temp[j,3]  
        
      }
      
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- 
        jointprob_matching_train_result_normalized_temp
      jointprob_matching_train_result_normalized_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        
        # option 2
        
        jointprob_matching_train_result_normalized_temp[j,1] <-   log10( jointprob_matching_train_normalized[[i]][j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_train_normalized[[i]][j,2]) * wimweight_su[2] +
          log10( jointprob_matching_train_normalized[[i]][j,3]) * wimweight_su[3] +
          log10( jointprob_matching_train_normalized[[i]][j,4]) * wimweight_su[4] +
          log10( jointprob_matching_train_normalized[[i]][j,5]) * wimweight_su[5] +
          log10( jointprob_matching_train_normalized[[i]][j,6]) * wimweight_su[6] +
          log10( jointprob_matching_train_normalized[[i]][j,7]) * wimweight_su[7] +
          log10( jointprob_matching_train_normalized[[i]][j,13]) * wimweight_su[13] +
          log10( jointprob_matching_train_normalized[[i]][j,14]) * wimweight_su[14] +
          log10( jointprob_matching_train_normalized[[i]][j,15]) * wimweight_su[15] +
          log10( jointprob_matching_train_normalized[[i]][j,16]) * wimweight_su[16] +
          log10( jointprob_matching_train_normalized[[i]][j,17]) * wimweight_su[17] +
          log10( jointprob_matching_train_normalized[[i]][j,18]) * wimweight_su[18] +
          log10( jointprob_matching_train_normalized[[i]][j,19]) * wimweight_su[19] +
          log10( jointprob_matching_train_normalized[[i]][j,20]) * wimweight_su[20] +
          log10( jointprob_matching_train_normalized[[i]][j,21]) * wimweight_su[21] +
          log10( jointprob_matching_train_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_matching_train_result_normalized_temp[j,2] <- log10( jointprob_matching_train[[i]][j,31]) * wimweight_su[31]
        jointprob_matching_train_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_train_result_normalized_temp[j,3] <- jointprob_matching_train_result_normalized_temp[j,3] + 
            log10(jointprob_matching_train_sig_normalized[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_train_result_normalized_temp[j,4] <-  jointprob_matching_train_result_normalized_temp[j,1] + 
          jointprob_matching_train_result_normalized_temp[j,2] 
        jointprob_matching_train_result_normalized_temp[j,5] <-  jointprob_matching_train_result_normalized_temp[j,1] +
          jointprob_matching_train_result_normalized_temp[j,3]  
        jointprob_matching_train_result_normalized_temp[j,6] <-  jointprob_matching_train_result_normalized_temp[j,1] +
          jointprob_matching_train_result_normalized_temp[j,2]  + jointprob_matching_train_result_normalized_temp[j,3]  
        
      }
      
      jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- 
        jointprob_matching_train_result_normalized_temp
      jointprob_matching_train_result_normalized_temp <- data.frame()
      
    }
  }
#   else # class is not 9
#     
#   {
#     jointprob_matching_train_result_normalized[[length(jointprob_matching_train_result_normalized) + 1]] <- NA
#   }
  
}


# normalized train missing

jointprob_missing_train_result_normalized_temp <- data.frame()
jointprob_missing_train_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_train_missing)){
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
               
        # option 2
        
        jointprob_missing_train_result_normalized_temp[j,1] <-   log10( jointprob_missing_train_normalized[[i]][j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_train_normalized[[i]][j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_train_normalized[[i]][j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_train_normalized[[i]][j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_train_normalized[[i]][j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_train_normalized[[i]][j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_train_normalized[[i]][j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_train_normalized[[i]][j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_train_normalized[[i]][j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_train_normalized[[i]][j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_train_normalized[[i]][j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_train_normalized[[i]][j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_train_normalized[[i]][j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_train_normalized[[i]][j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_train_normalized[[i]][j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_train_normalized[[i]][j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_train_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_missing_train_result_normalized_temp[j,2] <- log10( jointprob_missing_train[[i]][j,31]) * wimweight_tt[31]
        jointprob_missing_train_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_normalized_temp[j,3] <- jointprob_missing_train_result_normalized_temp[j,3] + 
            log10(jointprob_missing_train_sig[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_train_result_normalized_temp[j,4] <-  jointprob_missing_train_result_normalized_temp[j,1] + 
          jointprob_missing_train_result_normalized_temp[j,2] 
        jointprob_missing_train_result_normalized_temp[j,5] <-  jointprob_missing_train_result_normalized_temp[j,1] +
          jointprob_missing_train_result_normalized_temp[j,3]  
        jointprob_missing_train_result_normalized_temp[j,6] <-  jointprob_missing_train_result_normalized_temp[j,1] +
          jointprob_missing_train_result_normalized_temp[j,2] + jointprob_missing_train_result_normalized_temp[j,3] 
        
      }
     
      
      jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- 
        jointprob_missing_train_result_normalized_temp

      jointprob_missing_train_result_normalized_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_train [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_train_missing[[i]][1,1] )  ) {
      jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        # option 2
        
        jointprob_missing_train_result_normalized_temp[j,1] <-   log10( jointprob_missing_train_normalized[[i]][j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_train_normalized[[i]][j,2]) * wimweight_su[2] +
          log10( jointprob_missing_train_normalized[[i]][j,3]) * wimweight_su[3] +
          log10( jointprob_missing_train_normalized[[i]][j,4]) * wimweight_su[4] +
          log10( jointprob_missing_train_normalized[[i]][j,5]) * wimweight_su[5] +
          log10( jointprob_missing_train_normalized[[i]][j,6]) * wimweight_su[6] +
          log10( jointprob_missing_train_normalized[[i]][j,7]) * wimweight_su[7] +
          log10( jointprob_missing_train_normalized[[i]][j,13]) * wimweight_su[13] +
          log10( jointprob_missing_train_normalized[[i]][j,14]) * wimweight_su[14] +
          log10( jointprob_missing_train_normalized[[i]][j,15]) * wimweight_su[15] +
          log10( jointprob_missing_train_normalized[[i]][j,16]) * wimweight_su[16] +
          log10( jointprob_missing_train_normalized[[i]][j,17]) * wimweight_su[17] +
          log10( jointprob_missing_train_normalized[[i]][j,18]) * wimweight_su[18] +
          log10( jointprob_missing_train_normalized[[i]][j,19]) * wimweight_su[19] +
          log10( jointprob_missing_train_normalized[[i]][j,20]) * wimweight_su[20] +
          log10( jointprob_missing_train_normalized[[i]][j,21]) * wimweight_su[21] +
          log10( jointprob_missing_train_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_missing_train_result_normalized_temp[j,2] <- log10( jointprob_missing_train[[i]][j,31]) * wimweight_su[31]
        jointprob_missing_train_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_train_result_normalized_temp[j,3] <- jointprob_missing_train_result_normalized_temp[j,3] + 
            log10(jointprob_missing_train_sig[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_train_result_normalized_temp[j,4] <-  jointprob_missing_train_result_normalized_temp[j,1] + 
          jointprob_missing_train_result_normalized_temp[j,2] 
        jointprob_missing_train_result_normalized_temp[j,5] <-  jointprob_missing_train_result_normalized_temp[j,1] +
          jointprob_missing_train_result_normalized_temp[j,3]  
        jointprob_missing_train_result_normalized_temp[j,6] <-  jointprob_missing_train_result_normalized_temp[j,1] +
          jointprob_missing_train_result_normalized_temp[j,2] + jointprob_missing_train_result_normalized_temp[j,3] 
        
      }
      
      
      jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- 
        jointprob_missing_train_result_normalized_temp
      
      jointprob_missing_train_result_normalized_temp <- data.frame()
      
    }
  }
  
#   else # class is not 9
#     
#   {
#     jointprob_missing_train_result_normalized[[length(jointprob_missing_train_result_normalized) + 1]] <- NA
#   }
  
}




# test - matching



# joint prob 
jointprob_matching_test <- list()
jointprob_matching_test_sig <- list()
jointprob_matching_test_result <- list()


jointprob_matching_test_temp <- data.frame()
jointprob_matching_test_sig_temp <- data.frame()
jointprob_matching_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test[[length(jointprob_matching_test) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_tt[[m]], 
                                                                      normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
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
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                                                         normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_matching_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf_matching
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf_matching
        
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
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_matching_test_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_matching_test_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_matching_test_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_matching_test_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_matching_test_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_matching_test_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_matching_test_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_matching_test_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_matching_test_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_matching_test_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_matching_test_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_matching_test_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_matching_test_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_matching_test_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_matching_test_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_matching_test_temp[j,30]) * wimweight_tt[30] 
        #                                               log10(    1 - ( alphasig * jointprob_matching_test[i,31] ) )
        #                                               log10( jointprob_matching_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_matching_test[i,33] <-  log10( 1/ jointprob_matching_test[i,31]) * weight[31]  
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        jointprob_matching_test_result_temp[j,6] <-  jointprob_matching_test_result_temp[j,1]  + 
                                                     jointprob_matching_test_result_temp[j,2]  +jointprob_matching_test_result_temp[j,3]  
        
      }
      
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test[[length( jointprob_matching_test) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) < 8 ) { # SU
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test[[length(jointprob_matching_test) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_c_su[[m]], 
                                                                      normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
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
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                                                         normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <- buf_matching_sig 
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_matching_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf_matching
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf_matching
        
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
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_test_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_matching_test_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_matching_test_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_matching_test_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_matching_test_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_matching_test_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_matching_test_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_matching_test_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_matching_test_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_matching_test_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_matching_test_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_matching_test_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_matching_test_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_matching_test_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_matching_test_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_matching_test_temp[j,30]) * wimweight_su[30] 
        #                                               log10(    1 - ( alphasig * jointprob_matching_test[i,31] ) )
        #                                               log10( jointprob_matching_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_matching_test[i,33] <-  log10( 1/ jointprob_matching_test[i,31]) * weight[31]  
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * wimweight_su[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        jointprob_matching_test_result_temp[j,6] <-  jointprob_matching_test_result_temp[j,1]  + 
          jointprob_matching_test_result_temp[j,2]  +jointprob_matching_test_result_temp[j,3]  
        
      }
      
      jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test[[length( jointprob_matching_test) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
#   else # class is not 9
#     
#   {
#     jointprob_matching_test[[length(jointprob_matching_test) + 1 ]] <-  NA
#     jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
#     jointprob_matching_test_result[[length(jointprob_matching_test_result) + 1]] <- NA
#   }
  
}

# test - missing

jointprob_missing_test <- list()
jointprob_missing_test_sig <- list()
jointprob_missing_test_result <- list()

jointprob_missing_test_temp <- data.frame()
jointprob_missing_test_sig_temp <- data.frame()
jointprob_missing_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test[[length(jointprob_missing_test) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_tt[[m]], 
                                                                     normal_nonmat_c_tt[[m]]  * multiplier_hist_nonmat_c_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[m]] ) ) ] ,
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
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                                                                        normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_missing_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf_missing
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf_missing
        
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
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_test_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_test_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_test_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_test_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_test_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_test_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_test_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_test_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_test_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_test_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_test_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_test_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_test_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_test_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_test_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_test_temp[j,30]) * wimweight_tt[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_test[i,31] ) )
        #                                               log10( jointprob_missing_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_missing_test[i,33] <-  log10( 1/ jointprob_missing_test[i,31]) * weight[31]  
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        jointprob_missing_test_result_temp[j,6] <-  jointprob_missing_test_result_temp[j,1]  +
                                                    jointprob_missing_test_result_temp[j,2]  + jointprob_missing_test_result_temp[j,3] 
         
      }
      
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test[[length( jointprob_missing_test) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test[[length(jointprob_missing_test) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
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
          if ( m %in% classallidxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_c_su[[m]], 
                                                                     normal_nonmat_c_su[[m]]  * multiplier_hist_nonmat_c_su[[m]][ which.min(is.na( multiplier_hist_nonmat_c_su[[m]] ) ) ] ,
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
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                                                                        normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_missing_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf_missing
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf_missing
        
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
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_test_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_missing_test_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_missing_test_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_missing_test_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_missing_test_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_missing_test_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_missing_test_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_missing_test_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_missing_test_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_missing_test_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_missing_test_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_missing_test_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_missing_test_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_missing_test_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_missing_test_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_missing_test_temp[j,30]) * wimweight_su[30] 
        #                                               log10(    1 - ( alphasig * jointprob_missing_test[i,31] ) )
        #                                               log10( jointprob_missing_test[i,31]) * weight[31] 
        #                                               log10(1/Upcandidates_attribute_test_missing[i,31]) * weight[31] 
        #       jointprob_missing_test[i,33] <-  log10( 1/ jointprob_missing_test[i,31]) * weight[31]  
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * wimweight_su[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        jointprob_missing_test_result_temp[j,6] <-  jointprob_missing_test_result_temp[j,1]  +
          jointprob_missing_test_result_temp[j,2]  + jointprob_missing_test_result_temp[j,3] 
        
      }
      
      jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test[[length( jointprob_missing_test) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
  
#   else # class is not 9
#     
#   {
#     jointprob_missing_test[[length(jointprob_missing_test) + 1 ]] <-  NA
#     jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
#     jointprob_missing_test_result[[length(jointprob_missing_test_result) + 1]] <- NA
#   }
  
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
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8) { # TT
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_n[[length(jointprob_matching_test_n) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_tt[[m]], 
                                                                      normal_mat_n_tt[[m]]  * multiplier_hist_mat_n_tt[[m]][ which.min(is.na( multiplier_hist_mat_n_tt[[m]] ) ) ] ,
                                                                      (( Upcandidates_attribute_test_missing[[i]][j,m] - min_train_mat_tt[m] ) / ( max_train_mat_tt[m] - min_train_mat_tt[m])) )$y )
            jointprob_matching_test_temp[j,m] [is.na(jointprob_matching_test_temp[j,m])] <- buf_matching 
          }
          
          
          
          else {
            jointprob_matching_test_temp[j,m] <- 99999
          }
        }
        
        
        for (n in 1: 50) {
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                   normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                   Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <-  buf_matching_sig
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_matching_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf_matching
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf_matching
        
        
        # option 2
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_matching_test_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_matching_test_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_matching_test_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_matching_test_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_matching_test_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_matching_test_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_matching_test_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_matching_test_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_matching_test_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_matching_test_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_matching_test_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_matching_test_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_matching_test_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_matching_test_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_matching_test_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_matching_test_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * wimweight_tt[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        jointprob_matching_test_result_temp[j,6] <-  jointprob_matching_test_result_temp[j,1]  +
                                                     jointprob_matching_test_result_temp[j,2]  + jointprob_matching_test_result_temp[j,3]   
        
      }
      
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test_n[[length( jointprob_matching_test_n) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) < 8) { # SU
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_n[[length(jointprob_matching_test_n) + 1 ]] <-  NA
      jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_matching_test_temp[j,m] <- as.numeric ( approx( diffseq_mat_n_su[[m]], 
                                                                      normal_mat_n_su[[m]]  * multiplier_hist_mat_n_su[[m]][ which.min(is.na( multiplier_hist_mat_n_su[[m]] ) ) ] ,
                                                                      (( Upcandidates_attribute_test_missing[[i]][j,m] - min_train_mat_su[m] ) / ( max_train_mat_su[m] - min_train_mat_su[m])) )$y )
            jointprob_matching_test_temp[j,m] [is.na(jointprob_matching_test_temp[j,m])] <- buf_matching 
          }
          
          
          
          else {
            jointprob_matching_test_temp[j,m] <- 99999
          }
        }
        
        
        for (n in 1: 50) {
          
          jointprob_matching_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                                                         normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                         Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_matching_test_sig_temp[j,n] [is.na(  jointprob_matching_test_sig_temp[j,n])] <- buf_matching_sig 
        }
        
        
        jointprob_matching_test_temp[j,31] [is.na(jointprob_matching_test_temp[j,31] )] <-  buf_matching_sig
        
        jointprob_matching_test_sig_temp[is.na(jointprob_matching_test_sig_temp)] <- buf_matching_sig 
        jointprob_matching_test_sig_temp[jointprob_matching_test_sig_temp == 0 ] <- buf_matching_sig 
        jointprob_matching_test_temp[is.na (jointprob_matching_test_temp)] <- buf_matching
        jointprob_matching_test_temp[ jointprob_matching_test_temp == 0] <- buf_matching
        
        
        # option 2
        
        jointprob_matching_test_result_temp[j,1] <-   log10( jointprob_matching_test_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_test_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_matching_test_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_matching_test_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_matching_test_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_matching_test_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_matching_test_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_matching_test_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_matching_test_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_matching_test_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_matching_test_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_matching_test_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_matching_test_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_matching_test_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_matching_test_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_matching_test_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_matching_test_temp[j,30]) * wimweight_su[30] 
        
        jointprob_matching_test_result_temp[j,2] <- log10( jointprob_matching_test_temp[j,31]) * wimweight_su[31]
        jointprob_matching_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_temp[j,3] <- jointprob_matching_test_result_temp[j,3] + 
            log10(jointprob_matching_test_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_test_result_temp[j,4] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,2] 
        jointprob_matching_test_result_temp[j,5] <-  jointprob_matching_test_result_temp[j,1]  + jointprob_matching_test_result_temp[j,3]  
        jointprob_matching_test_result_temp[j,6] <-  jointprob_matching_test_result_temp[j,1]  +
          jointprob_matching_test_result_temp[j,2]  + jointprob_matching_test_result_temp[j,3]   
        
      }
      
      jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- jointprob_matching_test_result_temp
      jointprob_matching_test_sig[[length( jointprob_matching_test_sig) +1]] <-  jointprob_matching_test_sig_temp
      jointprob_matching_test_n[[length( jointprob_matching_test_n) +1]] <-  jointprob_matching_test_temp
      
      jointprob_matching_test_temp <- data.frame()
      jointprob_matching_test_sig_temp <- data.frame()
      jointprob_matching_test_result_temp <- data.frame()
      
    }
  }
  
  
#   else # class is not 9
#     
#   {
#     jointprob_matching_test_n[[length(jointprob_matching_test_n) + 1 ]] <-  NA
#     jointprob_matching_test_sig[[length(jointprob_matching_test_sig) + 1 ]] <-  NA
#     jointprob_matching_test_result_n[[length(jointprob_matching_test_result_n) + 1]] <- NA
#   }
  
}

# test - missing

jointprob_missing_test_n <- list()
jointprob_missing_test_sig <- list()
jointprob_missing_test_result_n <- list()

jointprob_missing_test_temp <- data.frame()
jointprob_missing_test_sig_temp <- data.frame()
jointprob_missing_test_result_temp <- data.frame()



for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_n[[length(jointprob_missing_test_n) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_tt[[m]], 
                                                                     normal_nonmat_n_tt[[m]]  * multiplier_hist_nonmat_n_tt[[m]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[m]] ) ) ] ,
                                                                     (( Upcandidates_attribute_test_missing[[i]][j,m] - min_train_nonmat_tt[m] ) / ( max_train_nonmat_tt[m] - min_train_nonmat_tt[m])) )$y )
            jointprob_missing_test_temp[j,m] [is.na(jointprob_missing_test_temp[j,m])] <- buf_missing 
          }
          
          
          else {
            jointprob_missing_test_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_tt[[n]],  
                                                                        normal_nonmat_c_sig_tt[[n]]  *  multiplier_hist_nonmat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_missing_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf_missing
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf_missing
        
        
        
        # option 2
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_test_temp[j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_test_temp[j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_test_temp[j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_test_temp[j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_test_temp[j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_test_temp[j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_test_temp[j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_test_temp[j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_test_temp[j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_test_temp[j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_test_temp[j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_test_temp[j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_test_temp[j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_test_temp[j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_test_temp[j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_test_temp[j,30]) * wimweight_tt[30] 
        
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * wimweight_tt[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        jointprob_missing_test_result_temp[j,6] <-  jointprob_missing_test_result_temp[j,1]  + 
                                                    jointprob_missing_test_result_temp[j,2]  + jointprob_missing_test_result_temp[j,3] 
        
      }
      
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test_n[[length( jointprob_missing_test_n) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
  if ( as.numeric (Downtarget_attributes_test [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_n[[length(jointprob_missing_test_n) + 1 ]] <-  NA
      jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        for (m in 1: 31) { 
          
          
          # option 2 - parametric
          if ( m %in% classallidxprob) {
            jointprob_missing_test_temp[j,m] <- as.numeric ( approx( diffseq_nonmat_n_su[[m]], 
                                                                     normal_nonmat_n_su[[m]]  * multiplier_hist_nonmat_n_su[[m]][ which.min(is.na( multiplier_hist_nonmat_n_su[[m]] ) ) ] ,
                                                                     (( Upcandidates_attribute_test_missing[[i]][j,m] - min_train_nonmat_su[m] ) / ( max_train_nonmat_su[m] - min_train_nonmat_su[m])) )$y )
            jointprob_missing_test_temp[j,m] [is.na(jointprob_missing_test_temp[j,m])] <- buf_missing 
          }
          
          
          else {
            jointprob_missing_test_temp[j,m] <- 99999
          }
        }
        
        for (n in 1: 50) {
          
          jointprob_missing_test_sig_temp[j,n] <- as.numeric ( (approx( diffseq_nonmat_c_sig_su[[n]],  
                                                                        normal_nonmat_c_sig_su[[n]]  *  multiplier_hist_nonmat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_nonmat_c_sig_su[[n]] ) ) ] ,
                                                                        Upcandidates_attribute_test_missing_sig[[i]][j,n]) )$y )
          
          jointprob_missing_test_sig_temp[j,n] [is.na(  jointprob_missing_test_sig_temp[j,n])] <- buf_missing_sig 
        }
        
        
        jointprob_missing_test_temp[j,31] [is.na(jointprob_missing_test_temp[j,31] )] <- buf_missing_sig 
        
        jointprob_missing_test_sig_temp[is.na(jointprob_missing_test_sig_temp)] <- buf_missing_sig 
        jointprob_missing_test_sig_temp[jointprob_missing_test_sig_temp == 0 ] <- buf_missing_sig 
        jointprob_missing_test_temp[is.na (jointprob_missing_test_temp)] <- buf_missing
        jointprob_missing_test_temp[ jointprob_missing_test_temp == 0] <- buf_missing
        
        
        
        # option 2
        
        jointprob_missing_test_result_temp[j,1] <-   log10( jointprob_missing_test_temp[j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_test_temp[j,2]) * wimweight_su[2] +
          log10( jointprob_missing_test_temp[j,3]) * wimweight_su[3] +
          log10( jointprob_missing_test_temp[j,4]) * wimweight_su[4] +
          log10( jointprob_missing_test_temp[j,5]) * wimweight_su[5] +
          log10( jointprob_missing_test_temp[j,6]) * wimweight_su[6] +
          log10( jointprob_missing_test_temp[j,7]) * wimweight_su[7] +
          log10( jointprob_missing_test_temp[j,13]) * wimweight_su[13] +
          log10( jointprob_missing_test_temp[j,14]) * wimweight_su[14] +
          log10( jointprob_missing_test_temp[j,15]) * wimweight_su[15] +
          log10( jointprob_missing_test_temp[j,16]) * wimweight_su[16] +
          log10( jointprob_missing_test_temp[j,17]) * wimweight_su[17] +
          log10( jointprob_missing_test_temp[j,18]) * wimweight_su[18] +
          log10( jointprob_missing_test_temp[j,19]) * wimweight_su[19] +
          log10( jointprob_missing_test_temp[j,20]) * wimweight_su[20] +
          log10( jointprob_missing_test_temp[j,21]) * wimweight_su[21] +
          log10( jointprob_missing_test_temp[j,30]) * wimweight_su[30] 
        
        jointprob_missing_test_result_temp[j,2] <- log10( jointprob_missing_test_temp[j,31]) * wimweight_su[31]
        jointprob_missing_test_result_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_temp[j,3] <- jointprob_missing_test_result_temp[j,3] + 
            log10(jointprob_missing_test_sig_temp[j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_test_result_temp[j,4] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,2] 
        jointprob_missing_test_result_temp[j,5] <-  jointprob_missing_test_result_temp[j,1]  + jointprob_missing_test_result_temp[j,3]  
        jointprob_missing_test_result_temp[j,6] <-  jointprob_missing_test_result_temp[j,1]  + 
          jointprob_missing_test_result_temp[j,2]  + jointprob_missing_test_result_temp[j,3] 
        
      }
      
      jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- jointprob_missing_test_result_temp
      jointprob_missing_test_sig[[length( jointprob_missing_test_sig) +1]] <-  jointprob_missing_test_sig_temp
      jointprob_missing_test_n[[length( jointprob_missing_test_n) +1]] <-  jointprob_missing_test_temp
      
      jointprob_missing_test_temp <- data.frame()
      jointprob_missing_test_sig_temp <- data.frame()
      jointprob_missing_test_result_temp <- data.frame()
      
    }
  }
#   else # class is not 9
#     
#   {
#     jointprob_missing_test_n[[length(jointprob_missing_test_n) + 1 ]] <-  NA
#     jointprob_missing_test_sig[[length(jointprob_missing_test_sig) + 1 ]] <-  NA
#     jointprob_missing_test_result_n[[length(jointprob_missing_test_result_n) + 1]] <- NA
#   }
  
}

# jointprob_missing_test <- jointprob_missing_test_n
# jointprob_matching_test <- jointprob_matching_test_n
# jointprob_missing_test_result <- jointprob_missing_test_result_n
# jointprob_matching_test_result <- jointprob_matching_test_result_n
# 
# jointprob_missing_test_result <- jointprob_missing_test_result_n
# jointprob_matching_test_result <- jointprob_matching_test_result_n
# jointprob_missing_test <- jointprob_missing_test_n
# jointprob_matching_test <- jointprob_matching_test_n



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


# test matching and missing prob normalization
jointprob_matching_test_sig_normalized <- list()
jointprob_missing_test_sig_normalized <- list()

jointprob_matching_test_sig_normalized_temp <- data.frame()
jointprob_missing_test_sig_normalized_temp <- data.frame()


for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( !is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
    for (j in 1 : length(Upcandidates_attribute_test_missing[[i]][,1] )){
      for (k in 1 : length(Upcandidates_attribute_test_missing_sig[[i]] )){
        
        jointprob_matching_test_sig_normalized_temp[j,k] = 
          jointprob_matching_test_sig[[i]][j,k]  / (  jointprob_matching_test_sig[[i]][j,k] +  jointprob_missing_test_sig[[i]][j,k] )
        
        jointprob_missing_test_sig_normalized_temp[j,k] = 
          jointprob_missing_test_sig[[i]][j,k]  / (  jointprob_matching_test_sig[[i]][j,k] +  jointprob_missing_test_sig[[i]][j,k] )
        
      }  
    } 
  }
  
  
  else{
    jointprob_matching_test_sig_normalized_temp[j,k]  <- NA
    jointprob_missing_test_sig_normalized_temp[j,k]  <- NA
  }
  
  
  jointprob_matching_test_sig_normalized[[length( jointprob_matching_test_sig_normalized) + 1 ]]  <- jointprob_matching_test_sig_normalized_temp
  jointprob_missing_test_sig_normalized[[length( jointprob_missing_test_sig_normalized) + 1 ]]  <- jointprob_missing_test_sig_normalized_temp
  
}


# normalized test matching
jointprob_matching_test_result_normalized_temp <- data.frame()
jointprob_matching_test_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { # TT
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        # option 2
        
        jointprob_matching_test_result_normalized_temp[j,1] <-   log10( jointprob_matching_test_normalized[[i]][j,1]) * wimweight_tt[1] +  
          log10( jointprob_matching_test_normalized[[i]][j,2]) * wimweight_tt[2] +
          log10( jointprob_matching_test_normalized[[i]][j,3]) * wimweight_tt[3] +
          log10( jointprob_matching_test_normalized[[i]][j,4]) * wimweight_tt[4] +
          log10( jointprob_matching_test_normalized[[i]][j,5]) * wimweight_tt[5] +
          log10( jointprob_matching_test_normalized[[i]][j,6]) * wimweight_tt[6] +
          log10( jointprob_matching_test_normalized[[i]][j,7]) * wimweight_tt[7] +
          log10( jointprob_matching_test_normalized[[i]][j,13]) * wimweight_tt[13] +
          log10( jointprob_matching_test_normalized[[i]][j,14]) * wimweight_tt[14] +
          log10( jointprob_matching_test_normalized[[i]][j,15]) * wimweight_tt[15] +
          log10( jointprob_matching_test_normalized[[i]][j,16]) * wimweight_tt[16] +
          log10( jointprob_matching_test_normalized[[i]][j,17]) * wimweight_tt[17] +
          log10( jointprob_matching_test_normalized[[i]][j,18]) * wimweight_tt[18] +
          log10( jointprob_matching_test_normalized[[i]][j,19]) * wimweight_tt[19] +
          log10( jointprob_matching_test_normalized[[i]][j,20]) * wimweight_tt[20] +
          log10( jointprob_matching_test_normalized[[i]][j,21]) * wimweight_tt[21] +
          log10( jointprob_matching_test_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_matching_test_result_normalized_temp[j,2] <- log10( jointprob_matching_test[[i]][j,31]) * wimweight_tt[31]
        jointprob_matching_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_normalized_temp[j,3] <- jointprob_matching_test_result_normalized_temp[j,3] + 
            log10(jointprob_matching_test_sig_normalized[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_matching_test_result_normalized_temp[j,4] <-  jointprob_matching_test_result_normalized_temp[j,1] + 
          jointprob_matching_test_result_normalized_temp[j,2] 
        jointprob_matching_test_result_normalized_temp[j,5] <-  jointprob_matching_test_result_normalized_temp[j,1] +
          jointprob_matching_test_result_normalized_temp[j,3]  
        jointprob_matching_test_result_normalized_temp[j,6] <-  jointprob_matching_test_result_normalized_temp[j,1] +
          jointprob_matching_test_result_normalized_temp[j,2]  +  jointprob_matching_test_result_normalized_temp[j,3]  
        
      }
      
  
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- 
        jointprob_matching_test_result_normalized_temp

      jointprob_matching_test_result_normalized_temp <- data.frame()
      
    }
  }
  
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        # option 2
        
        jointprob_matching_test_result_normalized_temp[j,1] <-   log10( jointprob_matching_test_normalized[[i]][j,1]) * wimweight_su[1] +  
          log10( jointprob_matching_test_normalized[[i]][j,2]) * wimweight_su[2] +
          log10( jointprob_matching_test_normalized[[i]][j,3]) * wimweight_su[3] +
          log10( jointprob_matching_test_normalized[[i]][j,4]) * wimweight_su[4] +
          log10( jointprob_matching_test_normalized[[i]][j,5]) * wimweight_su[5] +
          log10( jointprob_matching_test_normalized[[i]][j,6]) * wimweight_su[6] +
          log10( jointprob_matching_test_normalized[[i]][j,7]) * wimweight_su[7] +
          log10( jointprob_matching_test_normalized[[i]][j,13]) * wimweight_su[13] +
          log10( jointprob_matching_test_normalized[[i]][j,14]) * wimweight_su[14] +
          log10( jointprob_matching_test_normalized[[i]][j,15]) * wimweight_su[15] +
          log10( jointprob_matching_test_normalized[[i]][j,16]) * wimweight_su[16] +
          log10( jointprob_matching_test_normalized[[i]][j,17]) * wimweight_su[17] +
          log10( jointprob_matching_test_normalized[[i]][j,18]) * wimweight_su[18] +
          log10( jointprob_matching_test_normalized[[i]][j,19]) * wimweight_su[19] +
          log10( jointprob_matching_test_normalized[[i]][j,20]) * wimweight_su[20] +
          log10( jointprob_matching_test_normalized[[i]][j,21]) * wimweight_su[21] +
          log10( jointprob_matching_test_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_matching_test_result_normalized_temp[j,2] <- log10( jointprob_matching_test[[i]][j,31]) * wimweight_su[31]
        jointprob_matching_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_matching_test_result_normalized_temp[j,3] <- jointprob_matching_test_result_normalized_temp[j,3] + 
            log10(jointprob_matching_test_sig_normalized[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_matching_test_result_normalized_temp[j,4] <-  jointprob_matching_test_result_normalized_temp[j,1] + 
          jointprob_matching_test_result_normalized_temp[j,2] 
        jointprob_matching_test_result_normalized_temp[j,5] <-  jointprob_matching_test_result_normalized_temp[j,1] +
          jointprob_matching_test_result_normalized_temp[j,3]  
        jointprob_matching_test_result_normalized_temp[j,6] <-  jointprob_matching_test_result_normalized_temp[j,1] +
          jointprob_matching_test_result_normalized_temp[j,2]  +  jointprob_matching_test_result_normalized_temp[j,3]  
        
      }
      
      
      jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- 
        jointprob_matching_test_result_normalized_temp
      
      jointprob_matching_test_result_normalized_temp <- data.frame()
      
    }
  }
  
#   else # class is not 9
#     
#   {
#     jointprob_matching_test_result_normalized[[length(jointprob_matching_test_result_normalized) + 1]] <- NA
#   }
#   
}



# normalized test missing
jointprob_missing_test_result_normalized_temp <- data.frame()
jointprob_missing_test_result_normalized <- list()

for (i in 1:length(Upcandidates_attribute_test_missing)){
  
  if ( as.numeric (Downtarget_attributes_test [i,2] ) >= 8 ) { # tt
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        

        # option 2
        
        jointprob_missing_test_result_normalized_temp[j,1] <-   log10( jointprob_missing_test_normalized[[i]][j,1]) * wimweight_tt[1] +  
          log10( jointprob_missing_test_normalized[[i]][j,2]) * wimweight_tt[2] +
          log10( jointprob_missing_test_normalized[[i]][j,3]) * wimweight_tt[3] +
          log10( jointprob_missing_test_normalized[[i]][j,4]) * wimweight_tt[4] +
          log10( jointprob_missing_test_normalized[[i]][j,5]) * wimweight_tt[5] +
          log10( jointprob_missing_test_normalized[[i]][j,6]) * wimweight_tt[6] +
          log10( jointprob_missing_test_normalized[[i]][j,7]) * wimweight_tt[7] +
          log10( jointprob_missing_test_normalized[[i]][j,13]) * wimweight_tt[13] +
          log10( jointprob_missing_test_normalized[[i]][j,14]) * wimweight_tt[14] +
          log10( jointprob_missing_test_normalized[[i]][j,15]) * wimweight_tt[15] +
          log10( jointprob_missing_test_normalized[[i]][j,16]) * wimweight_tt[16] +
          log10( jointprob_missing_test_normalized[[i]][j,17]) * wimweight_tt[17] +
          log10( jointprob_missing_test_normalized[[i]][j,18]) * wimweight_tt[18] +
          log10( jointprob_missing_test_normalized[[i]][j,19]) * wimweight_tt[19] +
          log10( jointprob_missing_test_normalized[[i]][j,20]) * wimweight_tt[20] +
          log10( jointprob_missing_test_normalized[[i]][j,21]) * wimweight_tt[21] +
          log10( jointprob_missing_test_normalized[[i]][j,30]) * wimweight_tt[30] 
        
        jointprob_missing_test_result_normalized_temp[j,2] <- log10( jointprob_missing_test[[i]][j,31]) * wimweight_tt[31]
        jointprob_missing_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_normalized_temp[j,3] <- jointprob_missing_test_result_normalized_temp[j,3] + 
            log10(jointprob_missing_test_sig_normalized[[i]][j,n]) * sigweight_tt[n]
        }
        
        jointprob_missing_test_result_normalized_temp[j,4] <-  jointprob_missing_test_result_normalized_temp[j,1] + 
          jointprob_missing_test_result_normalized_temp[j,2] 
        jointprob_missing_test_result_normalized_temp[j,5] <-  jointprob_missing_test_result_normalized_temp[j,1] +
          jointprob_missing_test_result_normalized_temp[j,3]  
        jointprob_missing_test_result_normalized_temp[j,6] <-  jointprob_missing_test_result_normalized_temp[j,1] +
          jointprob_missing_test_result_normalized_temp[j,2]  + jointprob_missing_test_result_normalized_temp[j,3]  
        
      }
      
      
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- 
        jointprob_missing_test_result_normalized_temp
    
      jointprob_missing_test_result_normalized_temp <- data.frame()
      
    }
  }
  
  if ( as.numeric (Downtarget_attributes_test [i,2] )  < 8 ) { # SU
    
    if ( is.na ( Upcandidates_attribute_test_missing[[i]][1,1] )  ) {
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- NA
    }
    
    else
    {
      for (j in 1:6){
        
        
        # option 2
        
        jointprob_missing_test_result_normalized_temp[j,1] <-   log10( jointprob_missing_test_normalized[[i]][j,1]) * wimweight_su[1] +  
          log10( jointprob_missing_test_normalized[[i]][j,2]) * wimweight_su[2] +
          log10( jointprob_missing_test_normalized[[i]][j,3]) * wimweight_su[3] +
          log10( jointprob_missing_test_normalized[[i]][j,4]) * wimweight_su[4] +
          log10( jointprob_missing_test_normalized[[i]][j,5]) * wimweight_su[5] +
          log10( jointprob_missing_test_normalized[[i]][j,6]) * wimweight_su[6] +
          log10( jointprob_missing_test_normalized[[i]][j,7]) * wimweight_su[7] +
          log10( jointprob_missing_test_normalized[[i]][j,13]) * wimweight_su[13] +
          log10( jointprob_missing_test_normalized[[i]][j,14]) * wimweight_su[14] +
          log10( jointprob_missing_test_normalized[[i]][j,15]) * wimweight_su[15] +
          log10( jointprob_missing_test_normalized[[i]][j,16]) * wimweight_su[16] +
          log10( jointprob_missing_test_normalized[[i]][j,17]) * wimweight_su[17] +
          log10( jointprob_missing_test_normalized[[i]][j,18]) * wimweight_su[18] +
          log10( jointprob_missing_test_normalized[[i]][j,19]) * wimweight_su[19] +
          log10( jointprob_missing_test_normalized[[i]][j,20]) * wimweight_su[20] +
          log10( jointprob_missing_test_normalized[[i]][j,21]) * wimweight_su[21] +
          log10( jointprob_missing_test_normalized[[i]][j,30]) * wimweight_su[30] 
        
        jointprob_missing_test_result_normalized_temp[j,2] <- log10( jointprob_missing_test[[i]][j,31]) * wimweight_su[31]
        jointprob_missing_test_result_normalized_temp[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprob_missing_test_result_normalized_temp[j,3] <- jointprob_missing_test_result_normalized_temp[j,3] + 
            log10(jointprob_missing_test_sig_normalized[[i]][j,n]) * sigweight_su[n]
        }
        
        jointprob_missing_test_result_normalized_temp[j,4] <-  jointprob_missing_test_result_normalized_temp[j,1] + 
          jointprob_missing_test_result_normalized_temp[j,2] 
        jointprob_missing_test_result_normalized_temp[j,5] <-  jointprob_missing_test_result_normalized_temp[j,1] +
          jointprob_missing_test_result_normalized_temp[j,3]  
        jointprob_missing_test_result_normalized_temp[j,6] <-  jointprob_missing_test_result_normalized_temp[j,1] +
          jointprob_missing_test_result_normalized_temp[j,2]  + jointprob_missing_test_result_normalized_temp[j,3]  
        
      }
      
      
      jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- 
        jointprob_missing_test_result_normalized_temp
      
      jointprob_missing_test_result_normalized_temp <- data.frame()
      
    }
  }
  
#   else # class is not 9
#     
#   {
#     jointprob_missing_test_result_normalized[[length(jointprob_missing_test_result_normalized) + 1]] <- NA
#   }
  
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
# 
weightwim <- 3
weightsig1 <- 0
weightsig2 <- 1

for ( weightwim in seq(from=1, to=3, by=1)) {
#   for ( weightsig1 in seq(from=1, to=3, by=1)) {
    for ( weightsig2 in seq(from=1, to=3, by=1)) {
      
      #  result normalization - train
      for (i in 1:length(Upcandidates_attribute_train_missing)){
        
#         if ( as.numeric (Downtarget_attributes_train [i,2] ) >= 8 ) { 
          
        if (!is.na(jointprob_matching_train_result[[i]][1][1])) {
          
          for (j in 1: 6 ) {
            
            jointprob_matching_train_result[[i]][j,7] <- (jointprob_matching_train_result[[i]][j,1] -
                                                            ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
            jointprob_missing_train_result[[i]][j,7] <- (jointprob_missing_train_result[[i]][j,1] -
                                                           ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
            jointprob_matching_train_result[[i]][j,8] <- (jointprob_matching_train_result[[i]][j,2] -
                                                            ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
            jointprob_missing_train_result[[i]][j,8] <- (jointprob_missing_train_result[[i]][j,2] -
                                                           ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
            jointprob_matching_train_result[[i]][j,9] <- (jointprob_matching_train_result[[i]][j,3] -
                                                            ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
            jointprob_missing_train_result[[i]][j,9] <- (jointprob_missing_train_result[[i]][j,3] -
                                                           ( min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result[[i]][,1:3]) , unlist(jointprob_missing_train_result[[i]][,1:3])) )
            
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
                                                                       ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
            jointprob_missing_train_result_normalized[[i]][j,7] <- (jointprob_missing_train_result_normalized[[i]][j,1] -
                                                                      ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
            jointprob_matching_train_result_normalized[[i]][j,8] <- (jointprob_matching_train_result_normalized[[i]][j,2] -
                                                                       ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
            jointprob_missing_train_result_normalized[[i]][j,8] <- (jointprob_missing_train_result_normalized[[i]][j,2] -
                                                                      ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
            jointprob_matching_train_result_normalized[[i]][j,9] <- (jointprob_matching_train_result_normalized[[i]][j,3] -
                                                                       ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
            jointprob_missing_train_result_normalized[[i]][j,9] <- (jointprob_missing_train_result_normalized[[i]][j,3] -
                                                                      ( min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) ) )/ 
              ( max(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) - 
                  min(unlist(jointprob_matching_train_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_train_result_normalized[[i]][,1:3])) )
            
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
                                                             ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
              jointprob_missing_test_result[[i]][j,7] <- (jointprob_missing_test_result[[i]][j,1] -
                                                            ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
              jointprob_matching_test_result[[i]][j,8] <- (jointprob_matching_test_result[[i]][j,2] -
                                                             ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
              jointprob_missing_test_result[[i]][j,8] <- (jointprob_missing_test_result[[i]][j,2] -
                                                            ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
              jointprob_matching_test_result[[i]][j,9] <- (jointprob_matching_test_result[[i]][j,3] -
                                                             ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
              jointprob_missing_test_result[[i]][j,9] <- (jointprob_missing_test_result[[i]][j,3] -
                                                            ( min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result[[i]][,1:3]) , unlist(jointprob_missing_test_result[[i]][,1:3])) ))
              
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
                                                                        ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
              jointprob_missing_test_result_normalized[[i]][j,7] <- (jointprob_missing_test_result_normalized[[i]][j,1] -
                                                                       ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
              jointprob_matching_test_result_normalized[[i]][j,8] <- (jointprob_matching_test_result_normalized[[i]][j,2] -
                                                                        ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
              jointprob_missing_test_result_normalized[[i]][j,8] <- (jointprob_missing_test_result_normalized[[i]][j,2] -
                                                                       ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
              jointprob_matching_test_result_normalized[[i]][j,9] <- (jointprob_matching_test_result_normalized[[i]][j,3] -
                                                                        ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
              jointprob_missing_test_result_normalized[[i]][j,9] <- (jointprob_missing_test_result_normalized[[i]][j,3] -
                                                                       ( min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ) )/ 
                (( max(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) - 
                     min(unlist(jointprob_matching_test_result_normalized[[i]][,1:3]) , unlist(jointprob_missing_test_result_normalized[[i]][,1:3])) ))
              
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

  

# ALL unit
TargetTable_train_all <- subset( ResultMisMatching_train , as.numeric(ResultMisMatching_train[,1]) >= 4 ) # CHANGE


Target_obj_train_all   <- TargetTable_train_all [,2]

missing_obj_train_all   <- length (Target_obj_train_all [Target_obj_train_all  == 999]) 
matching_obj_train_all  <- length (Target_obj_train_all [Target_obj_train_all  != 999]) 


CVeh_train_all  <- matching_obj_train_all [1]
Veh_train_all  <- length(TargetTable_train_all [,1])

matching_NN_train_all  <- vector()
missing_NN_train_all  <- vector()
CMVeh_train_all  <- vector()
MVeh_train_all  <- vector()
MMVeh_train_all  <- vector()
SIMR_train_all  <- vector()
SCMR_train_all  <- vector()
SER_train_all  <- vector()


for (i in 1:12) {
  CMVeh_train_all[i] <- sum ( as.numeric ((TargetTable_train_all [,2]) == as.numeric (TargetTable_train_all [,i+9])) &
                                as.numeric (TargetTable_train_all [,2]) != 999)
  MVeh_train_all[i] <- sum(   (as.numeric( TargetTable_train_all[,i+9])) > 1000 ) 
  
  MMVeh_train_all[i] <- length(  subset(TargetTable_train_all[,1], as.numeric( Target_obj_train_all ) 
                                        !=  as.numeric( TargetTable_train_all[,i+9])   ))
  
  SIMR_train_all[i] <- CMVeh_train_all[i] / CVeh_train_all[1]
  SCMR_train_all[i] <- CMVeh_train_all[i] / MVeh_train_all[i]
  SER_train_all[i] <- MMVeh_train_all[i] / Veh_train_all[1]
  
  ResultMissing_train_all <- rbind(  ResultMissing_train_all  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_train_all[1], missing_obj_train_all[1],              
                                                                    CMVeh_train_all[[i]], CVeh_train_all[[1]], MVeh_train_all[[i]] ,
                                                                    SIMR_train_all[[i]], SCMR_train_all[[i]], MMVeh_train_all[[i]], Veh_train_all[[1]], SER_train_all[[i]] ))
  
}

# TT unit
TargetTable_train_tt <- subset( ResultMisMatching_train , as.numeric(ResultMisMatching_train[,1]) >= 8 ) 


Target_obj_train_tt   <- TargetTable_train_tt [,2]

missing_obj_train_tt   <- length (Target_obj_train_tt [Target_obj_train_tt  == 999]) 
matching_obj_train_tt  <- length (Target_obj_train_tt [Target_obj_train_tt  != 999]) 


CVeh_train_tt  <- matching_obj_train_tt [1]
Veh_train_tt  <- length(TargetTable_train_tt [,1])

matching_NN_train_tt  <- vector()
missing_NN_train_tt  <- vector()
CMVeh_train_tt  <- vector()
MVeh_train_tt  <- vector()
MMVeh_train_tt  <- vector()
SIMR_train_tt  <- vector()
SCMR_train_tt  <- vector()
SER_train_tt  <- vector()


for (i in 1:12) {
  CMVeh_train_tt[i] <- sum ( as.numeric ((TargetTable_train_tt [,2]) == as.numeric (TargetTable_train_tt [,i+9])) &
                            as.numeric (TargetTable_train_tt [,2]) != 999)
  MVeh_train_tt[i] <- sum(   (as.numeric( TargetTable_train_tt[,i+9])) > 1000 ) 
  
  MMVeh_train_tt[i] <- length(  subset(TargetTable_train_tt[,1], as.numeric( Target_obj_train_tt ) 
                                    !=  as.numeric( TargetTable_train_tt[,i+9])   ))
  
  SIMR_train_tt[i] <- CMVeh_train_tt[i] / CVeh_train_tt[1]
  SCMR_train_tt[i] <- CMVeh_train_tt[i] / MVeh_train_tt[i]
  SER_train_tt[i] <- MMVeh_train_tt[i] / Veh_train_tt[1]
  
  ResultMissing_train_tt <- rbind(  ResultMissing_train_tt  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_train_tt[1], missing_obj_train_tt[1],              
                                                            CMVeh_train_tt[[i]], CVeh_train_tt[[1]], MVeh_train_tt[[i]] ,
                                                            SIMR_train_tt[[i]], SCMR_train_tt[[i]], MMVeh_train_tt[[i]], Veh_train_tt[[1]], SER_train_tt[[i]] ))
  
}

# SU unit


TargetTable_train_su <- subset( ResultMisMatching_train , as.numeric(ResultMisMatching_train[,1]) < 8 ) # CHANGE


Target_obj_train_su   <- TargetTable_train_su [,2]

missing_obj_train_su   <- length (Target_obj_train_su [Target_obj_train_su  == 999]) 
matching_obj_train_su  <- length (Target_obj_train_su [Target_obj_train_su  != 999]) 


CVeh_train_su  <- matching_obj_train_su [1]
Veh_train_su  <- length(TargetTable_train_su [,1])

matching_NN_train_su  <- vector()
missing_NN_train_su  <- vector()
CMVeh_train_su  <- vector()
MVeh_train_su  <- vector()
MMVeh_train_su  <- vector()
SIMR_train_su  <- vector()
SCMR_train_su  <- vector()
SER_train_su  <- vector()


for (i in 1:12) {
  CMVeh_train_su[i] <- sum ( as.numeric ((TargetTable_train_su [,2]) == as.numeric (TargetTable_train_su [,i+9])) &
                               as.numeric (TargetTable_train_su [,2]) != 999)
  MVeh_train_su[i] <- sum(   (as.numeric( TargetTable_train_su[,i+9])) > 1000 ) 
  
  MMVeh_train_su[i] <- length(  subset(TargetTable_train_su[,1], as.numeric( Target_obj_train_su ) 
                                       !=  as.numeric( TargetTable_train_su[,i+9])   ))
  
  SIMR_train_su[i] <- CMVeh_train_su[i] / CVeh_train_su[1]
  SCMR_train_su[i] <- CMVeh_train_su[i] / MVeh_train_su[i]
  SER_train_su[i] <- MMVeh_train_su[i] / Veh_train_su[1]
  
  ResultMissing_train_su <- rbind(  ResultMissing_train_su  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_train_su[1], missing_obj_train_su[1],              
                                                                  CMVeh_train_su[[i]], CVeh_train_su[[1]], MVeh_train_su[[i]] ,
                                                                  SIMR_train_su[[i]], SCMR_train_su[[i]], MMVeh_train_su[[i]], Veh_train_su[[1]], SER_train_su[[i]] ))
  
}


# ALL unit
TargetTable_test_all <- subset( ResultMisMatching_test , as.numeric(ResultMisMatching_test[,1]) >= 4 ) # CHANGE


Target_obj_test_all   <- TargetTable_test_all [,2]

missing_obj_test_all   <- length (Target_obj_test_all [Target_obj_test_all  == 999]) 
matching_obj_test_all  <- length (Target_obj_test_all [Target_obj_test_all  != 999]) 


CVeh_test_all  <- matching_obj_test_all [1]
Veh_test_all  <- length(TargetTable_test_all [,1])

matching_NN_test_all  <- vector()
missing_NN_test_all  <- vector()
CMVeh_test_all  <- vector()
MVeh_test_all  <- vector()
MMVeh_test_all  <- vector()
SIMR_test_all  <- vector()
SCMR_test_all  <- vector()
SER_test_all  <- vector()


for (i in 1:12) {
  CMVeh_test_all[i] <- sum ( as.numeric ((TargetTable_test_all [,2]) == as.numeric (TargetTable_test_all [,i+9])) &
                               as.numeric (TargetTable_test_all [,2]) != 999)
  MVeh_test_all[i] <- sum(   (as.numeric( TargetTable_test_all[,i+9])) > 1000 ) 
  
  MMVeh_test_all[i] <- length(  subset(TargetTable_test_all[,1], as.numeric( Target_obj_test_all ) 
                                       !=  as.numeric( TargetTable_test_all[,i+9])   ))
  
  SIMR_test_all[i] <- CMVeh_test_all[i] / CVeh_test_all[1]
  SCMR_test_all[i] <- CMVeh_test_all[i] / MVeh_test_all[i]
  SER_test_all[i] <- MMVeh_test_all[i] / Veh_test_all[1]
  
  ResultMissing_test_all <- rbind(  ResultMissing_test_all  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_test_all[1], missing_obj_test_all[1],              
                                                                  CMVeh_test_all[[i]], CVeh_test_all[[1]], MVeh_test_all[[i]] ,
                                                                  SIMR_test_all[[i]], SCMR_test_all[[i]], MMVeh_test_all[[i]], Veh_test_all[[1]], SER_test_all[[i]] ))
  
}

# TT unit
TargetTable_test_tt <- subset( ResultMisMatching_test , ResultMisMatching_test[,1] >= 8 ) 


Target_obj_test_tt   <- TargetTable_test_tt [,2]

missing_obj_test_tt   <- length (Target_obj_test_tt [Target_obj_test_tt  == 999]) 
matching_obj_test_tt  <- length (Target_obj_test_tt [Target_obj_test_tt  != 999]) 


CVeh_test_tt  <- matching_obj_test_tt [1]
Veh_test_tt  <- length(TargetTable_test_tt [,1])

matching_NN_test_tt  <- vector()
missing_NN_test_tt  <- vector()
CMVeh_test_tt  <- vector()
MVeh_test_tt  <- vector()
MMVeh_test_tt  <- vector()
SIMR_test_tt  <- vector()
SCMR_test_tt  <- vector()
SER_test_tt  <- vector()


for (i in 1:12) {
  CMVeh_test_tt[i] <- sum ( as.numeric ((TargetTable_test_tt [,2]) == as.numeric (TargetTable_test_tt [,i+9])) &
                              as.numeric (TargetTable_test_tt [,2]) != 999)
  MVeh_test_tt[i] <- sum(   (as.numeric( TargetTable_test_tt[,i+9])) > 1000 ) 
  
  MMVeh_test_tt[i] <- length(  subset(TargetTable_test_tt[,1], as.numeric( Target_obj_test_tt ) 
                                      !=  as.numeric( TargetTable_test_tt[,i+9])   ))
  
  SIMR_test_tt[i] <- CMVeh_test_tt[i] / CVeh_test_tt[1]
  SCMR_test_tt[i] <- CMVeh_test_tt[i] / MVeh_test_tt[i]
  SER_test_tt[i] <- MMVeh_test_tt[i] / Veh_test_tt[1]
  
  ResultMissing_test_tt <- rbind(  ResultMissing_test_tt  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_test_tt[1], missing_obj_test_tt[1],              
                                                                CMVeh_test_tt[[i]], CVeh_test_tt[[1]], MVeh_test_tt[[i]] ,
                                                                SIMR_test_tt[[i]], SCMR_test_tt[[i]], MMVeh_test_tt[[i]], Veh_test_tt[[1]], SER_test_tt[[i]] ))
  
}

# SU unit


TargetTable_test_su <- subset( ResultMisMatching_test , ResultMisMatching_test[,1] < 8 ) # CHANGE


Target_obj_test_su   <- TargetTable_test_su [,2]

missing_obj_test_su   <- length (Target_obj_test_su [Target_obj_test_su  == 999]) 
matching_obj_test_su  <- length (Target_obj_test_su [Target_obj_test_su  != 999]) 


CVeh_test_su  <- matching_obj_test_su [1]
Veh_test_su  <- length(TargetTable_test_su [,1])

matching_NN_test_su  <- vector()
missing_NN_test_su  <- vector()
CMVeh_test_su  <- vector()
MVeh_test_su  <- vector()
MMVeh_test_su  <- vector()
SIMR_test_su  <- vector()
SCMR_test_su  <- vector()
SER_test_su  <- vector()


for (i in 1:12) {
  CMVeh_test_su[i] <- sum ( as.numeric ((TargetTable_test_su [,2]) == as.numeric (TargetTable_test_su [,i+9])) &
                              as.numeric (TargetTable_test_su [,2]) != 999)
  MVeh_test_su[i] <- sum(   (as.numeric( TargetTable_test_su[,i+9])) > 1000 ) 
  
  MMVeh_test_su[i] <- length(  subset(TargetTable_test_su[,1], as.numeric( Target_obj_test_su ) 
                                      !=  as.numeric( TargetTable_test_su[,i+9])   ))
  
  SIMR_test_su[i] <- CMVeh_test_su[i] / CVeh_test_su[1]
  SCMR_test_su[i] <- CMVeh_test_su[i] / MVeh_test_su[i]
  SER_test_su[i] <- MMVeh_test_su[i] / Veh_test_su[1]
  
  ResultMissing_test_su <- rbind(  ResultMissing_test_su  , c ( i,weightwim, weightsig1, weightsig2,matching_obj_test_su[1], missing_obj_test_su[1],              
                                                                CMVeh_test_su[[i]], CVeh_test_su[[1]], MVeh_test_su[[i]] ,
                                                                SIMR_test_su[[i]], SCMR_test_su[[i]], MMVeh_test_su[[i]], Veh_test_su[[1]], SER_test_su[[i]] ))
  
}
}}
#     }}}
# by class
# ResultMissing_train_class <- data.frame()
# ResultMissing_test_class <- data.frame()
# Class <- sort(unique( Downheader_new[,14]))
# 
# # classid = 5
# for (classid in Class) {
#     TargetTable_train_class <- subset( ResultMisMatching_train , ResultMisMatching_train[,1] == classid) 
#     
#     
#     Target_obj_train_class   <- TargetTable_train_class [,2]
#     
#     missing_obj_train_class   <- length (Target_obj_train_class [Target_obj_train_class  == 999]) 
#     matching_obj_train_class  <- length (Target_obj_train_class [Target_obj_train_class  != 999]) 
#     
#     
#     CVeh_train_class  <- matching_obj_train_class [1]
#     Veh_train_class  <- length(TargetTable_train_class [,1])
#     
#     matching_NN_train_class  <- vector()
#     missing_NN_train_class  <- vector()
#     CMVeh_train_class  <- vector()
#     MVeh_train_class  <- vector()
#     MMVeh_train_class  <- vector()
#     SIMR_train_class  <- vector()
#     SCMR_train_class  <- vector()
#     SER_train_class  <- vector()
#     
#     
#     for (i in 1:12) {
#       CMVeh_train_class[i] <- sum ( as.numeric ((TargetTable_train_class [,2]) == as.numeric (TargetTable_train_class [,i+9])) &
#                                       as.numeric (TargetTable_train_class [,2]) != 999)
#       MVeh_train_class[i] <- sum(   (as.numeric( TargetTable_train_class[,i+9])) > 1000 ) 
#       
#       MMVeh_train_class[i] <- length(  subset(TargetTable_train_class[,1], as.numeric( Target_obj_train_class ) 
#                                               !=  as.numeric( TargetTable_train_class[,i+9])   ))
#       
#       SIMR_train_class[i] <- CMVeh_train_class[i] / CVeh_train_class[1]
#       SCMR_train_class[i] <- CMVeh_train_class[i] / MVeh_train_class[i]
#       SER_train_class[i] <- MMVeh_train_class[i] / Veh_train_class[1]
#       
#       ResultMissing_train_class <- rbind(  ResultMissing_train_class  , c ( classid, i,weightwim, weightsig1, weightsig2,matching_obj_train_class[1], missing_obj_train_class[1],              
#                                                                             CMVeh_train_class[[i]], CVeh_train_class[[1]], MVeh_train_class[[i]] ,
#                                                                             SIMR_train_class[[i]], SCMR_train_class[[i]], MMVeh_train_class[[i]], Veh_train_class[[1]], SER_train_class[[i]] ))
#       
#     }
#     
#     
#     TargetTable_test_class <- subset( ResultMisMatching_test , ResultMisMatching_test[,1] == classid) 
#     
#     
#     Target_obj_test_class   <- TargetTable_test_class [,2]
#     
#     missing_obj_test_class   <- length (Target_obj_test_class [Target_obj_test_class  == 999]) 
#     matching_obj_test_class  <- length (Target_obj_test_class [Target_obj_test_class  != 999]) 
#     
#     
#     CVeh_test_class  <- matching_obj_test_class [1]
#     Veh_test_class  <- length(TargetTable_test_class [,1])
#     
#     matching_NN_test_class  <- vector()
#     missing_NN_test_class  <- vector()
#     CMVeh_test_class  <- vector()
#     MVeh_test_class  <- vector()
#     MMVeh_test_class  <- vector()
#     SIMR_test_class  <- vector()
#     SCMR_test_class  <- vector()
#     SER_test_class  <- vector()
#     
#     
#     for (i in 1:12) {
#       CMVeh_test_class[i] <- sum ( as.numeric ((TargetTable_test_class [,2]) == as.numeric (TargetTable_test_class [,i+9])) &
#                                      as.numeric (TargetTable_test_class [,2]) != 999)
#       MVeh_test_class[i] <- sum(   (as.numeric( TargetTable_test_class[,i+9])) > 1000 ) 
#       
#       MMVeh_test_class[i] <- length(  subset(TargetTable_test_class[,1], as.numeric( Target_obj_test_class ) 
#                                              !=  as.numeric( TargetTable_test_class[,i+9])   ))
#       
#       SIMR_test_class[i] <- CMVeh_test_class[i] / CVeh_test_class[1]
#       SCMR_test_class[i] <- CMVeh_test_class[i] / MVeh_test_class[i]
#       SER_test_class[i] <- MMVeh_test_class[i] / Veh_test_class[1]
#       
#       ResultMissing_test_class <- rbind(  ResultMissing_test_class  , c ( classid, i,weightwim, weightsig1, weightsig2,matching_obj_test_class[1], missing_obj_test_class[1],              
#                                                                           CMVeh_test_class[[i]], CVeh_test_class[[1]], MVeh_test_class[[i]] ,
#                                                                           SIMR_test_class[[i]], SCMR_test_class[[i]], MMVeh_test_class[[i]], Veh_test_class[[1]], SER_test_class[[i]] ))
#       
#     }
# }



save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Missing_06232015_variancceonly")
### end
# Test_all <- cbind(TargetTable_test_all[,1],TargetTable_test_all[,2] , TargetTable_test_all[,3] , TargetTable_test_all[,15])
# Train_all <- cbind(TargetTable_train_all[,1],TargetTable_train_all[,2] , TargetTable_train_all[,3] , TargetTable_train_all[,15])
# 
# 
# rm(TestSample)
# TestSample <- cbind(ResultMisMatching_test[,1],ResultMisMatching_test[,2],ResultMisMatching_test[,3],
#                     ResultMisMatching_test[,14] , ResultMisMatching_test[,20] )
# TrainSample <- cbind(ResultMisMatching_train[,1],ResultMisMatching_train[,2],ResultMisMatching_train[,3],
#                     ResultMisMatching_train[,14] , ResultMisMatching_train[,20] )
# 
# View(TrainSample)
# View(ResultMissing_test_all_2)
# setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/")
# write.table(ResultMissing_train_all, "./ResultMissing_train_all.txt", sep="\t",row.names=FALSE)
# write.table(ResultMissing_test_all, "./ResultMissing_test_all.txt", sep="\t",row.names=FALSE)
# write.table(ResultMissing_train_su, "./ResultMissing_train_su.txt", sep="\t",row.names=FALSE)
# write.table(ResultMissing_test_su, "./ResultMissing_test_su.txt", sep="\t",row.names=FALSE)
# write.table(ResultMissing_train_tt, "./ResultMissing_train_tt.txt", sep="\t",row.names=FALSE)
# write.table(ResultMissing_test_tt, "./ResultMissing_test_tt.txt", sep="\t",row.names=FALSE)
