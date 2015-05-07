# find mismatching
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_04272015")
rm(list=ls())

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_02112015")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_04172015")
library(stringr)

thresholdForDif <- 2.5
# buf <- 0.00000001 # at least for version 1
buf <- 0.001 # at least for version 2
bufsig <- 0.000000000001
weight <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5)
sigweight <- sigfeatidx
sigweight[sigweight==2] <-1
sigweight[sigweight==3] <-2


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
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp", "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt")

Downtarget_attributes_all <- cbind(Downheader_new[,13:44],Downheader_new[,7] )


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

Upcandidatesindex_train <- list()
Upcandidates_train <- list()


Attribute_sig_train_temp <- list()
Attribute_sig_nonnormal_train <- list()
Attribute_sig_test_temp <- list()
Attribute_sig_nonnormal_test <- list()

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

  for (j in 1: length(Upcandidatesindex_train[[i]]) ) {  
    
    if ( Upcandidates_attribute_train[[i]][[1]][1] != 999 ) {
    Attribute_difftemp_train[[j]] <-  
     abs (as.numeric( (unlist (Upcandidates_attribute_train[[i]][j,4:33] )  ) ) - 
       as.numeric(Downtarget_attributes_train[i,4:33] )  )    
    
    Attribute_sig_train_temp[[j]] <- sigfeature_train[[i]][j]
    }
    
    else {
      Attribute_difftemp_train[[j]] <- NA     
      Attribute_sig_train_temp[[j]] <- NA
    }
    
    Attribute_difftemp_train[[j]][31] <- a_magdif_train[[i]][[ Upcandidatesindex_train[[i]][j] ]]  
   
   
  }
  
    
  Attribute_diff_nonnormal_train[[length(Attribute_diff_nonnormal_train)+1]] <- Attribute_difftemp_train # list in the list
  Attribute_difftemp_train <- list()
  
  Attribute_sig_nonnormal_train[[length (Attribute_sig_nonnormal_train ) +1]] <-  Attribute_sig_train_temp
  Attribute_sig_train_temp <- list()
  
}





# test


for (i in 1: length(Upsiglist_test)) {  
  

  
  Upcandidatesindex_test[[i]] <- which(a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  Upcandidates_test[[i]] <- subset (Upsiglist_test[[i]], a_magdif_test[[i]] < thresholdForDif * min(a_magdif_test[[i]]) )
  
  # test 
  if ( any (is.na (Upcandidates_test[[i]] ) ) )
    Upcandidates_attribute_test[[i]] <- 999
  
  else {
    Upcandidates_attribute_test[[i]] <- cbind(      
      Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),13:44] , 
      Upheader_new_test[ match( as.numeric(Upcandidates_test[[i]]), as.numeric(Upheader_new_test[,13])),7] 
    )
  }
  
  for (j in 1: length(Upcandidatesindex_test[[i]]) ) {  
    
    if ( Upcandidates_attribute_test[[i]][[1]][1] != 999 ) {
      Attribute_difftemp_test[[j]] <-  
        abs (as.numeric( (unlist (Upcandidates_attribute_test[[i]][j,4:33] )  ) ) - 
               as.numeric(Downtarget_attributes_test[i,4:33] )  )     
      
      Attribute_sig_test_temp[[j]] <- sigfeature_test[[i]][j]
    }
    
    else {
      Attribute_difftemp_test[[j]] <- NA    
      Attribute_sig_test_temp <- NA
    }
    
    Attribute_difftemp_test[[j]][31] <- a_magdif_test[[i]][[ Upcandidatesindex_test[[i]][j] ]]  
    
  }
  
  
  Attribute_diff_nonnormal_test[[length(Attribute_diff_nonnormal_test)+1]] <- Attribute_difftemp_test # list in the list
  Attribute_difftemp_test <- list()
  
  Attribute_sig_nonnormal_test[[ length(Attribute_sig_nonnormal_test) +1]] <- Attribute_sig_test_temp
  Attribute_sig_test_temp <- list()
}




# joint probability
jointprobtemp_train <- data.frame()
jointprobtemp_test <- data.frame()
jointprob_train <- list()
jointprob_test <- list()

idxjointprob_train <- data.frame()
idxjointprob_test <- data.frame()
UpFinalcandidates_train <- data.frame()
UpFinalcandidates_test <- data.frame()


jointprobtemp_train_sig <- data.frame()
jointprobtemp_test_sig <- data.frame()
jointprob_train_sig <- list()
jointprob_test_sig <- list()



jointprobtemp_train_result <- data.frame()
jointprobtemp_test_result <- data.frame()
jointprob_train_result <- list()
jointprob_test_result <- list()

maxtemp <- vector()
mintemp <- vector()

Target_baseanalysis_Jan0910_table_train <- data.frame()
Target_baseanalysis_Jan0910_table_test <- data.frame()

Target_baseanalysis_Jan0910_table_train <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd  )
Target_baseanalysis_Jan0910_table_test  <- subset(Target_baseanalysis_Jan0910_table, 
      as.numeric(str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) < utcbd  )


m <- 0
class9idxprob <- c(1:7, 13:21, 30:31)


Downtarget_attributes_train_Class9 <- subset ( Downtarget_attributes_train, Downtarget_attributes_train [,2] == 9  )



for (i in 1:length(Upcandidates_train)){

  
  if ( as.numeric ( Downtarget_attributes_train [i,2] ) == 9 ) {  # Only Class 9
    
   if ( is.na ( Attribute_diff_nonnormal_train[[i]][[1]][1] )  ) {
     jointprob_train[[length(jointprob_train) +1]] <- 999
     jointprob_train_result[[length(jointprob_train_result) +1]] <- 999
     idxjointprob_train[i,1] <- 999
     idxjointprob_train[i,2] <- 999
     idxjointprob_train[i,3] <- 999
     idxjointprob_train[i,4] <- 999
     idxjointprob_train[i,5] <- 999

     UpFinalcandidates_train[i,1] <- 999
     UpFinalcandidates_train[i,2] <- 999
     UpFinalcandidates_train[i,3] <- 999
     UpFinalcandidates_train[i,4] <- 999
     UpFinalcandidates_train[i,5] <- 999

   }
   
   else {
            
      for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
                           
#           for (m in 1: 31) {
        for (m in 1: 31) {
            
#               # option 1 - non parametric 
#               if ( m %in% class9idxprob) {
#                jointprobtemp_train[j,m] <- as.numeric ( (approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
#                                                                  Attribute_diff_nonnormal_train[[i]][[j]][m]) )$y )
#               }
              
#               another option 1 - non parametric but smoothing
#               if ( m %in% class9idxprob) {
#                 jointprobtemp_train[j,m] <- as.numeric ( (approx( density_smooth_hist_mat_c[[m]]$x , density_smooth_hist_mat_c[[m]]$y,
#                                                                   Attribute_diff_nonnormal_train[[i]][[j]][m]) )$y )
#               }
              
#               option 2 - parametric
              if ( m %in% class9idxprob) {
                jointprobtemp_train[j,m] <- as.numeric ( (approx( diffseq_mat_c[[m]],  
                                            normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                            Attribute_diff_nonnormal_train[[i]][[j]][m]) )$y )
                
                
              }
#               
#               option 3 - histogram
#               if ( m %in% class9idxprob  ) {
#                 
#                 if (  length ( which( histdensity_c[[m]][,1] < Attribute_diff_nonnormal_train[[i]][[j]][m] & 
#                       Attribute_diff_nonnormal_train[[i]][[j]][m] < histdensity_c[[m]][,2])) > 0 )
#                   
#                 {
#                   jointprobtemp_train[j,m] <-
#                     histdensity_c[[m]][ which (histdensity_c[[m]][,1] < Attribute_diff_nonnormal_train[[i]][[j]][m] & 
#                                             Attribute_diff_nonnormal_train[[i]][[j]][m] < histdensity_c[[m]][,2]),3]  
#                 }
#                
# 
#                 else
#                 {
#                   jointprobtemp_train[j,m] <- buf
#                 }
#                                                     
#               }
#               # option 3 end 
#               
#               
              else {
                jointprobtemp_train[j,m] <- 99999
              }
          
          }

        for (n in 1: 50) {
         
            jointprobtemp_train_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                               normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                               Attribute_sig_nonnormal_train[[i]][[j]][[1]][n]) )$y )
            
            
          
        }

          
          jointprobtemp_train[j,31] [is.na(jointprobtemp_train[j,31] )] <- 0.0000000001
                       
          jointprobtemp_train [is.na(jointprobtemp_train )] <- buf 
          jointprobtemp_train [jointprobtemp_train == 0] <- buf 

          jointprobtemp_train_sig [is.na(jointprobtemp_train_sig )] <- bufsig 
          jointprobtemp_train_sig [jointprobtemp_train_sig == 0] <- bufsig

#           jointprobtemp_train[j,31] <- 1 /  Attribute_diff_nonnormal_train[[i]][[j]][31]




          
#           # option 1
#            jointprobtemp_train[j,32] <-  jointprobtemp_train[j,1] * jointprobtemp_train[j,2] * jointprobtemp_train[j,3]  * 
#               jointprobtemp_train[j,6] * jointprobtemp_train[j,7] * 
#               jointprobtemp_train[j,14] *  jointprobtemp_train[j,16] *
#               jointprobtemp_train[j,18] *  jointprobtemp_train[j,20] *
#               jointprobtemp_train[j,30]  *
#               jointprobtemp_train[j,31]  

#                ( 1 /jointprobtemp_train[j,31] * weightSig )


          # option 2
       
        jointprobtemp_train_result[j,1] <-   log10(jointprobtemp_train[j,1]) * weight[1] +  
                                       log10(jointprobtemp_train[j,2]) * weight[2] +
                                       log10(jointprobtemp_train[j,3]) * weight[3] +
                                       log10(jointprobtemp_train[j,4]) * weight[4] +
                                       log10(jointprobtemp_train[j,5]) * weight[5] +
                                       log10(jointprobtemp_train[j,6]) * weight[6] +
                                       log10(jointprobtemp_train[j,7]) * weight[7] +
                                       log10(jointprobtemp_train[j,13]) * weight[13] +
                                       log10(jointprobtemp_train[j,14]) * weight[14] +
                                       log10(jointprobtemp_train[j,15]) * weight[15] +
                                       log10(jointprobtemp_train[j,16]) * weight[16] +
                                       log10(jointprobtemp_train[j,17]) * weight[17] +
                                       log10(jointprobtemp_train[j,18]) * weight[18] +
                                       log10(jointprobtemp_train[j,19]) * weight[19] +
                                       log10(jointprobtemp_train[j,20]) * weight[20] +
                                       log10(jointprobtemp_train[j,21]) * weight[21] +
                                       log10(jointprobtemp_train[j,30]) * weight[30] 

        jointprobtemp_train_result[j,2] <- log10( jointprobtemp_train[j,31]) * weight[31] 
        jointprobtemp_train_result[j,3] <- 0

         for ( n in 1: length(sigweight)){
            jointprobtemp_train_result[j,3] <-  jointprobtemp_train_result[j,3] +
                                    log10(jointprobtemp_train_sig[j,n]) * sigweight[n]
         }
          
  
                                      
          jointprobtemp_train_result[j,4] <-  jointprobtemp_train_result[j,1] + jointprobtemp_train_result[j,2]
          jointprobtemp_train_result[j,5] <-  jointprobtemp_train_result[j,1] + jointprobtemp_train_result[j,3]

         
      }
# normalization
for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
        maxtemp[1] <- max( jointprobtemp_train_result[,1] )
        mintemp[1] <- min( jointprobtemp_train_result[,1] )
        jointprobtemp_train_result[j,6] <- ( jointprobtemp_train_result[j,1] - max( jointprobtemp_train_result[,1] ) ) / 
                                          ( max( jointprobtemp_train_result[,1] ) - min( jointprobtemp_train_result[,1] +1) )
        jointprobtemp_train_result[j,7] <- ( jointprobtemp_train_result[j,2] - max( jointprobtemp_train_result[,2] ) ) / 
                                          ( max( jointprobtemp_train_result[,2] ) - min( jointprobtemp_train_result[,2] +1) )
        jointprobtemp_train_result[j,8] <- ( jointprobtemp_train_result[j,3] - max( jointprobtemp_train_result[,3] ) ) / 
                                          ( max( jointprobtemp_train_result[,3] ) - min( jointprobtemp_train_result[,3] +1) )
        jointprobtemp_train_result[j,9] <- ( jointprobtemp_train_result[j,4] - max( jointprobtemp_train_result[,4] ) ) / 
                                          ( max( jointprobtemp_train_result[,4] ) - min( jointprobtemp_train_result[,4] +1) )
        jointprobtemp_train_result[j,10] <- ( jointprobtemp_train_result[j,5] - max( jointprobtemp_train_result[,5] ) ) / 
                                          ( max( jointprobtemp_train_result[,5] ) - min( jointprobtemp_train_result[,5] +1) )
     
        jointprobtemp_train_result[j,11] <-  Upcandidates_train[[i]][j]   
}


                                                                                                                       
    jointprob_train [[length(jointprob_train) + 1]] <- jointprobtemp_train
    jointprob_train_sig [[length(jointprob_train_sig) + 1 ]] <- jointprobtemp_train_sig
    jointprob_train_result[[length(jointprob_train_result)+1]] <-  jointprobtemp_train_result

    jointprobtemp_train_result <- data.frame()
    jointprobtemp_train <- data.frame()
    jointprobtemp_train_sig <- data.frame()
   
    idxjointprob_train[i,1] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][6] ) )
    idxjointprob_train[i,2] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][7] ) )
    idxjointprob_train[i,3] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][8] ) )
    idxjointprob_train[i,4] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][9] ) )  
    idxjointprob_train[i,5] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][10] ) ) 

    UpFinalcandidates_train[i,1] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,1]] ]  
    UpFinalcandidates_train[i,2] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,2]] ]  
    UpFinalcandidates_train[i,3] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,3]] ]  
    UpFinalcandidates_train[i,4] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,4]] ]  
    UpFinalcandidates_train[i,5] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,5]] ] 

   } 
  }
    
    else # class is not 9
      
    {
      jointprob_train_result[[length(jointprob_train_result)+1]] <-  NA 
      jointprob_train[[length(jointprob_train)+1]] <-  NA  
      idxjointprob_train[i,1] <- NA
      idxjointprob_train[i,2] <- NA
      idxjointprob_train[i,3] <- NA
      idxjointprob_train[i,4] <- NA
      idxjointprob_train[i,5] <- NA

      UpFinalcandidates_train[i,1] <- NA
      UpFinalcandidates_train[i,2] <- NA
      UpFinalcandidates_train[i,3] <- NA
      UpFinalcandidates_train[i,4] <- NA
      UpFinalcandidates_train[i,5] <- NA

    }
}





# test


Downtarget_attributes_test_Class9 <- subset ( Downtarget_attributes_test, Downtarget_attributes_test [,2] == 9  )



for (i in 1:length(Upcandidates_test)){
  
  
  if ( as.numeric ( Downtarget_attributes_test [i,2] ) == 9 ) {  # Only Class 9
    
    if ( is.na ( Attribute_diff_nonnormal_test[[i]][[1]][1] )  ) {
      jointprob_test[[length(jointprob_test) +1]] <- 999
      jointprob_test_result[[length(jointprob_test_result) +1]] <- 999
      idxjointprob_test[i,1] <- 999
      idxjointprob_test[i,2] <- 999
      idxjointprob_test[i,3] <- 999
      idxjointprob_test[i,4] <- 999
      idxjointprob_test[i,5] <- 999
      
      UpFinalcandidates_test[i,1] <- 999
      UpFinalcandidates_test[i,2] <- 999
      UpFinalcandidates_test[i,3] <- 999
      UpFinalcandidates_test[i,4] <- 999
      UpFinalcandidates_test[i,5] <- 999
      
    }
    
    else {
      
      for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
        
        #           for (m in 1: 31) {
        for (m in 1: 31) {
          
          #               # option 1 - non parametric 
          #               if ( m %in% class9idxprob) {
          #                jointprobtemp_test[j,m] <- as.numeric ( (approx( kernel_mat_c[[m]]$x, kernel_mat_c[[m]]$y,
          #                                                                  Attribute_diff_nonnormal_test[[i]][[j]][m]) )$y )
          #               }
          
          #               another option 1 - non parametric but smoothing
          #               if ( m %in% class9idxprob) {
          #                 jointprobtemp_test[j,m] <- as.numeric ( (approx( density_smooth_hist_mat_c[[m]]$x , density_smooth_hist_mat_c[[m]]$y,
          #                                                                   Attribute_diff_nonnormal_test[[i]][[j]][m]) )$y )
          #               }
          
          #               option 2 - parametric
          if ( m %in% class9idxprob) {
            jointprobtemp_test[j,m] <- as.numeric ( (approx( diffseq_mat_c[[m]],  
                                                             normal_mat_c[[m]]  * multiplier_hist_mat_c[[m]][ which.min(is.na( multiplier_hist_mat_c[[m]] ) ) ] ,
                                                             Attribute_diff_nonnormal_test[[i]][[j]][m]) )$y )
            
            
          }
          #               
          #               option 3 - histogram
          #               if ( m %in% class9idxprob  ) {
          #                 
          #                 if (  length ( which( histdensity_c[[m]][,1] < Attribute_diff_nonnormal_test[[i]][[j]][m] & 
          #                       Attribute_diff_nonnormal_test[[i]][[j]][m] < histdensity_c[[m]][,2])) > 0 )
          #                   
          #                 {
          #                   jointprobtemp_test[j,m] <-
          #                     histdensity_c[[m]][ which (histdensity_c[[m]][,1] < Attribute_diff_nonnormal_test[[i]][[j]][m] & 
          #                                             Attribute_diff_nonnormal_test[[i]][[j]][m] < histdensity_c[[m]][,2]),3]  
          #                 }
          #                
          # 
          #                 else
          #                 {
          #                   jointprobtemp_test[j,m] <- buf
          #                 }
          #                                                     
          #               }
          #               # option 3 end 
          #               
          #               
          else {
            jointprobtemp_test[j,m] <- 99999
          }
          
        }
        
        for (n in 1: 50) {
          
          jointprobtemp_test_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig[[n]],  
                                                               normal_mat_c_sig[[n]]  *  multiplier_hist_mat_c_sig[[n]][ which.min(is.na( multiplier_hist_mat_c_sig[[n]] ) ) ] ,
                                                               Attribute_sig_nonnormal_test[[i]][[j]][[1]][n]) )$y )
          
          
          
        }
        
        
        jointprobtemp_test[j,31] [is.na(jointprobtemp_test[j,31] )] <- 0.0000000001
        
        jointprobtemp_test [is.na(jointprobtemp_test )] <- buf 
        jointprobtemp_test [jointprobtemp_test == 0] <- buf 
        
        jointprobtemp_test_sig [is.na(jointprobtemp_test_sig )] <- bufsig 
        jointprobtemp_test_sig [jointprobtemp_test_sig == 0] <- bufsig
        
        #           jointprobtemp_test[j,31] <- 1 /  Attribute_diff_nonnormal_test[[i]][[j]][31]
        
        
        
        
        
        #           # option 1
        #            jointprobtemp_test[j,32] <-  jointprobtemp_test[j,1] * jointprobtemp_test[j,2] * jointprobtemp_test[j,3]  * 
        #               jointprobtemp_test[j,6] * jointprobtemp_test[j,7] * 
        #               jointprobtemp_test[j,14] *  jointprobtemp_test[j,16] *
        #               jointprobtemp_test[j,18] *  jointprobtemp_test[j,20] *
        #               jointprobtemp_test[j,30]  *
        #               jointprobtemp_test[j,31]  
        
        #                ( 1 /jointprobtemp_test[j,31] * weightSig )
        
        
        # option 2
        
        jointprobtemp_test_result[j,1] <-   log10(jointprobtemp_test[j,1]) * weight[1] +  
          log10(jointprobtemp_test[j,2]) * weight[2] +
          log10(jointprobtemp_test[j,3]) * weight[3] +
          log10(jointprobtemp_test[j,4]) * weight[4] +
          log10(jointprobtemp_test[j,5]) * weight[5] +
          log10(jointprobtemp_test[j,6]) * weight[6] +
          log10(jointprobtemp_test[j,7]) * weight[7] +
          log10(jointprobtemp_test[j,13]) * weight[13] +
          log10(jointprobtemp_test[j,14]) * weight[14] +
          log10(jointprobtemp_test[j,15]) * weight[15] +
          log10(jointprobtemp_test[j,16]) * weight[16] +
          log10(jointprobtemp_test[j,17]) * weight[17] +
          log10(jointprobtemp_test[j,18]) * weight[18] +
          log10(jointprobtemp_test[j,19]) * weight[19] +
          log10(jointprobtemp_test[j,20]) * weight[20] +
          log10(jointprobtemp_test[j,21]) * weight[21] +
          log10(jointprobtemp_test[j,30]) * weight[30] 
        
        jointprobtemp_test_result[j,2] <- log10( jointprobtemp_test[j,31]) * weight[31] 
        jointprobtemp_test_result[j,3] <- 0
        
        for ( n in 1: length(sigweight)){
          jointprobtemp_test_result[j,3] <-  jointprobtemp_test_result[j,3] +
            log10(jointprobtemp_test_sig[j,n]) * sigweight[n]
        }
        
        
        
        jointprobtemp_test_result[j,4] <-  jointprobtemp_test_result[j,1] + jointprobtemp_test_result[j,2]
        jointprobtemp_test_result[j,5] <-  jointprobtemp_test_result[j,1] + jointprobtemp_test_result[j,3]
        
        
      }
      # normalization
      for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
        maxtemp[1] <- max( jointprobtemp_test_result[,1] )
        mintemp[1] <- min( jointprobtemp_test_result[,1] )
        jointprobtemp_test_result[j,6] <- ( jointprobtemp_test_result[j,1] - max( jointprobtemp_test_result[,1] ) ) / 
          ( max( jointprobtemp_test_result[,1] ) - min( jointprobtemp_test_result[,1] +1) )
        jointprobtemp_test_result[j,7] <- ( jointprobtemp_test_result[j,2] - max( jointprobtemp_test_result[,2] ) ) / 
          ( max( jointprobtemp_test_result[,2] ) - min( jointprobtemp_test_result[,2] +1) )
        jointprobtemp_test_result[j,8] <- ( jointprobtemp_test_result[j,3] - max( jointprobtemp_test_result[,3] ) ) / 
          ( max( jointprobtemp_test_result[,3] ) - min( jointprobtemp_test_result[,3] +1) )
        jointprobtemp_test_result[j,9] <- ( jointprobtemp_test_result[j,4] - max( jointprobtemp_test_result[,4] ) ) / 
          ( max( jointprobtemp_test_result[,4] ) - min( jointprobtemp_test_result[,4] +1) )
        jointprobtemp_test_result[j,10] <- ( jointprobtemp_test_result[j,5] - max( jointprobtemp_test_result[,5] ) ) / 
          ( max( jointprobtemp_test_result[,5] ) - min( jointprobtemp_test_result[,5] +1) )
        
        jointprobtemp_test_result[j,11] <-  Upcandidates_test[[i]][j]   
      }
      
      
      
      jointprob_test [[length(jointprob_test) + 1]] <- jointprobtemp_test
      jointprob_test_sig [[length(jointprob_test_sig) + 1 ]] <- jointprobtemp_test_sig
      jointprob_test_result[[length(jointprob_test_result)+1]] <-  jointprobtemp_test_result
      
      jointprobtemp_test_result <- data.frame()
      jointprobtemp_test <- data.frame()
      jointprobtemp_test_sig <- data.frame()
      
      idxjointprob_test[i,1] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][6] ) )
      idxjointprob_test[i,2] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][7] ) )
      idxjointprob_test[i,3] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][8] ) )
      idxjointprob_test[i,4] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][9] ) )  
      idxjointprob_test[i,5] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][10] ) ) 
      
      UpFinalcandidates_test[i,1] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,1]] ]  
      UpFinalcandidates_test[i,2] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,2]] ]  
      UpFinalcandidates_test[i,3] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,3]] ]  
      UpFinalcandidates_test[i,4] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,4]] ]  
      UpFinalcandidates_test[i,5] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,5]] ] 
      
    } 
  }
  
  else # class is not 9
    
  {
    jointprob_test_result[[length(jointprob_test_result)+1]] <-  NA 
    jointprob_test[[length(jointprob_test)+1]] <-  NA  
    idxjointprob_test[i,1] <- NA
    idxjointprob_test[i,2] <- NA
    idxjointprob_test[i,3] <- NA
    idxjointprob_test[i,4] <- NA
    idxjointprob_test[i,5] <- NA
    
    UpFinalcandidates_test[i,1] <- NA
    UpFinalcandidates_test[i,2] <- NA
    UpFinalcandidates_test[i,3] <- NA
    UpFinalcandidates_test[i,4] <- NA
    UpFinalcandidates_test[i,5] <- NA
    
  }
}


### performance 
#index for class 9
idxForClass9 <- which(Downheader_new[,14] == 9)
 
rm(ResultMisMatching_train, ResultMisMatching_test)




ResultMisMatching_train <- cbind(Target_baseanalysis_Jan0910_table_train[,1], Target_baseanalysis_Jan0910_table_train[,6],
                                 Target_baseanalysis_Jan0910_table_train[,4], 
                                 UpFinalcandidates_train[,1] ,  UpFinalcandidates_train[,2] ,
                                 UpFinalcandidates_train[,3] ,  UpFinalcandidates_train[,4] ,  UpFinalcandidates_train[,5])

ResultMisMatching_test <- cbind(Target_baseanalysis_Jan0910_table_test[,1],Target_baseanalysis_Jan0910_table_test[,6],
                                Target_baseanalysis_Jan0910_table_test[,4], 
                                UpFinalcandidates_test[,1] ,  UpFinalcandidates_test[,2] , 
                                UpFinalcandidates_test[,3] ,  UpFinalcandidates_test[,4] , UpFinalcandidates_test[,5] )

ResultMisMatching_train[is.na ( ResultMisMatching_train)]  <- c(999)
ResultMisMatching_test[is.na ( ResultMisMatching_test)]  <- c(999)

ResultMisMatching_train_class9 <- subset( ResultMisMatching_train , ResultMisMatching_train[,1] == 9 )
ResultMisMatching_test_class9 <- subset( ResultMisMatching_test , ResultMisMatching_test[,1] == 9 )


# train

TargetTable_train <- ResultMisMatching_train_class9 
Target_obj_train  <- ResultMisMatching_train_class9 [,2]

missing_obj_train  <- length (Target_obj_train[Target_obj_train == 999]) 
matching_obj_train <- length (Target_obj_train[Target_obj_train != 999]) 

missing_NN_train <- sum ( as.numeric (ResultMisMatching_train[,2]) == c(999))
CVeh_train[1] <- matching_obj_train[1]
Veh_train <- length(TargetTable_train[,1])

for (i in 1: 5) {
  matching_NN_train[i] <- sum ( as.numeric ((ResultMisMatching_train [,2]) == as.numeric (ResultMisMatching_train [,i+3])) &
                               as.numeric (ResultMisMatching_train [,2]) != 999)
  MVeh_train[i] <- sum(   (as.numeric( TargetTable_train[,i+3])) > 1000 ) 
  
  
  CMVeh_train[i] <-  matching_NN_train[i]
 
  MMVeh_train[i] <- length(  subset(TargetTable_train[,1], as.numeric( Target_obj_train ) 
                                 !=  as.numeric( TargetTable_train[,i+3])   ))  
  SIMR_train[i] <- CMVeh_train[i] / CVeh_train[1]
  SCMR_train[i] <- CMVeh_train[i] / MVeh_train[i]
  SER_train[i] <- MMVeh_train[i] / Veh_train
  
  ResultMismatching_train[i,] <- data.frame( matching_obj_train[1], missing_obj_train[1],              
     matching_NN_train[[i]],  missing_NN_train[[1]],
     CMVeh_train[[i]], CVeh_train[[1]], MVeh_train[[i]],
     SIMR_train[[i]], SCMR_train[[i]], MMVeh_train[[i]], Veh_train[[1]], SER_train[[i]] )

}




# test

TargetTable_test <- ResultMisMatching_test_class9 
Target_obj_test  <- ResultMisMatching_test_class9 [,2]

missing_obj_test  <- length (Target_obj_test[Target_obj_test == 999]) 
matching_obj_test <- length (Target_obj_test[Target_obj_test != 999]) 

missing_NN_test <- sum ( as.numeric (ResultMisMatching_test[,2]) == c(999))
CVeh_test[1] <- matching_obj_test[1]
Veh_test <- length(TargetTable_test[,1])

for (i in 1: 5) {
  matching_NN_test[i] <- sum ( as.numeric ((ResultMisMatching_test [,2]) == as.numeric (ResultMisMatching_test [,i+3])) &
                                 as.numeric (ResultMisMatching_test [,2]) != 999)
  MVeh_test[i] <- sum(   (as.numeric( TargetTable_test[,i+3])) > 1000 ) 
  
  
  CMVeh_test[i] <-  matching_NN_test[i]
  
  MMVeh_test[i] <- length(  subset(TargetTable_test[,1], as.numeric( Target_obj_test ) 
                                   !=  as.numeric( TargetTable_test[,i+3])   ))  
  SIMR_test[i] <- CMVeh_test[i] / CVeh_test[1]
  SCMR_test[i] <- CMVeh_test[i] / MVeh_test[i]
  SER_test[i] <- MMVeh_test[i] / Veh_test
  
  ResultMismatching_test[i,] <- data.frame( matching_obj_test[1], missing_obj_test[1],              
                                            matching_NN_test[[i]],  missing_NN_test[[1]],
                                            CMVeh_test[[i]], CVeh_test[[1]], MVeh_test[[i]],
                                            SIMR_test[[i]], SCMR_test[[i]], MMVeh_test[[i]], Veh_test[[1]], SER_test[[i]] )
  
}

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_05062015")



Upsiglist_train [[ which(Downtarget_attributes_train[,1] ==531357854438662 )  ]]
jointprob_train[[  which(Downtarget_attributes_train[,1] ==531357854438662 )   ]]
# # look more closely
# Upsiglist[[ idxForClass9[[16]]  ]]
# a_magdif [[ idxForClass9[[16]]  ]]
jointprob_test [[ idxForClass9[[12]]  ]]
# jointprob_train[[16]]
# ResultMismatching_test
# ResultMismatching_train


## duplicate module


                                 
duplicated(TargetTable_test[,4])
jointprob_test[[9]]
               

## end
i=94
comp <- matrix(0,3,33)
comp <- Downtarget_attributes_test[match( ResultMisMatching_train[i,3] , Downtarget_attributes_train[,1] ),]
# aa <- Downheader_new[match ( ResultMisMatching_train[i,3] , Downheader_new[,13]) , ]
# temp <- Upheader_new[match ( ResultMisMatching_train[i,2] , Upheader_new[,13]) , ]
temp <- Upheader_new[match ( ResultMisMatching_train[i,2] , Upheader_new[,13]) , 13:44]
temp <- cbind(temp, Upheader_new_train[match ( ResultMisMatching_train[i,2] , Upheader_new[,13]) , 7] )
names(temp) <- names(comp)
comp <- rbind(comp, temp)
comp <- rbind(comp, as.numeric (comp[1,]) -as.numeric( comp[2,]) )
View(comp)


comp2 <- matrix(0,3,33)
comp2 <- Downtarget_attributes_train[match( ResultMisMatching_train[i,3] , Downtarget_attributes_train[,1] ),]
# aa <- Downheader_new[match ( ResultMisMatching_train[i,3] , Downheader_new[,13]) , ]
# temp <- Upheader_new[match ( ResultMisMatching_train[i,2] , Upheader_new[,13]) , ]
temp <- Upheader_new[match ( ResultMisMatching_train[i,4] , Upheader_new[,13]) , 13:44]
temp <- cbind(temp, Upheader_new_train[match ( ResultMisMatching_train[i,2] , Upheader_new[,13]) , 7] )
names(temp) <- names(comp2)
comp2 <- rbind(comp2, temp)
comp2 <- rbind(comp2, as.numeric (comp2[1,]) -as.numeric( comp2[2,]) )
View(comp2)


a_magdif_train[[i]]
Upsiglist_train[[i]]
jointprob_train[[i]]


time <- seq(from= 0, to= 1, by = 1/(num-1))
insig_time <- time

sigidDown=ResultMisMatching_train[i,3] 
Down <- data.frame(time, Downobjout[match(sigidDown, Downheader_new$sigid),] )
sigplot <- plot(Down[,1], Down[,2], main=paste("Candidate (Downstream)", sigidDown[1]))

sigidUp_est= ResultMisMatching_train[i,4]
Up_est <- data.frame(time, Upobjout[match(sigidUp_est, Upheader_new$sigid),] )
sigplot <- plot(Up_est[,1], Up_est[,2], main=paste("Target (Upstream)", sigidUp_est[1]))

sigidUp_act= ResultMisMatching_train[i,2]
Up_act<- data.frame(time, Upobjout[match(sigidUp_act, Upheader_new$sigid),] )
sigplot <- plot(Up_act[,1], Up_act[,2], main=paste("Target (Upstream)", sigidUp_act[1]))

ggplot()+
  geom_line(data=Down, aes ( x=Down[,1] , y=Down[,2]  ) , color='green' ) +
  geom_line(data=Up_est , aes ( x=Up_est[,1] , y=Up_est [,2]  ) , color='red') +
  geom_line(data=Up_act , aes ( x=Up_act[,1] , y=Up_act [,2]  ) , color='blue') 



# test

i=75
comp <- matrix(0,3,33)
comp <- Downtarget_attributes_test[match( ResultMisMatching_test[i,3] , Downtarget_attributes_test[,1] ),]
# aa <- Downheader_new[match ( ResultMisMatching_test[i,3] , Downheader_new[,13]) , ]
# temp <- Upheader_new[match ( ResultMisMatching_test[i,2] , Upheader_new[,13]) , ]
temp <- Upheader_new[match ( ResultMisMatching_test[i,2] , Upheader_new[,13]) , 13:44]
temp <- cbind(temp, Upheader_new_test[match ( ResultMisMatching_test[i,2] , Upheader_new[,13]) , 7] )
names(temp) <- names(comp)
comp <- rbind(comp, temp)
comp <- rbind(comp, as.numeric (comp[1,]) -as.numeric( comp[2,]) )
View(comp)


comp2 <- matrix(0,3,33)
comp2 <- Downtarget_attributes_test[match( ResultMisMatching_test[i,3] , Downtarget_attributes_test[,1] ),]
# aa <- Downheader_new[match ( ResultMisMatching_test[i,3] , Downheader_new[,13]) , ]
# temp <- Upheader_new[match ( ResultMisMatching_test[i,2] , Upheader_new[,13]) , ]
temp <- Upheader_new[match ( ResultMisMatching_test[i,4] , Upheader_new[,13]) , 13:44]
temp <- cbind(temp, Upheader_new_test[match ( ResultMisMatching_test[i,2] , Upheader_new[,13]) , 7] )
names(temp) <- names(comp2)
comp2 <- rbind(comp2, temp)
comp2 <- rbind(comp2, as.numeric (comp2[1,]) -as.numeric( comp2[2,]) )
View(comp2)


a_magdif_test[[i]]
Upsiglist_test[[i]]
jointprob_test[[i]]


time <- seq(from= 0, to= 1, by = 1/(num-1))
insig_time <- time

sigidDown=ResultMisMatching_test[i,3] 
Down <- data.frame(time, Downobjout[match(sigidDown, Downheader_new$sigid),] )
sigplot <- plot(Down[,1], Down[,2], main=paste("Candidate (Downstream)", sigidDown[1]))

sigidUp_est= ResultMisMatching_test[i,4]
Up_est <- data.frame(time, Upobjout[match(sigidUp_est, Upheader_new$sigid),] )
sigplot <- plot(Up_est[,1], Up_est[,2], main=paste("Target (Upstream)", sigidUp_est[1]))

sigidUp_act= ResultMisMatching_test[i,2]
Up_act<- data.frame(time, Upobjout[match(sigidUp_act, Upheader_new$sigid),] )
sigplot <- plot(Up_act[,1], Up_act[,2], main=paste("Target (Upstream)", sigidUp_act[1]))

ggplot()+
  geom_line(data=Down, aes ( x=Down[,1] , y=Down[,2]  ) , color='green' ) +
  geom_line(data=Up_est , aes ( x=Up_est[,1] , y=Up_est [,2]  ) , color='red') +
  geom_line(data=Up_act , aes ( x=Up_act[,1] , y=Up_act [,2]  ) , color='blue') 

