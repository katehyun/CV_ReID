rm(list=ls())
options(scipen=999) 

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_tt.RData")


load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_su.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_n_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_n_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_n_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_n_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_n_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_n_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_n_su.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_sig_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_sig_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_sig_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_sig_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_sig_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_sig_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_sig_su.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_mat_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_mat_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_mat_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_mat_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_nonmat_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_nonmat_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_nonmat_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_nonmat_tt.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Downobjout_set1v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_magdif_set1v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_basemagdif_set1v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Upheader_new_set1v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Downheader_new_set1v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/candidate_set1v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Upsiglist_set1v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/FHWAClass_set1v2.RData" )
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/sigfeature_set1v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Target_baseanalysis_table_set1v2.RData")


load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Downobjout_set2v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_magdif_set2v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_basemagdif_set2v2.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Upheader_new_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Downheader_new_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/candidate_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Upsiglist_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/FHWAClass_set2v2.RData" )
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/sigfeature_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Target_baseanalysis_table_set2v2.RData")

# feature weight
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


############################ start here ############################
weightwim  <- 1
weightsig1 <- 0
weightsig2 <- 1 

thresholdForDif <- 2.5
buf <- 0.001 
bufsig <- 0.000000000001
# utcbd <- 1357804800000
sigfeatlen <- 50
wimfeatlen <- 31

## Extract attributes 
Upcandidates<- list()

Downtarget_attributes_all <- data.frame()
Upcandidatesindex <- list()

Attribute_difftemp <- list()
Attribute_diff_nonnormal <- list()

Upcandidatesindex <- list()
Upcandidates <- list()

Attribute_sign_temp <- list()
Attribute_sig_nonnormal <- list()
Attribute_sig_temp <- list()
Attribute_sig_nonnormal <- list()


Upcandidates_attribute <- list()
Downtarget_attributes <- data.frame()
Downtarget_attributes <- cbind(Downheader_new[,9] , Downheader_new[,12],Downheader_new[,42],Downheader_new[,16:41],Downheader_new[,5] )



for (i in 1: length(Upsiglist)) {  
# for (i in 1: 50) {  
  
  Upcandidatesindex[[i]] <- which(a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )
  Upcandidates[[i]] <- subset (Upsiglist[[i]], a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )
  

  if ( any (is.na (Upcandidates[[i]] ) ) )
    Upcandidates_attribute[[i]] <- 999 
  else {
    Upcandidates_attribute[[i]] <- cbind(    
      Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new$sig_id)),9] ,
      Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new$sig_id)),12] ,
      Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new$sig_id)),42] ,
      Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new$sig_id)),16:41] , 
      Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new$sig_id)),5] 
    )
  }
  
  for (j in 1: length(Upcandidatesindex[[i]]) ) {  
    
    if ( Upcandidates_attribute[[i]][[1]][1] != 999 ) {
      Attribute_difftemp[[j]] <-  
        abs (as.numeric( (unlist (Upcandidates_attribute[[i]][j,1:30] )  ) ) - as.numeric(Downtarget_attributes[i,1:30] )  )    
      
      Attribute_sig_temp[[j]] <- sigfeature[[i]][ Upcandidatesindex[[i]][j]  ]
    }
    
    else {
      Attribute_difftemp[[j]] <- NA     
      Attribute_sig_temp[[j]] <- NA
    }
    
    Attribute_difftemp[[j]][31] <- a_magdif[[i]][[ Upcandidatesindex[[i]][j] ]]  
 
  }
  
  Attribute_diff_nonnormal[[length(Attribute_diff_nonnormal)+1]] <- Attribute_difftemp # list in the list
  Attribute_difftemp_train <- list()
  
  Attribute_sig_nonnormal[[length (Attribute_sig_nonnormal ) +1]] <-  Attribute_sig_temp
  Attribute_sig_temp <- list()
  
}




# joint probability
jointprobtemp <- data.frame()
jointprob <- list()

idxjointprob <- data.frame()
UpFinalcandidates <- data.frame()

jointprobtemp_sig <- data.frame()
jointprob_sig <- list()

jointprobtemp_result <- data.frame()
jointprob_result <- list()

maxtemp <- vector()
mintemp <- vector()



classallidxprob <- c(1:7, 12:21, 30:31)

# Downtarget_attributes_tt <- subset ( Downtarget_attributes, Downtarget_attributes$FHWAclass >= 8  )
# Downtarget_attributes_su <- subset ( Downtarget_attributes, Downtarget_attributes$FHWAclass < 8  )

# 
# weightwim <- 1
# weightsig1 <- 0
# weightsig2 <- 2

ResultMismatching_all <- data.frame()
ResultMismatching_tt <- data.frame()
ResultMismatching_su <- data.frame()

for (i in 1:length(Upsiglist)){
# for (i in 1:50){
  
  if ( as.numeric ( Downheader_new$FHWAclass [i] ) >= 8 ) {  # tt
    

    
    if ( is.na ( Attribute_diff_nonnormal[[i]][[1]][1] )  ) {
      jointprob[[length(jointprob) +1]] <- 999
      jointprob_sig[[length(jointprob_sig) +1]] <- 999
      jointprob_result[[length(jointprob_result) +1]] <- 999
      
      idxjointprob[i,1] <- 999
      idxjointprob[i,2] <- 999
      idxjointprob[i,3] <- 999
      idxjointprob[i,4] <- 999
      idxjointprob[i,5] <- 999
      idxjointprob[i,6] <- 999
      
      
      UpFinalcandidates[i,1] <- 999
      UpFinalcandidates[i,2] <- 999
      UpFinalcandidates[i,3] <- 999
      UpFinalcandidates[i,4] <- 999
      UpFinalcandidates[i,5] <- 999
      UpFinalcandidates[i,6] <- 999
      
      
    }
    
    else {
      
      for (j in 1: length(  Upcandidatesindex[[i]]  )  ) {
        
        for (m in 1: wimfeatlen) {


          if ( m %in% classallidxprob) {
            jointprobtemp[j,m] <- as.numeric ( (approx( diffseq_mat_c_tt[[m]],  
                               normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
                              Attribute_diff_nonnormal[[i]][[j]][m]) )$y )     
          }
           
          else {
            jointprobtemp[j,m] <- 99999
          }
          
        }
        
        for (n in 1: sigfeatlen) {
          
          jointprobtemp_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                Attribute_sig_nonnormal[[i]][[j]][[1]][n]) )$y )              
        }
        
        
        jointprobtemp[j,31] [is.na(jointprobtemp[j,31] )] <- bufsig
        
        jointprobtemp[is.na(jointprobtemp)] <- buf 
        jointprobtemp[jointprobtemp == 0] <- buf 
        
        jointprobtemp_sig [is.na(jointprobtemp_sig )] <- bufsig 
        jointprobtemp_sig [jointprobtemp_sig == 0] <- bufsig
        
        
        jointprobtemp_result[j,1]  <-   log10(jointprobtemp[j,1]) * wimweight_tt[1] +  
                                        log10(jointprobtemp[j,2]) * wimweight_tt[2] +
                                        log10(jointprobtemp[j,3]) * wimweight_tt[3] +
                                        log10(jointprobtemp[j,4]) * wimweight_tt[4] +
                                        log10(jointprobtemp[j,5]) * wimweight_tt[5] +
                                        log10(jointprobtemp[j,6]) * wimweight_tt[6] +
                                        log10(jointprobtemp[j,7]) * wimweight_tt[7] +
                                        log10(jointprobtemp[j,13]) * wimweight_tt[13] +
                                        log10(jointprobtemp[j,14]) * wimweight_tt[14] +
                                        log10(jointprobtemp[j,15]) * wimweight_tt[15] +
                                        log10(jointprobtemp[j,16]) * wimweight_tt[16] +
                                        log10(jointprobtemp[j,17]) * wimweight_tt[17] +
                                        log10(jointprobtemp[j,18]) * wimweight_tt[18] +
                                        log10(jointprobtemp[j,19]) * wimweight_tt[19] +
                                        log10(jointprobtemp[j,20]) * wimweight_tt[20] +
                                        log10(jointprobtemp[j,21]) * wimweight_tt[21] +
                                        log10(jointprobtemp[j,30]) * wimweight_tt[30] 
        
        jointprobtemp_result[j,2] <- log10( jointprobtemp[j,31]) * wimweight_tt[31] 
        jointprobtemp_result[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprobtemp_result[j,3] <-  jointprobtemp_result[j,3] + log10(jointprobtemp_sig[j,n]) * sigweight_tt[n]
        }
        
        jointprobtemp_result[j,4] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,2] 
        jointprobtemp_result[j,5] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,3] 
        jointprobtemp_result[j,6] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,2]  + jointprobtemp_result[j,3] 
        
      }
      
      # normalization
      for (j in 1: length(  Upcandidatesindex[[i]]  )  ) {
        
        maxtemp[1] <- max( jointprobtemp_result[,1] )
        mintemp[1] <- min( jointprobtemp_result[,1] )
        
        jointprobtemp_result[j,7] <- ( jointprobtemp_result[j,1] - max( jointprobtemp_result[,1] ) ) / 
                                     ( max( jointprobtemp_result[,1] ) - min( jointprobtemp_result[,1] +1) )
        jointprobtemp_result[j,8] <- ( jointprobtemp_result[j,2] - max( jointprobtemp_result[,2] ) ) / 
                                     ( max( jointprobtemp_result[,2] ) - min( jointprobtemp_result[,2] +1) )
        jointprobtemp_result[j,9] <- ( jointprobtemp_result[j,3] - max( jointprobtemp_result[,3] ) ) / 
                                     ( max( jointprobtemp_result[,3] ) - min( jointprobtemp_result[,3] +1) )
        jointprobtemp_result[j,10] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,8] * weightsig1 
        jointprobtemp_result[j,11] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,9] * weightsig2 
        jointprobtemp_result[j,12] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,8] * weightsig1 +
                                      jointprobtemp_result[j,9] * weightsig2 
        
        jointprobtemp_result[j,13] <-  Upcandidates[[i]][j]   
      }
      

      jointprob[[length(jointprob) + 1]] <- jointprobtemp
      jointprob_sig [[length(jointprob_sig) + 1 ]] <- jointprobtemp_sig
      jointprob_result[[length(jointprob_result)+1]] <-  jointprobtemp_result
      
      jointprobtemp_result <- data.frame()
      jointprobtemp <- data.frame()
      jointprobtemp_sig <- data.frame()
      
      
      idxjointprob[i,1] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][7] ) )
      idxjointprob[i,2] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][8] ) )
      idxjointprob[i,3] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][9] ) )  
      idxjointprob[i,4] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][10] ) ) 
      idxjointprob[i,5] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][11] ) )  
      idxjointprob[i,6] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][12] ) ) 
      
      UpFinalcandidates[i,1] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,1]] ]  
      UpFinalcandidates[i,2] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,2]] ]  
      UpFinalcandidates[i,3] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,3]] ]  
      UpFinalcandidates[i,4] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,4]] ]  
      UpFinalcandidates[i,5] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,5]] ] 
      UpFinalcandidates[i,6] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,6]] ]  
      
      
    } 
  }
  
  
  if ( as.numeric ( Downheader_new$FHWAclass [i] ) < 8 ) {  # su
    
    if ( is.na ( Attribute_diff_nonnormal[[i]][[1]][1] )  ) {
      jointprob[[length(jointprob) +1]] <- 999
      jointprob_sig[[length(jointprob_sig) +1]] <- 999
      jointprob_result[[length(jointprob_result) +1]] <- 999
      
      idxjointprob[i,1] <- 999
      idxjointprob[i,2] <- 999
      idxjointprob[i,3] <- 999
      idxjointprob[i,4] <- 999
      idxjointprob[i,5] <- 999
      idxjointprob[i,6] <- 999
      
      
      UpFinalcandidates[i,1] <- 999
      UpFinalcandidates[i,2] <- 999
      UpFinalcandidates[i,3] <- 999
      UpFinalcandidates[i,4] <- 999
      UpFinalcandidates[i,5] <- 999
      UpFinalcandidates[i,6] <- 999
      
      
    }
    
    else {
      
      for (j in 1: length(  Upcandidatesindex[[i]]  )  ) {
        
        for (m in 1: wimfeatlen) {

          if ( m %in% classallidxprob) {
            jointprobtemp[j,m] <- as.numeric ( (approx( diffseq_mat_c_su[[m]],  
                               normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
                               Attribute_diff_nonnormal[[i]][[j]][m]) )$y )
               
          }
            
          else {
            jointprobtemp[j,m] <- 99999
          }
          
        }
        
        for (n in 1: sigfeatlen) {
          
          jointprobtemp_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                 normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                 Attribute_sig_nonnormal[[i]][[j]][[1]][n]) )$y )
    
        }
        
        
        jointprobtemp[j,31] [is.na(jointprobtemp[j,31] )] <- bufsig
        
        jointprobtemp[is.na(jointprobtemp )] <- buf 
        jointprobtemp[jointprobtemp == 0] <- buf 
        
        jointprobtemp_sig [is.na(jointprobtemp_sig )] <- bufsig 
        jointprobtemp_sig [jointprobtemp_sig == 0] <- bufsig
        
        
        jointprobtemp_result[j,1] <-  log10(jointprobtemp[j,1]) * wimweight_su[1] +  
                                      log10(jointprobtemp[j,2]) * wimweight_su[2] +
                                      log10(jointprobtemp[j,3]) * wimweight_su[3] +
                                      log10(jointprobtemp[j,4]) * wimweight_su[4] +
                                      log10(jointprobtemp[j,5]) * wimweight_su[5] +
                                      log10(jointprobtemp[j,6]) * wimweight_su[6] +
                                      log10(jointprobtemp[j,7]) * wimweight_su[7] +
                                      log10(jointprobtemp[j,13]) * wimweight_su[13] +
                                      log10(jointprobtemp[j,14]) * wimweight_su[14] +
                                      log10(jointprobtemp[j,15]) * wimweight_su[15] +
                                      log10(jointprobtemp[j,16]) * wimweight_su[16] +
                                      log10(jointprobtemp[j,17]) * wimweight_su[17] +
                                      log10(jointprobtemp[j,18]) * wimweight_su[18] +
                                      log10(jointprobtemp[j,19]) * wimweight_su[19] +
                                      log10(jointprobtemp[j,20]) * wimweight_su[20] +
                                      log10(jointprobtemp[j,21]) * wimweight_su[21] +
                                      log10(jointprobtemp[j,30]) * wimweight_su[30] 
                                    
        jointprobtemp_result[j,2] <- log10( jointprobtemp[j,31]) * wimweight_su[31] 
        jointprobtemp_result[j,3] <- 0
        
        for ( n in 1: sigfeatlen){
          jointprobtemp_result[j,3] <-  jointprobtemp_result[j,3] + log10(jointprobtemp_sig[j,n]) * sigweight_su[n]
        }

        jointprobtemp_result[j,4] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,2] 
        jointprobtemp_result[j,5] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,3] 
        jointprobtemp_result[j,6] <-  jointprobtemp_result[j,1]  + jointprobtemp_result[j,2]  + jointprobtemp_result[j,3] 
        
        
      }
      # normalization
      for (j in 1: length(  Upcandidatesindex[[i]]  )  ) {
        maxtemp[1] <- max( jointprobtemp_result[,1] )
        mintemp[1] <- min( jointprobtemp_result[,1] )
        jointprobtemp_result[j,7] <- ( jointprobtemp_result[j,1] - max( jointprobtemp_result[,1] ) ) / 
                                     ( max( jointprobtemp_result[,1] ) - min( jointprobtemp_result[,1] +1) )
        jointprobtemp_result[j,8] <- ( jointprobtemp_result[j,2] - max( jointprobtemp_result[,2] ) ) / 
                                     ( max( jointprobtemp_result[,2] ) - min( jointprobtemp_result[,2] +1) )
        jointprobtemp_result[j,9] <- ( jointprobtemp_result[j,3] - max( jointprobtemp_result[,3] ) ) / 
                                     ( max( jointprobtemp_result[,3] ) - min( jointprobtemp_result[,3] +1) )
        jointprobtemp_result[j,10] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,8] * weightsig1 
        jointprobtemp_result[j,11] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,9] * weightsig2 
        jointprobtemp_result[j,12] <- jointprobtemp_result[j,7] * weightwim + jointprobtemp_result[j,8] * weightsig1 +
                                      jointprobtemp_result[j,9] * weightsig2 
        
        jointprobtemp_result[j,13] <-  Upcandidates[[i]][j]   
      }
      
      
      
      jointprob[[length(jointprob) + 1]] <- jointprobtemp
      jointprob_sig [[length(jointprob_sig) + 1 ]] <- jointprobtemp_sig
      jointprob_result[[length(jointprob_result)+1]] <-  jointprobtemp_result
      
      jointprobtemp_result <- data.frame()
      jointprobtemp <- data.frame()
      jointprobtemp_sig <- data.frame()
      
      
      idxjointprob[i,1] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][7] ) )
      idxjointprob[i,2] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][8] ) )
      idxjointprob[i,3] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][9] ) )  
      idxjointprob[i,4] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][10] ) ) 
      idxjointprob[i,5] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][11] ) )  
      idxjointprob[i,6] <- which.max(unlist ( jointprob_result[[length(jointprob_result)]][12] ) ) 
      
      UpFinalcandidates[i,1] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,1]] ]  
      UpFinalcandidates[i,2] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,2]] ]  
      UpFinalcandidates[i,3] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,3]] ]  
      UpFinalcandidates[i,4] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,4]] ]  
      UpFinalcandidates[i,5] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,5]] ] 
      UpFinalcandidates[i,6] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[i,6]] ]  
      
      
    } 
  }


}





### performance 
#index for class 9
idxFortt <- which(Downheader_new$FHWAclass >= 8)
idxForsu <- which(Downheader_new$FHWAclass < 8)


MisMatching_temp  <- data.frame()

# ix <- seq(from=1, to=50)
# Target_baseanalysis_table <- Target_baseanalysis_table[ix,]
MisMatching_temp <- cbind(Target_baseanalysis_table[,1] , Target_baseanalysis_table[,2], Target_baseanalysis_table[,4], 
                                      UpFinalcandidates[,1] ,  UpFinalcandidates[,2] ,
                                      UpFinalcandidates[,3] ,  UpFinalcandidates[,4] ,  
                                      UpFinalcandidates[,5] ,  UpFinalcandidates[,6] )

MisMatching_temp[is.na (MisMatching_temp)]  <- c(999)


## TT
ResultMisMatching_tt <- subset( MisMatching_temp , MisMatching_temp[,1] >= 8 )
## SU
ResultMisMatching_su <- subset( MisMatching_temp , MisMatching_temp[,1] < 8 )
## ALL
ResultMisMatching_all <- rbind( ResultMisMatching_tt , ResultMisMatching_su  )


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Mismatching_07132015_set1v2")
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Mismatching_07132015_set2v2")


