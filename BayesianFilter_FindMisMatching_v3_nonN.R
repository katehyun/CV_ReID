# find mismatching
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_06232015")
rm(list=ls())

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_02112015")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_05062015")
library(stringr)

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

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_06232015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif_06232015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_06232015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist_train.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist_test.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_train.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_test.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_NN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/SOLCFHWAClass.RData" )
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_06232015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_train.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_test.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Target_baseanalysis_Jan0910_table.RData")

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


############################ start here ############################
weightwim  <- 1
weightsig1 <- 0
weightsig2 <- 1 

thresholdForDif <- 2.5
buf <- 0.001 
bufsig <- 0.000000000001
utcbd <- 1357804800000
sigfeatlen <- 50
wimfeatlen <- 31

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
    
    Attribute_sig_train_temp[[j]] <- sigfeature_train[[i]][ Upcandidatesindex_train[[i]][j]  ]
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
      
      Attribute_sig_test_temp[[j]] <- sigfeature_test[[i]][Upcandidatesindex_test[[i]][j]]
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
classallidxprob <- c(1:7, 12:21, 30:31)

Downtarget_attributes_train_tt <- subset ( Downtarget_attributes_train, Downtarget_attributes_train [,2] >= 8  )
Downtarget_attributes_train_su <- subset ( Downtarget_attributes_train, Downtarget_attributes_train [,2] < 8  )

weightwim <- 1
weightsig1 <- 0
weightsig2 <- 2

ResultMismatching_train_all <- data.frame()
ResultMismatching_test_all <- data.frame()
ResultMismatching_train_tt <- data.frame()
ResultMismatching_test_tt <- data.frame()
ResultMismatching_train_su <- data.frame()
ResultMismatching_test_su <- data.frame()


# for ( weightwim in seq(from=0, to=2, by=1)) {
#   for ( weightsig1 in seq(from=1, to=2, by=1)) {
#     for ( weightsig2 in seq(from=1, to=2, by=1)) {

      for (i in 1:length(Upcandidates_train)){
        
        
        if ( as.numeric ( Downtarget_attributes_train [i,2] ) >= 8 ) {  # tt
          
          weightwim <- 1
          weightsig1 <- 0
          weightsig2 <- 2
          
          if ( is.na ( Attribute_diff_nonnormal_train[[i]][[1]][1] )  ) {
            jointprob_train[[length(jointprob_train) +1]] <- 999
            jointprob_train_sig[[length(jointprob_train_sig) +1]] <- 999
            jointprob_train_result[[length(jointprob_train_result) +1]] <- 999
            
            idxjointprob_train[i,1] <- 999
            idxjointprob_train[i,2] <- 999
            idxjointprob_train[i,3] <- 999
            idxjointprob_train[i,4] <- 999
            idxjointprob_train[i,5] <- 999
            idxjointprob_train[i,6] <- 999
            
            
            UpFinalcandidates_train[i,1] <- 999
            UpFinalcandidates_train[i,2] <- 999
            UpFinalcandidates_train[i,3] <- 999
            UpFinalcandidates_train[i,4] <- 999
            UpFinalcandidates_train[i,5] <- 999
            UpFinalcandidates_train[i,6] <- 999
            
            
          }
          
          else {
            
            for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
              
              #           for (m in 1: 31) {
              for (m in 1: wimfeatlen) {
                
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
                if ( m %in% classallidxprob) {
                  jointprobtemp_train[j,m] <- as.numeric ( (approx( diffseq_mat_c_tt[[m]],  
                                                                    normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
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
              
              for (n in 1: sigfeatlen) {
                
                jointprobtemp_train_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                                                      normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                                                      Attribute_sig_nonnormal_train[[i]][[j]][[1]][n]) )$y )
                
                
                
              }
              
              
              jointprobtemp_train[j,31] [is.na(jointprobtemp_train[j,31] )] <- bufsig
              
              jointprobtemp_train [is.na(jointprobtemp_train )] <- buf 
              jointprobtemp_train [jointprobtemp_train == 0] <- buf 
              
              jointprobtemp_train_sig [is.na(jointprobtemp_train_sig )] <- bufsig 
              jointprobtemp_train_sig [jointprobtemp_train_sig == 0] <- bufsig
              
              
              jointprobtemp_train_result[j,1] <-   log10(jointprobtemp_train[j,1]) * wimweight_tt[1] +  
                log10(jointprobtemp_train[j,2]) * wimweight_tt[2] +
                log10(jointprobtemp_train[j,3]) * wimweight_tt[3] +
                log10(jointprobtemp_train[j,4]) * wimweight_tt[4] +
                log10(jointprobtemp_train[j,5]) * wimweight_tt[5] +
                log10(jointprobtemp_train[j,6]) * wimweight_tt[6] +
                log10(jointprobtemp_train[j,7]) * wimweight_tt[7] +
                log10(jointprobtemp_train[j,13]) * wimweight_tt[13] +
                log10(jointprobtemp_train[j,14]) * wimweight_tt[14] +
                log10(jointprobtemp_train[j,15]) * wimweight_tt[15] +
                log10(jointprobtemp_train[j,16]) * wimweight_tt[16] +
                log10(jointprobtemp_train[j,17]) * wimweight_tt[17] +
                log10(jointprobtemp_train[j,18]) * wimweight_tt[18] +
                log10(jointprobtemp_train[j,19]) * wimweight_tt[19] +
                log10(jointprobtemp_train[j,20]) * wimweight_tt[20] +
                log10(jointprobtemp_train[j,21]) * wimweight_tt[21] +
                log10(jointprobtemp_train[j,30]) * wimweight_tt[30] 
              
              jointprobtemp_train_result[j,2] <- log10( jointprobtemp_train[j,31]) * wimweight_tt[31] 
              jointprobtemp_train_result[j,3] <- 0
              
              for ( n in 1: sigfeatlen){
                jointprobtemp_train_result[j,3] <-  jointprobtemp_train_result[j,3] +
                  log10(jointprobtemp_train_sig[j,n]) * sigweight_tt[n]
              }
              
              
              
              
              jointprobtemp_train_result[j,4] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,2] 
              jointprobtemp_train_result[j,5] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,3] 
              jointprobtemp_train_result[j,6] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,2]  +
                jointprobtemp_train_result[j,3] 
              
              
            }
            # normalization
            for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
              maxtemp[1] <- max( jointprobtemp_train_result[,1] )
              mintemp[1] <- min( jointprobtemp_train_result[,1] )
              jointprobtemp_train_result[j,7] <- ( jointprobtemp_train_result[j,1] - max( jointprobtemp_train_result[,1] ) ) / 
                ( max( jointprobtemp_train_result[,1] ) - min( jointprobtemp_train_result[,1] +1) )
              jointprobtemp_train_result[j,8] <- ( jointprobtemp_train_result[j,2] - max( jointprobtemp_train_result[,2] ) ) / 
                ( max( jointprobtemp_train_result[,2] ) - min( jointprobtemp_train_result[,2] +1) )
              jointprobtemp_train_result[j,9] <- ( jointprobtemp_train_result[j,3] - max( jointprobtemp_train_result[,3] ) ) / 
                ( max( jointprobtemp_train_result[,3] ) - min( jointprobtemp_train_result[,3] +1) )
              jointprobtemp_train_result[j,10] <- jointprobtemp_train_result[j,7] * weightwim +  
                jointprobtemp_train_result[j,8] * weightsig1 
              jointprobtemp_train_result[j,11] <- jointprobtemp_train_result[j,7] * weightwim +
                jointprobtemp_train_result[j,9] * weightsig2 
              jointprobtemp_train_result[j,12] <- jointprobtemp_train_result[j,7] * weightwim +
                jointprobtemp_train_result[j,8] * weightsig1 +
                jointprobtemp_train_result[j,9] * weightsig2 
              
              jointprobtemp_train_result[j,13] <-  Upcandidates_train[[i]][j]   
            }
            
            
            
            jointprob_train [[length(jointprob_train) + 1]] <- jointprobtemp_train
            jointprob_train_sig [[length(jointprob_train_sig) + 1 ]] <- jointprobtemp_train_sig
            jointprob_train_result[[length(jointprob_train_result)+1]] <-  jointprobtemp_train_result
            
            jointprobtemp_train_result <- data.frame()
            jointprobtemp_train <- data.frame()
            jointprobtemp_train_sig <- data.frame()
            
            
            idxjointprob_train[i,1] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][7] ) )
            idxjointprob_train[i,2] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][8] ) )
            idxjointprob_train[i,3] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][9] ) )  
            idxjointprob_train[i,4] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][10] ) ) 
            idxjointprob_train[i,5] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][11] ) )  
            idxjointprob_train[i,6] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][12] ) ) 
            
            UpFinalcandidates_train[i,1] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,1]] ]  
            UpFinalcandidates_train[i,2] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,2]] ]  
            UpFinalcandidates_train[i,3] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,3]] ]  
            UpFinalcandidates_train[i,4] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,4]] ]  
            UpFinalcandidates_train[i,5] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,5]] ] 
            UpFinalcandidates_train[i,6] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,6]] ]  
            
            
          } 
        }
        
        
        if ( as.numeric ( Downtarget_attributes_train [i,2] ) < 8 ) {  # su
          
          weightwim <- 1
          weightsig1 <- 0
          weightsig2 <- 2
          
          if ( is.na ( Attribute_diff_nonnormal_train[[i]][[1]][1] )  ) {
            jointprob_train[[length(jointprob_train) +1]] <- 999
            jointprob_train_sig[[length(jointprob_train_sig) +1]] <- 999
            jointprob_train_result[[length(jointprob_train_result) +1]] <- 999
            
            idxjointprob_train[i,1] <- 999
            idxjointprob_train[i,2] <- 999
            idxjointprob_train[i,3] <- 999
            idxjointprob_train[i,4] <- 999
            idxjointprob_train[i,5] <- 999
            idxjointprob_train[i,6] <- 999
            
            
            UpFinalcandidates_train[i,1] <- 999
            UpFinalcandidates_train[i,2] <- 999
            UpFinalcandidates_train[i,3] <- 999
            UpFinalcandidates_train[i,4] <- 999
            UpFinalcandidates_train[i,5] <- 999
            UpFinalcandidates_train[i,6] <- 999
            
            
          }
          
          else {
            
            for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
              
              #           for (m in 1: 31) {
              for (m in 1: wimfeatlen) {
                
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
                if ( m %in% classallidxprob) {
                  jointprobtemp_train[j,m] <- as.numeric ( (approx( diffseq_mat_c_su[[m]],  
                                                                    normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
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
              
              for (n in 1: sigfeatlen) {
                
                jointprobtemp_train_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                                                      normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                      Attribute_sig_nonnormal_train[[i]][[j]][[1]][n]) )$y )
                
                
                
              }
              
              
              jointprobtemp_train[j,31] [is.na(jointprobtemp_train[j,31] )] <- bufsig
              
              jointprobtemp_train [is.na(jointprobtemp_train )] <- buf 
              jointprobtemp_train [jointprobtemp_train == 0] <- buf 
              
              jointprobtemp_train_sig [is.na(jointprobtemp_train_sig )] <- bufsig 
              jointprobtemp_train_sig [jointprobtemp_train_sig == 0] <- bufsig
              
              
              jointprobtemp_train_result[j,1] <-   log10(jointprobtemp_train[j,1]) * wimweight_su[1] +  
                log10(jointprobtemp_train[j,2]) * wimweight_su[2] +
                log10(jointprobtemp_train[j,3]) * wimweight_su[3] +
                log10(jointprobtemp_train[j,4]) * wimweight_su[4] +
                log10(jointprobtemp_train[j,5]) * wimweight_su[5] +
                log10(jointprobtemp_train[j,6]) * wimweight_su[6] +
                log10(jointprobtemp_train[j,7]) * wimweight_su[7] +
                log10(jointprobtemp_train[j,13]) * wimweight_su[13] +
                log10(jointprobtemp_train[j,14]) * wimweight_su[14] +
                log10(jointprobtemp_train[j,15]) * wimweight_su[15] +
                log10(jointprobtemp_train[j,16]) * wimweight_su[16] +
                log10(jointprobtemp_train[j,17]) * wimweight_su[17] +
                log10(jointprobtemp_train[j,18]) * wimweight_su[18] +
                log10(jointprobtemp_train[j,19]) * wimweight_su[19] +
                log10(jointprobtemp_train[j,20]) * wimweight_su[20] +
                log10(jointprobtemp_train[j,21]) * wimweight_su[21] +
                log10(jointprobtemp_train[j,30]) * wimweight_su[30] 
              
              jointprobtemp_train_result[j,2] <- log10( jointprobtemp_train[j,31]) * wimweight_su[31] 
              jointprobtemp_train_result[j,3] <- 0
              
              for ( n in 1: sigfeatlen){
                jointprobtemp_train_result[j,3] <-  jointprobtemp_train_result[j,3] +
                  log10(jointprobtemp_train_sig[j,n]) * sigweight_su[n]
              }
              
              
              
              
              jointprobtemp_train_result[j,4] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,2] 
              jointprobtemp_train_result[j,5] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,3] 
              jointprobtemp_train_result[j,6] <-  jointprobtemp_train_result[j,1]  +
                jointprobtemp_train_result[j,2]  +
                jointprobtemp_train_result[j,3] 
              
              
            }
            # normalization
            for (j in 1: length(  Upcandidatesindex_train[[i]]  )  ) {
              maxtemp[1] <- max( jointprobtemp_train_result[,1] )
              mintemp[1] <- min( jointprobtemp_train_result[,1] )
              jointprobtemp_train_result[j,7] <- ( jointprobtemp_train_result[j,1] - max( jointprobtemp_train_result[,1] ) ) / 
                ( max( jointprobtemp_train_result[,1] ) - min( jointprobtemp_train_result[,1] +1) )
              jointprobtemp_train_result[j,8] <- ( jointprobtemp_train_result[j,2] - max( jointprobtemp_train_result[,2] ) ) / 
                ( max( jointprobtemp_train_result[,2] ) - min( jointprobtemp_train_result[,2] +1) )
              jointprobtemp_train_result[j,9] <- ( jointprobtemp_train_result[j,3] - max( jointprobtemp_train_result[,3] ) ) / 
                ( max( jointprobtemp_train_result[,3] ) - min( jointprobtemp_train_result[,3] +1) )
              jointprobtemp_train_result[j,10] <- jointprobtemp_train_result[j,7] * weightwim +  
                jointprobtemp_train_result[j,8] * weightsig1 
              jointprobtemp_train_result[j,11] <- jointprobtemp_train_result[j,7] * weightwim +
                jointprobtemp_train_result[j,9] * weightsig2 
              jointprobtemp_train_result[j,12] <- jointprobtemp_train_result[j,7] * weightwim +
                jointprobtemp_train_result[j,8] * weightsig1 +
                jointprobtemp_train_result[j,9] * weightsig2 
              
              jointprobtemp_train_result[j,13] <-  Upcandidates_train[[i]][j]   
            }
            
            
            
            jointprob_train [[length(jointprob_train) + 1]] <- jointprobtemp_train
            jointprob_train_sig [[length(jointprob_train_sig) + 1 ]] <- jointprobtemp_train_sig
            jointprob_train_result[[length(jointprob_train_result)+1]] <-  jointprobtemp_train_result
            
            jointprobtemp_train_result <- data.frame()
            jointprobtemp_train <- data.frame()
            jointprobtemp_train_sig <- data.frame()
            
            
            idxjointprob_train[i,1] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][7] ) )
            idxjointprob_train[i,2] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][8] ) )
            idxjointprob_train[i,3] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][9] ) )  
            idxjointprob_train[i,4] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][10] ) ) 
            idxjointprob_train[i,5] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][11] ) )  
            idxjointprob_train[i,6] <- which.max(unlist ( jointprob_train_result[[length(jointprob_train_result)]][12] ) ) 
            
            UpFinalcandidates_train[i,1] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,1]] ]  
            UpFinalcandidates_train[i,2] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,2]] ]  
            UpFinalcandidates_train[i,3] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,3]] ]  
            UpFinalcandidates_train[i,4] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,4]] ]  
            UpFinalcandidates_train[i,5] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,5]] ] 
            UpFinalcandidates_train[i,6] <- Upsiglist_train[[i]][ Upcandidatesindex_train[[i]][idxjointprob_train[i,6]] ]  
            
            
          } 
        }
      }
      
      #     else # class is not 9
      #       
      #     {
      #       jointprob_train_result[[length(jointprob_train_result)+1]] <-  NA 
      #       jointprob_train[[length(jointprob_train)+1]] <-  NA 
      #       jointprob_train_sig[[length(jointprob_train_sig)+1]] <-  NA  
      #       idxjointprob_train[i,1] <- NA
      #       idxjointprob_train[i,2] <- NA
      #       idxjointprob_train[i,3] <- NA
      #       idxjointprob_train[i,4] <- NA
      #       idxjointprob_train[i,5] <- NA
      #       idxjointprob_train[i,6] <- NA
      # 
      # 
      #       UpFinalcandidates_train[i,1] <- NA
      #       UpFinalcandidates_train[i,2] <- NA
      #       UpFinalcandidates_train[i,3] <- NA
      #       UpFinalcandidates_train[i,4] <- NA
      #       UpFinalcandidates_train[i,5] <- NA
      #       UpFinalcandidates_train[i,6] <- NA
      #   
      # 
      #     }
    }
    
    
    
    
    
    ### performance 
    #index for class 9
    idxFortt <- which(Downheader_new[,14] >= 8)
    idxForsu <- which(Downheader_new[,14] < 8)
    
    # rm(ResultMisMatching_train, ResultMisMatching_train)
    ResultMisMatching_train_temp  <- data.frame()

    
    
    
    
    
    ResultMisMatching_train_temp <- cbind(Target_baseanalysis_Jan0910_table_train[,1], Target_baseanalysis_Jan0910_table_train[,6],
                                          Target_baseanalysis_Jan0910_table_train[,4], 
                                          UpFinalcandidates_train[,1] ,  UpFinalcandidates_train[,2] ,
                                          UpFinalcandidates_train[,3] ,  UpFinalcandidates_train[,4] ,  
                                          UpFinalcandidates_train[,5] , UpFinalcandidates_train[,6] )
    

    
    ResultMisMatching_train_temp[is.na ( ResultMisMatching_train_temp)]  <- c(999)

    
    ## TT
    
    ResultMisMatching_train_tt <- subset( ResultMisMatching_train_temp , ResultMisMatching_train_temp[,1] >= 8 )
 
    
    
    ## SU
    ResultMisMatching_train_su <- subset( ResultMisMatching_train_temp , ResultMisMatching_train_temp[,1] < 8 )

    
    
    ## ALL
    ResultMisMatching_train_all <- rbind(ResultMisMatching_train_tt , ResultMisMatching_train_su  )

    
    
    # Train
    # ResultMismatching_train_tt <- data.frame()
    # ResultMismatching_train_su <- data.frame()
    # ResultMismatching_train_all <- data.frame()
    
    ## TT
    
    TargetTable_train_tt <- ResultMisMatching_train_tt 
    Target_obj_train_tt  <- ResultMisMatching_train_tt [,2]
    
    missing_obj_train_tt  <- length (Target_obj_train_tt[Target_obj_train_tt == 999]) 
    matching_obj_train_tt <- length (Target_obj_train_tt[Target_obj_train_tt != 999]) 
    
    CVeh_train_tt <- matching_obj_train_tt[1]
    Veh_train_tt <- length(TargetTable_train_tt[,1])
    
    matching_NN_train_tt <- vector()
    MVeh_train_tt  <- vector()
    CMVeh_train_tt  <- vector()
    MMVeh_train_tt  <- vector()
    SIMR_train_tt  <- vector()
    SCMR_train_tt  <- vector()
    SER_train_tt  <- vector()
    
    
    
    for (i in 1: 6) {
      CMVeh_train_tt[i] <- sum ( as.numeric ((ResultMisMatching_train_tt [,2]) == as.numeric (ResultMisMatching_train_tt [,i+3])) &
                                   as.numeric (ResultMisMatching_train_tt [,2]) != 999)
      MVeh_train_tt[i] <- sum(   (as.numeric( TargetTable_train_tt [,i+3])) > 1000 ) 
      
      
      MMVeh_train_tt[i] <- length(  subset(TargetTable_train_tt[,1], as.numeric( Target_obj_train_tt ) 
                                           !=  as.numeric( TargetTable_train_tt[,i+3])   ))  
      SIMR_train_tt[i] <- CMVeh_train_tt[i] / CVeh_train_tt[1]
      SCMR_train_tt[i] <- CMVeh_train_tt[i] / MVeh_train_tt[i]
      SER_train_tt[i] <- MMVeh_train_tt[i] / Veh_train_tt
      
      ResultMismatching_train_tt <- rbind(ResultMismatching_train_tt, c(i, weightwim , weightsig1, weightsig2,
                                                                        matching_obj_train_tt[1], missing_obj_train_tt[1],              
                                                                        CMVeh_train_tt[i], CVeh_train_tt[1], MVeh_train_tt[i],
                                                                        SIMR_train_tt[i], SCMR_train_tt[i], MMVeh_train_tt[i], 
                                                                        Veh_train_tt[1], SER_train_tt[i] ))
      
    }
    
    
    ## SU
    
    TargetTable_train_su <- ResultMisMatching_train_su 
    Target_obj_train_su  <- ResultMisMatching_train_su [,2]
    
    missing_obj_train_su  <- length (Target_obj_train_su[Target_obj_train_su == 999]) 
    matching_obj_train_su <- length (Target_obj_train_su[Target_obj_train_su != 999]) 
    
    CVeh_train_su <- matching_obj_train_su[1]
    Veh_train_su <- length(TargetTable_train_su[,1])
    
    matching_NN_train_su <- vector()
    MVeh_train_su  <- vector()
    CMVeh_train_su  <- vector()
    MMVeh_train_su  <- vector()
    SIMR_train_su  <- vector()
    SCMR_train_su  <- vector()
    SER_train_su  <- vector()
    
    
    
    for (i in 1: 6) {
      CMVeh_train_su[i] <- sum ( as.numeric ((ResultMisMatching_train_su [,2]) == as.numeric (ResultMisMatching_train_su [,i+3])) &
                                   as.numeric (ResultMisMatching_train_su [,2]) != 999)
      MVeh_train_su[i] <- sum(   (as.numeric( TargetTable_train_su [,i+3])) > 1000 ) 
      
      
      MMVeh_train_su[i] <- length(  subset(TargetTable_train_su[,1], as.numeric( Target_obj_train_su ) 
                                           !=  as.numeric( TargetTable_train_su[,i+3])   ))  
      SIMR_train_su[i] <- CMVeh_train_su[i] / CVeh_train_su[1]
      SCMR_train_su[i] <- CMVeh_train_su[i] / MVeh_train_su[i]
      SER_train_su[i] <- MMVeh_train_su[i] / Veh_train_su
      
      ResultMismatching_train_su <- rbind(ResultMismatching_train_su, c(i, weightwim , weightsig1, weightsig2,
                                                                        matching_obj_train_su[1], missing_obj_train_su[1],              
                                                                        CMVeh_train_su[i], CVeh_train_su[1], MVeh_train_su[i],
                                                                        SIMR_train_su[i], SCMR_train_su[i], MMVeh_train_su[i], 
                                                                        Veh_train_su[1], SER_train_su[i] ))
      
    }
    
    
    ## ALL
    
    TargetTable_train_all <- rbind( ResultMisMatching_train_tt  , ResultMisMatching_train_su )
    Target_obj_train_all  <- ResultMisMatching_train_all [,2]
    
    missing_obj_train_all  <- length (Target_obj_train_all[Target_obj_train_all == 999]) 
    matching_obj_train_all <- length (Target_obj_train_all[Target_obj_train_all != 999]) 
    
    missing_NN_train_all <- sum ( as.numeric (ResultMisMatching_train_all[,2]) == c(999))
    CVeh_train_all <- matching_obj_train_all[1]
    Veh_train_all <- length(TargetTable_train_all[,1])
    
    matching_NN_train_all <- vector()
    MVeh_train_all  <- vector()
    CMVeh_train_all  <- vector()
    MMVeh_train_all  <- vector()
    SIMR_train_all  <- vector()
    SCMR_train_all  <- vector()
    SER_train_all  <- vector()
    
    
    for (i in 1: 6) {
      CMVeh_train_all[i] <- sum ( as.numeric ((ResultMisMatching_train_all [,2]) == as.numeric (ResultMisMatching_train_all [,i+3])) &
                                    as.numeric (ResultMisMatching_train_all [,2]) != 999)
      MVeh_train_all[i] <- sum(   (as.numeric( TargetTable_train_all [,i+3])) > 1000 ) 
      
      
      MMVeh_train_all[i] <- length(  subset(TargetTable_train_all[,1], as.numeric( Target_obj_train_all ) 
                                            !=  as.numeric( TargetTable_train_all[,i+3])   ))  
      SIMR_train_all[i] <- CMVeh_train_all[i] / CVeh_train_all[1]
      SCMR_train_all[i] <- CMVeh_train_all[i] / MVeh_train_all[i]
      SER_train_all[i] <- MMVeh_train_all[i] / Veh_train_all
      
      ResultMismatching_train_all <- rbind( ResultMismatching_train_all, c(i, weightwim , weightsig1, weightsig2,
                                                                           matching_obj_train_all[1], missing_obj_train_all[1],              
                                                                           CMVeh_train_all[i], CVeh_train_all[1], MVeh_train_all[i],
                                                                           SIMR_train_all[i], SCMR_train_all[i], MMVeh_train_all[i], 
                                                                           Veh_train_all[1], SER_train_all[i] ))
      
    }
    
    
      
      for (i in 1:length(Upcandidates_test)){
        
        
        if ( as.numeric ( Downtarget_attributes_test [i,2] ) >= 8 ) {  # tt
          
          if ( is.na ( Attribute_diff_nonnormal_test[[i]][[1]][1] )  ) {
            jointprob_test[[length(jointprob_test) +1]] <- 999
            jointprob_test_sig[[length(jointprob_test_sig) +1]] <- 999
            jointprob_test_result[[length(jointprob_test_result) +1]] <- 999
            
            idxjointprob_test[i,1] <- 999
            idxjointprob_test[i,2] <- 999
            idxjointprob_test[i,3] <- 999
            idxjointprob_test[i,4] <- 999
            idxjointprob_test[i,5] <- 999
            idxjointprob_test[i,6] <- 999
            
            
            UpFinalcandidates_test[i,1] <- 999
            UpFinalcandidates_test[i,2] <- 999
            UpFinalcandidates_test[i,3] <- 999
            UpFinalcandidates_test[i,4] <- 999
            UpFinalcandidates_test[i,5] <- 999
            UpFinalcandidates_test[i,6] <- 999
            
            
          }
          
          else {
            
            for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
              
              #           for (m in 1: 31) {
              for (m in 1: wimfeatlen) {
                
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
                if ( m %in% classallidxprob) {
                  jointprobtemp_test[j,m] <- as.numeric ( (approx( diffseq_mat_c_tt[[m]],  
                                                                   normal_mat_c_tt[[m]]  * multiplier_hist_mat_c_tt[[m]][ which.min(is.na( multiplier_hist_mat_c_tt[[m]] ) ) ] ,
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
              
              for (n in 1: sigfeatlen) {
                
                jointprobtemp_test_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_tt[[n]],  
                                                                     normal_mat_c_sig_tt[[n]]  *  multiplier_hist_mat_c_sig_tt[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[n]] ) ) ] ,
                                                                     Attribute_sig_nonnormal_test[[i]][[j]][[1]][n]) )$y )
                
                
                
              }
              
              
              jointprobtemp_test[j,31] [is.na(jointprobtemp_test[j,31] )] <- bufsig
              
              jointprobtemp_test [is.na(jointprobtemp_test )] <- buf 
              jointprobtemp_test [jointprobtemp_test == 0] <- buf 
              
              jointprobtemp_test_sig [is.na(jointprobtemp_test_sig )] <- bufsig 
              jointprobtemp_test_sig [jointprobtemp_test_sig == 0] <- bufsig
              
              
              jointprobtemp_test_result[j,1] <-   log10(jointprobtemp_test[j,1]) * wimweight_tt[1] +  
                log10(jointprobtemp_test[j,2]) * wimweight_tt[2] +
                log10(jointprobtemp_test[j,3]) * wimweight_tt[3] +
                log10(jointprobtemp_test[j,4]) * wimweight_tt[4] +
                log10(jointprobtemp_test[j,5]) * wimweight_tt[5] +
                log10(jointprobtemp_test[j,6]) * wimweight_tt[6] +
                log10(jointprobtemp_test[j,7]) * wimweight_tt[7] +
                log10(jointprobtemp_test[j,13]) * wimweight_tt[13] +
                log10(jointprobtemp_test[j,14]) * wimweight_tt[14] +
                log10(jointprobtemp_test[j,15]) * wimweight_tt[15] +
                log10(jointprobtemp_test[j,16]) * wimweight_tt[16] +
                log10(jointprobtemp_test[j,17]) * wimweight_tt[17] +
                log10(jointprobtemp_test[j,18]) * wimweight_tt[18] +
                log10(jointprobtemp_test[j,19]) * wimweight_tt[19] +
                log10(jointprobtemp_test[j,20]) * wimweight_tt[20] +
                log10(jointprobtemp_test[j,21]) * wimweight_tt[21] +
                log10(jointprobtemp_test[j,30]) * wimweight_tt[30] 
              
              jointprobtemp_test_result[j,2] <- log10( jointprobtemp_test[j,31]) * wimweight_tt[31] 
              jointprobtemp_test_result[j,3] <- 0
              
              for ( n in 1: sigfeatlen){
                jointprobtemp_test_result[j,3] <-  jointprobtemp_test_result[j,3] +
                  log10(jointprobtemp_test_sig[j,n]) * sigweight_tt[n]
              }
              
              
              
              
              jointprobtemp_test_result[j,4] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,2] 
              jointprobtemp_test_result[j,5] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,3] 
              jointprobtemp_test_result[j,6] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,2]  +
                jointprobtemp_test_result[j,3] 
              
              
            }
            # normalization
            for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
              maxtemp[1] <- max( jointprobtemp_test_result[,1] )
              mintemp[1] <- min( jointprobtemp_test_result[,1] )
              jointprobtemp_test_result[j,7] <- ( jointprobtemp_test_result[j,1] - max( jointprobtemp_test_result[,1] ) ) / 
                ( max( jointprobtemp_test_result[,1] ) - min( jointprobtemp_test_result[,1] +1) )
              jointprobtemp_test_result[j,8] <- ( jointprobtemp_test_result[j,2] - max( jointprobtemp_test_result[,2] ) ) / 
                ( max( jointprobtemp_test_result[,2] ) - min( jointprobtemp_test_result[,2] +1) )
              jointprobtemp_test_result[j,9] <- ( jointprobtemp_test_result[j,3] - max( jointprobtemp_test_result[,3] ) ) / 
                ( max( jointprobtemp_test_result[,3] ) - min( jointprobtemp_test_result[,3] +1) )
              jointprobtemp_test_result[j,10] <- jointprobtemp_test_result[j,7] * weightwim +  
                jointprobtemp_test_result[j,8] * weightsig1 
              jointprobtemp_test_result[j,11] <- jointprobtemp_test_result[j,7] * weightwim +
                jointprobtemp_test_result[j,9] * weightsig2 
              jointprobtemp_test_result[j,12] <- jointprobtemp_test_result[j,7] * weightwim +
                jointprobtemp_test_result[j,8] * weightsig1 +
                jointprobtemp_test_result[j,9] * weightsig2 
              
              jointprobtemp_test_result[j,13] <-  Upcandidates_test[[i]][j]   
            }
            
            
            
            jointprob_test [[length(jointprob_test) + 1]] <- jointprobtemp_test
            jointprob_test_sig [[length(jointprob_test_sig) + 1 ]] <- jointprobtemp_test_sig
            jointprob_test_result[[length(jointprob_test_result)+1]] <-  jointprobtemp_test_result
            
            jointprobtemp_test_result <- data.frame()
            jointprobtemp_test <- data.frame()
            jointprobtemp_test_sig <- data.frame()
            
            
            idxjointprob_test[i,1] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][7] ) )
            idxjointprob_test[i,2] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][8] ) )
            idxjointprob_test[i,3] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][9] ) )  
            idxjointprob_test[i,4] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][10] ) ) 
            idxjointprob_test[i,5] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][11] ) )  
            idxjointprob_test[i,6] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][12] ) ) 
            
            UpFinalcandidates_test[i,1] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,1]] ]  
            UpFinalcandidates_test[i,2] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,2]] ]  
            UpFinalcandidates_test[i,3] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,3]] ]  
            UpFinalcandidates_test[i,4] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,4]] ]  
            UpFinalcandidates_test[i,5] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,5]] ] 
            UpFinalcandidates_test[i,6] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,6]] ]  
            
            
          } 
        }
        
        
        if ( as.numeric ( Downtarget_attributes_test [i,2] ) < 8 ) {  # su
          
          if ( is.na ( Attribute_diff_nonnormal_test[[i]][[1]][1] )  ) {
            jointprob_test[[length(jointprob_test) +1]] <- 999
            jointprob_test_sig[[length(jointprob_test_sig) +1]] <- 999
            jointprob_test_result[[length(jointprob_test_result) +1]] <- 999
            
            idxjointprob_test[i,1] <- 999
            idxjointprob_test[i,2] <- 999
            idxjointprob_test[i,3] <- 999
            idxjointprob_test[i,4] <- 999
            idxjointprob_test[i,5] <- 999
            idxjointprob_test[i,6] <- 999
            
            
            UpFinalcandidates_test[i,1] <- 999
            UpFinalcandidates_test[i,2] <- 999
            UpFinalcandidates_test[i,3] <- 999
            UpFinalcandidates_test[i,4] <- 999
            UpFinalcandidates_test[i,5] <- 999
            UpFinalcandidates_test[i,6] <- 999
            
            
          }
          
          else {
            
            for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
              
              #           for (m in 1: 31) {
              for (m in 1: wimfeatlen) {
                
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
                if ( m %in% classallidxprob) {
                  jointprobtemp_test[j,m] <- as.numeric ( (approx( diffseq_mat_c_su[[m]],  
                                                                   normal_mat_c_su[[m]]  * multiplier_hist_mat_c_su[[m]][ which.min(is.na( multiplier_hist_mat_c_su[[m]] ) ) ] ,
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
              
              for (n in 1: sigfeatlen) {
                
                jointprobtemp_test_sig[j,n] <- as.numeric ( (approx( diffseq_mat_c_sig_su[[n]],  
                                                                     normal_mat_c_sig_su[[n]]  *  multiplier_hist_mat_c_sig_su[[n]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[n]] ) ) ] ,
                                                                     Attribute_sig_nonnormal_test[[i]][[j]][[1]][n]) )$y )
                
                
                
              }
              
              
              jointprobtemp_test[j,31] [is.na(jointprobtemp_test[j,31] )] <- bufsig
              
              jointprobtemp_test [is.na(jointprobtemp_test )] <- buf 
              jointprobtemp_test [jointprobtemp_test == 0] <- buf 
              
              jointprobtemp_test_sig [is.na(jointprobtemp_test_sig )] <- bufsig 
              jointprobtemp_test_sig [jointprobtemp_test_sig == 0] <- bufsig
              
              
              jointprobtemp_test_result[j,1] <-   log10(jointprobtemp_test[j,1]) * wimweight_su[1] +  
                log10(jointprobtemp_test[j,2]) * wimweight_su[2] +
                log10(jointprobtemp_test[j,3]) * wimweight_su[3] +
                log10(jointprobtemp_test[j,4]) * wimweight_su[4] +
                log10(jointprobtemp_test[j,5]) * wimweight_su[5] +
                log10(jointprobtemp_test[j,6]) * wimweight_su[6] +
                log10(jointprobtemp_test[j,7]) * wimweight_su[7] +
                log10(jointprobtemp_test[j,13]) * wimweight_su[13] +
                log10(jointprobtemp_test[j,14]) * wimweight_su[14] +
                log10(jointprobtemp_test[j,15]) * wimweight_su[15] +
                log10(jointprobtemp_test[j,16]) * wimweight_su[16] +
                log10(jointprobtemp_test[j,17]) * wimweight_su[17] +
                log10(jointprobtemp_test[j,18]) * wimweight_su[18] +
                log10(jointprobtemp_test[j,19]) * wimweight_su[19] +
                log10(jointprobtemp_test[j,20]) * wimweight_su[20] +
                log10(jointprobtemp_test[j,21]) * wimweight_su[21] +
                log10(jointprobtemp_test[j,30]) * wimweight_su[30] 
              
              jointprobtemp_test_result[j,2] <- log10( jointprobtemp_test[j,31]) * wimweight_su[31] 
              jointprobtemp_test_result[j,3] <- 0
              
              for ( n in 1: sigfeatlen){
                jointprobtemp_test_result[j,3] <-  jointprobtemp_test_result[j,3] +
                  log10(jointprobtemp_test_sig[j,n]) * sigweight_su[n]
              }
              
              
              
              
              jointprobtemp_test_result[j,4] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,2] 
              jointprobtemp_test_result[j,5] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,3] 
              jointprobtemp_test_result[j,6] <-  jointprobtemp_test_result[j,1]  +
                jointprobtemp_test_result[j,2]  +
                jointprobtemp_test_result[j,3] 
              
              
            }
            # normalization
            for (j in 1: length(  Upcandidatesindex_test[[i]]  )  ) {
              maxtemp[1] <- max( jointprobtemp_test_result[,1] )
              mintemp[1] <- min( jointprobtemp_test_result[,1] )
              jointprobtemp_test_result[j,7] <- ( jointprobtemp_test_result[j,1] - max( jointprobtemp_test_result[,1] ) ) / 
                ( max( jointprobtemp_test_result[,1] ) - min( jointprobtemp_test_result[,1] +1) )
              jointprobtemp_test_result[j,8] <- ( jointprobtemp_test_result[j,2] - max( jointprobtemp_test_result[,2] ) ) / 
                ( max( jointprobtemp_test_result[,2] ) - min( jointprobtemp_test_result[,2] +1) )
              jointprobtemp_test_result[j,9] <- ( jointprobtemp_test_result[j,3] - max( jointprobtemp_test_result[,3] ) ) / 
                ( max( jointprobtemp_test_result[,3] ) - min( jointprobtemp_test_result[,3] +1) )
              jointprobtemp_test_result[j,10] <- jointprobtemp_test_result[j,7] * weightwim +  
                jointprobtemp_test_result[j,8] * weightsig1 
              jointprobtemp_test_result[j,11] <- jointprobtemp_test_result[j,7] * weightwim +
                jointprobtemp_test_result[j,9] * weightsig2 
              jointprobtemp_test_result[j,12] <- jointprobtemp_test_result[j,7] * weightwim +
                jointprobtemp_test_result[j,8] * weightsig1 +
                jointprobtemp_test_result[j,9] * weightsig2 
              
              jointprobtemp_test_result[j,13] <-  Upcandidates_test[[i]][j]   
            }
            
            
            
            jointprob_test [[length(jointprob_test) + 1]] <- jointprobtemp_test
            jointprob_test_sig [[length(jointprob_test_sig) + 1 ]] <- jointprobtemp_test_sig
            jointprob_test_result[[length(jointprob_test_result)+1]] <-  jointprobtemp_test_result
            
            jointprobtemp_test_result <- data.frame()
            jointprobtemp_test <- data.frame()
            jointprobtemp_test_sig <- data.frame()
            
            
            idxjointprob_test[i,1] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][7] ) )
            idxjointprob_test[i,2] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][8] ) )
            idxjointprob_test[i,3] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][9] ) )  
            idxjointprob_test[i,4] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][10] ) ) 
            idxjointprob_test[i,5] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][11] ) )  
            idxjointprob_test[i,6] <- which.max(unlist ( jointprob_test_result[[length(jointprob_test_result)]][12] ) ) 
            
            UpFinalcandidates_test[i,1] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,1]] ]  
            UpFinalcandidates_test[i,2] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,2]] ]  
            UpFinalcandidates_test[i,3] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,3]] ]  
            UpFinalcandidates_test[i,4] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,4]] ]  
            UpFinalcandidates_test[i,5] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,5]] ] 
            UpFinalcandidates_test[i,6] <- Upsiglist_test[[i]][ Upcandidatesindex_test[[i]][idxjointprob_test[i,6]] ]  
            
            
          } 
        }
      }
      
      #     else # class is not 9
      #       
      #     {
      #       jointprob_test_result[[length(jointprob_test_result)+1]] <-  NA 
      #       jointprob_test[[length(jointprob_test)+1]] <-  NA 
      #       jointprob_test_sig[[length(jointprob_test_sig)+1]] <-  NA  
      #       idxjointprob_test[i,1] <- NA
      #       idxjointprob_test[i,2] <- NA
      #       idxjointprob_test[i,3] <- NA
      #       idxjointprob_test[i,4] <- NA
      #       idxjointprob_test[i,5] <- NA
      #       idxjointprob_test[i,6] <- NA
      # 
      # 
      #       UpFinalcandidates_test[i,1] <- NA
      #       UpFinalcandidates_test[i,2] <- NA
      #       UpFinalcandidates_test[i,3] <- NA
      #       UpFinalcandidates_test[i,4] <- NA
      #       UpFinalcandidates_test[i,5] <- NA
      #       UpFinalcandidates_test[i,6] <- NA
      #   
      # 
      #     }
    }
    
    
    
    
    
    ### performance 
    #index for class 9
    idxFortt <- which(Downheader_new[,14] >= 8)
    idxForsu <- which(Downheader_new[,14] < 8)
    
    # rm(ResultMisMatching_test, ResultMisMatching_test)
    ResultMisMatching_test_temp  <- data.frame()
    ResultMisMatching_test_temp  <- data.frame()
    
    
    
    
    
    ResultMisMatching_test_temp <- cbind(Target_baseanalysis_Jan0910_table_test[,1], Target_baseanalysis_Jan0910_table_test[,6],
                                         Target_baseanalysis_Jan0910_table_test[,4], 
                                         UpFinalcandidates_test[,1] ,  UpFinalcandidates_test[,2] ,
                                         UpFinalcandidates_test[,3] ,  UpFinalcandidates_test[,4] ,  
                                         UpFinalcandidates_test[,5] , UpFinalcandidates_test[,6] )
    
    ResultMisMatching_test_temp <- cbind(Target_baseanalysis_Jan0910_table_test[,1],Target_baseanalysis_Jan0910_table_test[,6],
                                         Target_baseanalysis_Jan0910_table_test[,4], 
                                         UpFinalcandidates_test[,1] ,  UpFinalcandidates_test[,2] , 
                                         UpFinalcandidates_test[,3] ,  UpFinalcandidates_test[,4] ,
                                         UpFinalcandidates_test[,5] , UpFinalcandidates_test[,6])
    
    ResultMisMatching_test_temp[is.na ( ResultMisMatching_test_temp)]  <- c(999)
    ResultMisMatching_test_temp[is.na ( ResultMisMatching_test_temp)]  <- c(999)
    
    ## TT
    
    ResultMisMatching_test_tt <- subset( ResultMisMatching_test_temp , ResultMisMatching_test_temp[,1] >= 8 )
    ResultMisMatching_test_tt <- subset( ResultMisMatching_test_temp , ResultMisMatching_test_temp[,1] >= 8 )
    
    
    ## SU
    ResultMisMatching_test_su <- subset( ResultMisMatching_test_temp , ResultMisMatching_test_temp[,1] < 8 )
    ResultMisMatching_test_su <- subset( ResultMisMatching_test_temp , ResultMisMatching_test_temp[,1] < 8 )
    
    
    ## ALL
    ResultMisMatching_test_all <- rbind(ResultMisMatching_test_tt , ResultMisMatching_test_su  )
    ResultMisMatching_test_all <- rbind(ResultMisMatching_test_tt , ResultMisMatching_test_su  )
    
    
    # Train
    # ResultMismatching_test_tt <- data.frame()
    # ResultMismatching_test_su <- data.frame()
    # ResultMismatching_test_all <- data.frame()
    
    ## TT
    
    TargetTable_test_tt <- ResultMisMatching_test_tt 
    Target_obj_test_tt  <- ResultMisMatching_test_tt [,2]
    
    missing_obj_test_tt  <- length (Target_obj_test_tt[Target_obj_test_tt == 999]) 
    matching_obj_test_tt <- length (Target_obj_test_tt[Target_obj_test_tt != 999]) 
    
    CVeh_test_tt <- matching_obj_test_tt[1]
    Veh_test_tt <- length(TargetTable_test_tt[,1])
    
    matching_NN_test_tt <- vector()
    MVeh_test_tt  <- vector()
    CMVeh_test_tt  <- vector()
    MMVeh_test_tt  <- vector()
    SIMR_test_tt  <- vector()
    SCMR_test_tt  <- vector()
    SER_test_tt  <- vector()
    
    
    
    for (i in 1: 6) {
      CMVeh_test_tt[i] <- sum ( as.numeric ((ResultMisMatching_test_tt [,2]) == as.numeric (ResultMisMatching_test_tt [,i+3])) &
                                  as.numeric (ResultMisMatching_test_tt [,2]) != 999)
      MVeh_test_tt[i] <- sum(   (as.numeric( TargetTable_test_tt [,i+3])) > 1000 ) 
      
      
      MMVeh_test_tt[i] <- length(  subset(TargetTable_test_tt[,1], as.numeric( Target_obj_test_tt ) 
                                          !=  as.numeric( TargetTable_test_tt[,i+3])   ))  
      SIMR_test_tt[i] <- CMVeh_test_tt[i] / CVeh_test_tt[1]
      SCMR_test_tt[i] <- CMVeh_test_tt[i] / MVeh_test_tt[i]
      SER_test_tt[i] <- MMVeh_test_tt[i] / Veh_test_tt
      
      ResultMismatching_test_tt <- rbind(ResultMismatching_test_tt, c(i, weightwim , weightsig1, weightsig2,
                                                                      matching_obj_test_tt[1], missing_obj_test_tt[1],              
                                                                      CMVeh_test_tt[i], CVeh_test_tt[1], MVeh_test_tt[i],
                                                                      SIMR_test_tt[i], SCMR_test_tt[i], MMVeh_test_tt[i], 
                                                                      Veh_test_tt[1], SER_test_tt[i] ))
      
    }
    
    
    ## SU
    
    TargetTable_test_su <- ResultMisMatching_test_su 
    Target_obj_test_su  <- ResultMisMatching_test_su [,2]
    
    missing_obj_test_su  <- length (Target_obj_test_su[Target_obj_test_su == 999]) 
    matching_obj_test_su <- length (Target_obj_test_su[Target_obj_test_su != 999]) 
    
    CVeh_test_su <- matching_obj_test_su[1]
    Veh_test_su <- length(TargetTable_test_su[,1])
    
    matching_NN_test_su <- vector()
    MVeh_test_su  <- vector()
    CMVeh_test_su  <- vector()
    MMVeh_test_su  <- vector()
    SIMR_test_su  <- vector()
    SCMR_test_su  <- vector()
    SER_test_su  <- vector()
    
    
    
    for (i in 1: 6) {
      CMVeh_test_su[i] <- sum ( as.numeric ((ResultMisMatching_test_su [,2]) == as.numeric (ResultMisMatching_test_su [,i+3])) &
                                  as.numeric (ResultMisMatching_test_su [,2]) != 999)
      MVeh_test_su[i] <- sum(   (as.numeric( TargetTable_test_su [,i+3])) > 1000 ) 
      
      
      MMVeh_test_su[i] <- length(  subset(TargetTable_test_su[,1], as.numeric( Target_obj_test_su ) 
                                          !=  as.numeric( TargetTable_test_su[,i+3])   ))  
      SIMR_test_su[i] <- CMVeh_test_su[i] / CVeh_test_su[1]
      SCMR_test_su[i] <- CMVeh_test_su[i] / MVeh_test_su[i]
      SER_test_su[i] <- MMVeh_test_su[i] / Veh_test_su
      
      ResultMismatching_test_su <- rbind(ResultMismatching_test_su, c(i, weightwim , weightsig1, weightsig2,
                                                                      matching_obj_test_su[1], missing_obj_test_su[1],              
                                                                      CMVeh_test_su[i], CVeh_test_su[1], MVeh_test_su[i],
                                                                      SIMR_test_su[i], SCMR_test_su[i], MMVeh_test_su[i], 
                                                                      Veh_test_su[1], SER_test_su[i] ))
      
    }
    
    
    ## ALL
    
    TargetTable_test_all <- rbind( ResultMisMatching_test_tt  , ResultMisMatching_test_su )
    Target_obj_test_all  <- ResultMisMatching_test_all [,2]
    
    missing_obj_test_all  <- length (Target_obj_test_all[Target_obj_test_all == 999]) 
    matching_obj_test_all <- length (Target_obj_test_all[Target_obj_test_all != 999]) 
    
    missing_NN_test_all <- sum ( as.numeric (ResultMisMatching_test_all[,2]) == c(999))
    CVeh_test_all <- matching_obj_test_all[1]
    Veh_test_all <- length(TargetTable_test_all[,1])
    
    matching_NN_test_all <- vector()
    MVeh_test_all  <- vector()
    CMVeh_test_all  <- vector()
    MMVeh_test_all  <- vector()
    SIMR_test_all  <- vector()
    SCMR_test_all  <- vector()
    SER_test_all  <- vector()
    
    
    for (i in 1: 6) {
      CMVeh_test_all[i] <- sum ( as.numeric ((ResultMisMatching_test_all [,2]) == as.numeric (ResultMisMatching_test_all [,i+3])) &
                                   as.numeric (ResultMisMatching_test_all [,2]) != 999)
      MVeh_test_all[i] <- sum(   (as.numeric( TargetTable_test_all [,i+3])) > 1000 ) 
      
      
      MMVeh_test_all[i] <- length(  subset(TargetTable_test_all[,1], as.numeric( Target_obj_test_all ) 
                                           !=  as.numeric( TargetTable_test_all[,i+3])   ))  
      SIMR_test_all[i] <- CMVeh_test_all[i] / CVeh_test_all[1]
      SCMR_test_all[i] <- CMVeh_test_all[i] / MVeh_test_all[i]
      SER_test_all[i] <- MMVeh_test_all[i] / Veh_test_all
      
      ResultMismatching_test_all <- rbind( ResultMismatching_test_all, c(i, weightwim , weightsig1, weightsig2,
                                                                         matching_obj_test_all[1], missing_obj_test_all[1],              
                                                                         CMVeh_test_all[i], CVeh_test_all[1], MVeh_test_all[i],
                                                                         SIMR_test_all[i], SCMR_test_all[i], MMVeh_test_all[i], 
                                                                         Veh_test_all[1], SER_test_all[i] ))
      
    }
    


# }}}
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_06232015")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Mismatching_06232015")
## end

Upsiglist_train [[ which(Downtarget_attributes_train[,1] ==531357854438662 )  ]]
jointprob_train[[  which(Downtarget_attributes_train[,1] ==531357854438662 )   ]]
# # look more closely
# Upsiglist[[ idxForClass9[[16]]  ]]
# a_magdif [[ idxForClass9[[16]]  ]]
jointprob_test [[ idxForClass9[[12]]  ]]
# jointprob_train[[16]]
# ResultMismatching_test
# ResultMismatching_train



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



View(rbind(jointprob_train_sig[[34]][4,] , jointprob_train_sig[[34]][9,]  ))

