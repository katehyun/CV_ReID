rm(list=ls())

# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_04292015")
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_06232015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif_06232015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_06232015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_NN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/SOLCFHWAClass.RData" )
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_06232015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Target_baseanalysis_Jan0910_table.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_su.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_tt.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_tt.RData")

## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all, sub_all_train, sub_all_test, sub_matching_train, sub_matching_test, 
   sub_nonmatching_train, sub_nonmatching_test)
options(scipen=999) # non scientific notation

library(MASS)
library(plyr)
library(ggplot2)
library(stringr)
library(reshape2)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') 
library(rJava)
library(FSelector)

#WHAT TO CHAGE
utcbd <- 1357804800000


### kernal estimation based on NN


# USING NON-NORMALIZED data
# Downheader_new <- Downheader_new_nonN
# Upheader_new <- Upheader_new_nonN


Class <- sort(unique( Downheader_new[,14]))

# TT unit
tractortrailerunit <- which (Class>=8)

sub_tt <- data.frame()
for (i in tractortrailerunit){ 
  if (length(TargetTable_NN[[i]]) == 29 ){
    sub_tt <-  rbind(sub_tt, c(TargetTable_NN[[i]][,1:4],TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
  }
  else{
  sub_tt <-  rbind(sub_tt, cbind(TargetTable_NN[[i]][,1:4],TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
  }
}



sub_tt <- cbind( sub_tt,      
                  Downheader_new[ match(sub_tt[,4], as.numeric(Downheader_new[,13])),13:44] ,
                  Downheader_new[ match(sub_tt[,4], as.numeric(Downheader_new[,13])),7] ,
                  Upheader_new[ match( sub_tt[,6], as.numeric(Upheader_new[,13])),13:44] , # should be 6?
                  Upheader_new[ match( sub_tt[,6], as.numeric(Upheader_new[,13])),7] )
sub_tt  <- na.omit(sub_tt)




colnames(sub_tt)[5:6] <- c("objup", "upsig")
colnames(sub_tt)[7:39] <- c( "downsig", "class", "numax", "utc", "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt",  "duration")

colnames(sub_tt)[40:72] <- c("upsig", "class", "numax", "utc", "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp",
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt",  "duration")


# SU unit
singleunit <- which(Class < 8)
sub_su <- data.frame()
for (i in singleunit){
  sub_su <-  rbind(sub_su, cbind (TargetTable_NN[[i]][,1:4] ,TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
}



sub_su <- cbind( sub_su,      
                 Downheader_new[ match(sub_su[,4], as.numeric(Downheader_new[,13])),13:44] ,
                 Downheader_new[ match(sub_su[,4], as.numeric(Downheader_new[,13])),7] ,
                 Upheader_new[ match( sub_su[,6], as.numeric(Upheader_new[,13])),13:44] , # should be 6?
                 Upheader_new[ match( sub_su[,6], as.numeric(Upheader_new[,13])),7] )
sub_su  <- na.omit(sub_su)




colnames(sub_su)[5:6] <- c("objup", "upsig")
colnames(sub_su)[7:39] <- c( "downsig", "class", "numax", "utc", "length", "gvw", 
                             "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                             "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                             "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                             "ax9lwt", "ax9rwt",  "duration")

colnames(sub_su)[40:72] <- c("upsig", "class", "numax", "utc", "length", "gvw", 
                             "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp",
                             "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                             "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                             "ax9lwt", "ax9rwt",  "duration")

# # remove outliers
# 
# median_train_tt_down <- vector()
# median_train_su_down <- vector()
# median_train_tt_up <- vector()
# median_train_su_up <- vector()
# 
# for ( i in 1:30 )
# {
#   median_train_tt_down[i] <-  median(na.omit (sub_tt[,i+9]) )
#   median_train_su_down[i] <-  median(na.omit (sub_su[,i+9]) )
#  
# }
# 
# for ( i in 1:30 )
# {
#   median_train_tt_up[i] <-  median(na.omit (sub_tt[,i+42]) )
#   median_train_su_up[i] <-  median(na.omit (sub_su[,i+42]) )
#   
# }
# 
# 
# mzscores_tt_down <-  data.frame()
# mzscores_su_down <-  data.frame()
# mzscores_tt_up <-  data.frame()
# mzscores_su_up <-  data.frame()
# 
# for ( i in 1:30 ){
#   for (j in 1: length(sub_tt[,1])){
#     
#     mzscores_tt_down[j,i] <- 0.6745 * ( sub_tt[j,i+9] - median_train_tt_down[i]  ) /
#       ( median ( abs( sub_tt[,i+9] - median_train_tt_down[i] ) ) )
#     mzscores_tt_up[j,i] <- 0.6745 * ( sub_tt[j,i+42] - median_train_tt_up[i]  ) /
#       ( median ( abs( sub_tt[,i+9] - median_train_tt_up[i] ) ) )
# 
#   }
# }
# 
# for ( i in 1:30 ){
#   for (j in 1: length(sub_su[,1])){
#     
#     mzscores_su_down[j,i] <- 0.6745 * ( sub_su[j,i+9] - median_train_su_down[i]  ) /
#       ( median ( abs( sub_su[,i+9] - median_train_su_down[i] ) ) )
#     mzscores_su_up[j,i] <- 0.6745 * ( sub_su[j,i+42] - median_train_su_up[i]  ) /
#       ( median ( abs( sub_su[,i+9] - median_train_su_up[i] ) ) )
#     
#   }
# }
# 
# 
# 
# 
# mzscoresIndex_tt_down <- list()
# mzscoresIndex_su_down <- list()
# mzscoresIndex_tt_up <- list()
# mzscoresIndex_su_up <- list()
# mzscoresIndex_tt <- list()
# mzscoresIndex_su <- list()
# 
# 
# for ( i in 1:30 ){
#   if ( !is.nan (mzscores_tt_up[1,i])) {
#     mzscoresIndex_tt_down[[i]] <- which (  mzscores_tt_down[,i] < 3.5 &  mzscores_tt_down[,i]  > -3.5  ) 
#     mzscoresIndex_tt_up[[i]] <-  which (  mzscores_tt_up[,i] < 3.5 &  mzscores_tt_up[,i]  > -3.5  ) 
#   }
#   else {
#     mzscoresIndex_tt_down[[i]] <- 999
#     mzscoresIndex_tt_up[[i]] <-999
#   }
#   
#   mzscoresIndex_tt[[i]] <- intersect(mzscoresIndex_tt_down[[i]] , mzscoresIndex_tt_up[[i]]   )
# }
# 
# 
# for ( i in 1:30 ){
#   if ( !is.nan (mzscores_su_down[,i])) {
#     mzscoresIndex_su_down[[i]] <- which (  mzscores_su_down[,i] < 3.5 &  mzscores_su_down[,i]  > -3.5  ) 
#     mzscoresIndex_su_up[[i]] <-  which (  mzscores_su_up[,i] < 3.5 &  mzscores_su_up[,i]  > -3.5  ) 
#   }
#   else {
#     mzscoresIndex_su_down[[i]] <- 999
#     mzscoresIndex_su_up[[i]] <-999
#   }
#   mzscoresIndex_su[[i]] <- intersect(mzscoresIndex_su_down[[i]] , mzscoresIndex_su_up[[i]]   )
# }
#  
# 
# 
# 
# # collect only clean data - Non-normalized data
# ## TT unit
# Diff_tt <- list()
# Diff_su <- list()
# 
# for ( i in 1:30){
#   Diff_tt[[i]] <-  sub_tt[unlist(mzscoresIndex_tt[[i]]),i+9]  - sub_tt[unlist(mzscoresIndex_tt[[i]]),i+42] 
#   Diff_su[[i]] <-  sub_tt[unlist(mzscoresIndex_su[[i]]),i+9]  - sub_tt[unlist(mzscoresIndex_su[[i]]),i+42] 
# }
# 
# 


# WIM data calibration
# get difference

# TT Unit
matchingonly <- data.frame()
matchingonly <- subset(sub_tt, sub_tt$objup == sub_tt$upsig )
matching_highest20_a_magdif <- sort(matchingonly$min_a_magdif)[1:(length(matchingonly)/2)]  # only highest 50%
matching_highest20_wim <- subset( matchingonly, matchingonly$min_a_magdif < max (matching_highest20_a_magdif)  )
matching_highest20_wim_diff <- cbind( (matching_highest20_wim $length - matching_highest20_wim $length.1 ), 
                                      (matching_highest20_wim $gvw - matching_highest20_wim $gvw.1 ),  
                                      (matching_highest20_wim $ax12sp - matching_highest20_wim $ax12sp.1 ),
                                      (matching_highest20_wim $ax23sp - matching_highest20_wim $ax23sp.1 ),
                                      (matching_highest20_wim $ax34sp - matching_highest20_wim $ax34sp.1 ),
                                      (matching_highest20_wim $ax45sp - matching_highest20_wim $ax45sp.1 ),
                                      (matching_highest20_wim $ax56sp - matching_highest20_wim $ax56sp.1 ),
                                      (matching_highest20_wim $ax67sp - matching_highest20_wim $ax67sp.1 ),
                                      (matching_highest20_wim $ax78sp - matching_highest20_wim $ax78sp.1 ),
                                      (matching_highest20_wim $ax89sp - matching_highest20_wim $ax89sp.1 ),
                                      (matching_highest20_wim $ax1lwt - matching_highest20_wim $ax1lwt.1 ),
                                      (matching_highest20_wim $ax1rwt - matching_highest20_wim $ax1rwt.1 ),
                                      (matching_highest20_wim $ax2lwt - matching_highest20_wim $ax2lwt.1 ),
                                      (matching_highest20_wim $ax2rwt - matching_highest20_wim $ax2rwt.1 ),
                                      (matching_highest20_wim $ax3lwt - matching_highest20_wim $ax3lwt.1 ),
                                      (matching_highest20_wim $ax3rwt - matching_highest20_wim $ax3rwt.1 ),
                                      (matching_highest20_wim $ax4lwt - matching_highest20_wim $ax4lwt.1 ),
                                      (matching_highest20_wim $ax4rwt - matching_highest20_wim $ax4rwt.1 ),
                                      (matching_highest20_wim $ax5lwt - matching_highest20_wim $ax5lwt.1 ),
                                      (matching_highest20_wim $ax5rwt - matching_highest20_wim $ax5rwt.1 ),
                                      (matching_highest20_wim $ax6lwt - matching_highest20_wim $ax6lwt.1 ),
                                      (matching_highest20_wim $ax6rwt - matching_highest20_wim $ax6rwt.1 ),
                                      (matching_highest20_wim $ax7lwt - matching_highest20_wim $ax7lwt.1 ),
                                      (matching_highest20_wim $ax7rwt - matching_highest20_wim $ax7rwt.1 ),
                                      (matching_highest20_wim $ax8lwt - matching_highest20_wim $ax8lwt.1 ),
                                      (matching_highest20_wim $ax8rwt - matching_highest20_wim $ax8rwt.1 ),
                                      (matching_highest20_wim $ax9lwt - matching_highest20_wim $ax9lwt.1 ),
                                      (matching_highest20_wim $ax9rwt - matching_highest20_wim $ax9rwt.1 )                         
)

matching_highest20_wim_diff_median_tt <- data.frame()
matching_highest20_wim_diff_median_tt <- t( apply(matching_highest20_wim_diff, 2, FUN = median)  )
matching_highest20_wim_diff_median_tt <- round(matching_highest20_wim_diff_median_tt, digits = 1)
colnames(matching_highest20_wim_diff_median_tt)[1:28] <- c( "length", "gvw", 
                                                         "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                                                         "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                                                         "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                                                         "ax9lwt", "ax9rwt")




# SU Unit

matchingonly <- data.frame()
matchingonly <- subset(sub_su, sub_su$objup == sub_su$upsig )
matching_highest20_a_magdif <- sort(matchingonly$min_a_magdif)[1:(length(matchingonly)/2)]  # only highest 50%
matching_highest20_wim<- subset( matchingonly, matchingonly$min_a_magdif < max (matching_highest20_a_magdif)  )
matching_highest20_wim_diff <- cbind( (matching_highest20_wim$length - matching_highest20_wim $length.1 ), 
                                      (matching_highest20_wim $gvw - matching_highest20_wim $gvw.1 ),  
                                      (matching_highest20_wim $ax12sp - matching_highest20_wim $ax12sp.1 ),
                                      (matching_highest20_wim $ax23sp - matching_highest20_wim $ax23sp.1 ),
                                      (matching_highest20_wim $ax34sp - matching_highest20_wim $ax34sp.1 ),
                                      (matching_highest20_wim $ax45sp - matching_highest20_wim $ax45sp.1 ),
                                      (matching_highest20_wim $ax56sp - matching_highest20_wim $ax56sp.1 ),
                                      (matching_highest20_wim $ax67sp - matching_highest20_wim $ax67sp.1 ),
                                      (matching_highest20_wim $ax78sp - matching_highest20_wim $ax78sp.1 ),
                                      (matching_highest20_wim $ax89sp - matching_highest20_wim $ax89sp.1 ),
                                      (matching_highest20_wim $ax1lwt - matching_highest20_wim $ax1lwt.1 ),
                                      (matching_highest20_wim $ax1rwt - matching_highest20_wim $ax1rwt.1 ),
                                      (matching_highest20_wim $ax2lwt - matching_highest20_wim $ax2lwt.1 ),
                                      (matching_highest20_wim $ax2rwt - matching_highest20_wim $ax2rwt.1 ),
                                      (matching_highest20_wim $ax3lwt - matching_highest20_wim $ax3lwt.1 ),
                                      (matching_highest20_wim $ax3rwt - matching_highest20_wim $ax3rwt.1 ),
                                      (matching_highest20_wim $ax4lwt - matching_highest20_wim $ax4lwt.1 ),
                                      (matching_highest20_wim $ax4rwt - matching_highest20_wim $ax4rwt.1 ),
                                      (matching_highest20_wim $ax5lwt - matching_highest20_wim $ax5lwt.1 ),
                                      (matching_highest20_wim $ax5rwt - matching_highest20_wim $ax5rwt.1 ),
                                      (matching_highest20_wim $ax6lwt - matching_highest20_wim $ax6lwt.1 ),
                                      (matching_highest20_wim $ax6rwt - matching_highest20_wim $ax6rwt.1 ),
                                      (matching_highest20_wim $ax7lwt - matching_highest20_wim $ax7lwt.1 ),
                                      (matching_highest20_wim $ax7rwt - matching_highest20_wim $ax7rwt.1 ),
                                      (matching_highest20_wim $ax8lwt - matching_highest20_wim $ax8lwt.1 ),
                                      (matching_highest20_wim $ax8rwt - matching_highest20_wim $ax8rwt.1 ),
                                      (matching_highest20_wim $ax9lwt - matching_highest20_wim $ax9lwt.1 ),
                                      (matching_highest20_wim $ax9rwt - matching_highest20_wim $ax9rwt.1 )                         
)

matching_highest20_wim_diff_median_su <- data.frame()
matching_highest20_wim_diff_median_su <- t( apply(matching_highest20_wim_diff, 2, FUN = median)  )
matching_highest20_wim_diff_median_su <- round(matching_highest20_wim_diff_median_su, digits = 1)
colnames(matching_highest20_wim_diff_median_su)[1:28] <- c( "length", "gvw", 
                                                         "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                                                         "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                                                         "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                                                         "ax9lwt", "ax9rwt")





# calibrate wim (Up)
Upheader_new_cl <-  Upheader_new
for (i in 1: length( Upheader_new[,1])){
  for (j in 1: length(matching_highest20_wim_diff_median_tt)){
    
    if ( Upheader_new_cl[i,14] < 8) {
      Upheader_new_cl[i,j+16] <- Upheader_new_cl [i,j+16] + matching_highest20_wim_diff_median_su[1,j]
    }
    else{
      Upheader_new_cl[i,j+16] <- Upheader_new_cl [i,j+16] + matching_highest20_wim_diff_median_tt[1,j]
    }
  }
}


Upheader_new <- Upheader_new_cl

# reseting the sub_all with calibrated wim data


sub_tt <- data.frame()
for (i in tractortrailerunit){ 
  if (length(TargetTable_NN[[i]]) == 29 ){
    sub_tt <-  rbind(sub_tt, c(TargetTable_NN[[i]][,1:4],TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
  }
  else{
    sub_tt <-  rbind(sub_tt, cbind(TargetTable_NN[[i]][,1:4],TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
  }
}


sub_tt <- cbind( sub_tt,      
                 Downheader_new[ match(sub_tt[,4], as.numeric(Downheader_new[,13])),13:44] ,
                 Downheader_new[ match(sub_tt[,4], as.numeric(Downheader_new[,13])),7] ,
                 Upheader_new[ match( sub_tt[,6], as.numeric(Upheader_new[,13])),13:44] , 
                 Upheader_new[ match( sub_tt[,6], as.numeric(Upheader_new[,13])),7] )


sub_su <- data.frame()
for (i in singleunit){
  sub_su <-  rbind(sub_su, cbind (TargetTable_NN[[i]][,1:4] ,TargetTable_NN[[i]][,6], TargetTable_NN[[i]][,8]))
}



sub_su <- cbind( sub_su,      
                 Downheader_new[ match(sub_su[,4], as.numeric(Downheader_new[,13])),13:44] ,
                 Downheader_new[ match(sub_su[,4], as.numeric(Downheader_new[,13])),7] ,
                 Upheader_new[ match( sub_su[,6], as.numeric(Upheader_new[,13])),13:44] , 
                 Upheader_new[ match( sub_su[,6], as.numeric(Upheader_new[,13])),7] )


# install.packages("stringr")
# signature feature 

idx_magdif <- lapply(a_magdif,which.min)

sig_selected <- list()
for (i in 1:length(a_magdif)){ 
  a <- unlist(idx_magdif[i])
  sig_selected[i] <- sigfeature[[i]][a]  
}


# train (01/09)
DownheaderTrainIdx <- which (Downheader_new[,12] > utcbd )
DownheaderTestIdx <- which (Downheader_new[,12] < utcbd )
DownheaderTrainttIdx <-  which (Downheader_new[,12] > utcbd  &  Downheader_new[,14] >= 8)
DownheaderTestttIdx <-  which (Downheader_new[,12] < utcbd  &  Downheader_new[,14] >= 8)

DownheaderTrainsuIdx <-  which (Downheader_new[,12] > utcbd  &  Downheader_new[,14] < 8)
DownheaderTestsuIdx <-  which (Downheader_new[,12] < utcbd  &  Downheader_new[,14] < 8)

Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]
sig_selected_train <- sig_selected[DownheaderTrainIdx]
sig_selected_test <- sig_selected[DownheaderTestIdx]
sig_selected_train_tt <- sig_selected[DownheaderTrainttIdx]
sig_selected_test_tt <- sig_selected[DownheaderTestttIdx]
sig_selected_train_su <- sig_selected[DownheaderTrainsuIdx]
sig_selected_test_su <- sig_selected[DownheaderTestsuIdx]
a_magdif_train <- a_magdif[DownheaderTrainIdx]
a_magdif_test <- a_magdif[DownheaderTestIdx]

sig_train_matching_tt <-  which ( as.numeric (str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd   &
                                   Target_baseanalysis_Jan0910_table[,1] >= 8 &
                                   as.numeric ( Target_baseanalysis_Jan0910_table [,6]) == as.numeric( Target_baseanalysis_Jan0910_table [,8])  &
                                   as.numeric (Target_baseanalysis_Jan0910_table  [,6]) != 999)

sig_train_nonmatching_tt <-  which ( as.numeric (str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd   &
                                    Target_baseanalysis_Jan0910_table[,1] >= 8 &
                                    (as.numeric ( Target_baseanalysis_Jan0910_table [,6]) != as.numeric( Target_baseanalysis_Jan0910_table [,8]) ))
                                  


sig_train_matching_su <-  which ( as.numeric (str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd   &
                                    Target_baseanalysis_Jan0910_table[,1] < 8 &
                                    as.numeric ( Target_baseanalysis_Jan0910_table [,6]) == as.numeric( Target_baseanalysis_Jan0910_table [,8])  &
                                    as.numeric (Target_baseanalysis_Jan0910_table  [,6]) != 999)

sig_train_nonmatching_su <-  which ( as.numeric (str_sub (Target_baseanalysis_Jan0910_table[,4],-13,-1) ) > utcbd   &
                                       Target_baseanalysis_Jan0910_table[,1] < 8 &
                                       (as.numeric ( Target_baseanalysis_Jan0910_table [,6]) != as.numeric( Target_baseanalysis_Jan0910_table [,8]) ))




sigfeature_train <- sigfeature[DownheaderTrainIdx]
sigfeature_test <- sigfeature[DownheaderTestIdx]



# TT unit
# train set 
sub_train_tt <- subset(sub_tt, as.numeric (str_sub (sub_tt [,4],-13,-1) ) > utcbd    ) 
sub_matching_train_tt <- subset(sub_train_tt  , as.numeric (sub_train_tt  [,5]) == as.numeric(sub_train_tt  [,6]) &
                               as.numeric (sub_train_tt [,5]) != 999)
sub_nonmatching_train_tt <- subset(sub_train_tt , as.numeric (sub_train_tt [,5]) != as.numeric( sub_train_tt [,6]) )




# train index
subtrainIdx_tt <- which ( as.numeric (str_sub (sub_tt [,4],-13,-1) ) > utcbd  )
submatching_train_Idx_tt <- which (  as.numeric (sub_train_tt  [,5]) == as.numeric(sub_train_tt  [,6]) &
                                    as.numeric (sub_train_tt [,5]) != 999 )
subnonmatching_train_Idx_tt <- which ( as.numeric (sub_train_tt [,5]) != as.numeric( sub_train_tt [,6]) )



# test set
sub_test_tt  <- subset(sub_tt, as.numeric (str_sub (sub_tt [,4],-13,-1) ) <= utcbd     ) 
sub_matching_test_tt <- subset(sub_test_tt  , as.numeric (sub_test_tt  [,5]) == as.numeric(sub_test_tt  [,6]) &
                              as.numeric (sub_test_tt [,5]) != 999)
sub_nonmatching_test_tt <- subset(sub_test_tt , as.numeric (sub_test_tt [,5]) != as.numeric( sub_test_tt [,6]) )

# test index
suballtestIdx_tt <- which ( as.numeric (str_sub (sub_tt [,4],-13,-1) ) <= utcbd     )
submatching_test_Idx_tt <- which ( as.numeric (sub_test_tt  [,5]) == as.numeric(sub_test_tt  [,6]) &
                                  as.numeric (sub_test_tt [,5]) != 999)
subnonmatching_test_Idx_tt <- which ( as.numeric (sub_test_tt [,5]) != as.numeric( sub_test_tt [,6]) )




# SU unit
# train set 
sub_train_su <- subset(sub_tt, as.numeric (str_sub (sub_su [,4],-13,-1) ) > utcbd    ) 
sub_matching_train_su <- subset(sub_train_su  , as.numeric (sub_train_su  [,5]) == as.numeric(sub_train_su  [,6]) &
                                  as.numeric (sub_train_su [,5]) != 999)
sub_nonmatching_train_su <- subset(sub_train_su , as.numeric (sub_train_su [,5]) != as.numeric( sub_train_su [,6]) )




# train index
subtrainIdx_su <- which ( as.numeric (str_sub (sub_su [,4],-13,-1) ) > utcbd  )
submatching_train_Idx_su <- which (  as.numeric (sub_train_su  [,5]) == as.numeric(sub_train_su  [,6]) &
                                       as.numeric (sub_train_su [,5]) != 999 )
subnonmatching_train_Idx_su <- which ( as.numeric (sub_train_su [,5]) != as.numeric( sub_train_su [,6]) )



# test set
sub_test_su  <- subset(sub_su, as.numeric (str_sub (sub_su [,4],-13,-1) ) <= utcbd     ) 
sub_matching_test_su <- subset(sub_test_su  , as.numeric (sub_test_su  [,5]) == as.numeric(sub_test_su  [,6]) &
                                 as.numeric (sub_test_su [,5]) != 999)
sub_nonmatching_test_su <- subset(sub_test_su , as.numeric (sub_test_su [,5]) != as.numeric( sub_test_su [,6]) )

# test index
suballtestIdx_su <- which ( as.numeric (str_sub (sub_su [,4],-13,-1) ) <= utcbd     )
submatching_test_Idx_su <- which ( as.numeric (sub_test_su  [,5]) == as.numeric(sub_test_su  [,6]) &
                                     as.numeric (sub_test_su [,5]) != 999)
subnonmatching_test_Idx_su <- which ( as.numeric (sub_test_su [,5]) != as.numeric( sub_test_su [,6]) )



# Difference (train, test) 
## TT Unit
Diff_mat_train_tt <- list()
Diff_nonmat_train_tt <- list()
Diff_mat_test_tt <- list()
Diff_nonmat_test_tt <- list()

for ( i in 1:30 )
{
  Diff_mat_train_tt[[i]] <- na.omit ( (sub_matching_train_tt [,9+i] - sub_matching_train_tt[,42+i]) ) # no abs
  Diff_nonmat_train_tt[[i]] <- na.omit ( (sub_nonmatching_train_tt [,9+i] - sub_nonmatching_train_tt[,42+i]) )  
}

Diff_mat_train_tt[[31]] <- na.omit (  (sub_matching_train_tt [,3]  )) 
Diff_nonmat_train_tt[[31]] <- na.omit ( (sub_nonmatching_train_tt [,3]  )) 

for ( i in 1:30 )
{
  Diff_mat_test_tt[[i]] <- na.omit ( (sub_matching_test_tt [,9+i] - sub_matching_test_tt[,42+i]) ) # no abs
  Diff_nonmat_test_tt[[i]] <- na.omit ( (sub_nonmatching_test_tt [,9+i] - sub_nonmatching_test_tt[,42+i]) )  
}

Diff_mat_test_tt[[31]] <- na.omit (  (sub_matching_test_tt [,3]  )) 
Diff_nonmat_test_tt[[31]] <- na.omit ( (sub_nonmatching_test_tt [,3]  )) 

# signature


sig_mat_train_tt   <- sig_selected[sig_train_matching_tt]
sig_nonmat_train_tt <- sig_selected[sig_train_nonmatching_tt]


Diff_sig_mat_train_tt <- list()
Diff_sig_nonmat_train_tt <- list()



for ( i in 1: length( sig_mat_train_tt [[1]] ) ) {
  Diff_sig_mat_train_tt[[i]] <- do.call(rbind, sig_mat_train_tt)[,i]
}

for ( i in 1: length( sig_nonmat_train_tt [[1]] ) ) {
  Diff_sig_nonmat_train_tt[[i]] <- do.call(rbind, sig_nonmat_train_tt)[,i]
}




## SU unit
Diff_mat_train_su <- list()
Diff_nonmat_train_su <- list()
Diff_mat_test_su <- list()
Diff_nonmat_test_su <- list()

for ( i in 1:30 )
{
  Diff_mat_train_su[[i]] <- na.omit ( (sub_matching_train_su [,9+i] - sub_matching_train_su[,42+i]) ) # no abs
  Diff_nonmat_train_su[[i]] <- na.omit ( (sub_nonmatching_train_su [,9+i] - sub_nonmatching_train_su[,42+i]) )  
}

Diff_mat_train_su[[31]] <- na.omit (  (sub_matching_train_su [,3]  )) 
Diff_nonmat_train_su[[31]] <- na.omit ( (sub_nonmatching_train_su [,3]  )) 

for ( i in 1:30 )
{
  Diff_mat_test_su[[i]] <- na.omit ( (sub_matching_test_su [,9+i] - sub_matching_test_su[,42+i]) ) # no abs
  Diff_nonmat_test_su[[i]] <- na.omit ( (sub_nonmatching_test_su [,9+i] - sub_nonmatching_test_su[,42+i]) )  
}

Diff_mat_test_su[[31]] <- na.omit (  (sub_matching_test_su [,3]  )) 
Diff_nonmat_test_su[[31]] <- na.omit ( (sub_nonmatching_test_su [,3]  )) 

# signature


sig_mat_train_su   <- sig_selected[sig_train_matching_su]
sig_nonmat_train_su <- sig_selected[sig_train_nonmatching_su]


Diff_sig_mat_train_su <- list()
Diff_sig_nonmat_train_su <- list()



for ( i in 1: length( sig_mat_train_su [[1]] ) ) {
  Diff_sig_mat_train_su[[i]] <- do.call(rbind, sig_mat_train_su)[,i]
}

for ( i in 1: length( sig_nonmat_train_su [[1]] ) ) {
  Diff_sig_nonmat_train_su[[i]] <- do.call(rbind, sig_nonmat_train_su)[,i]
}



# remove outliers
## TT unit
median_train_mat_tt <- vector()
median_train_nonmat_tt <- vector()

median_train_mat_tt[1] <- median( unlist( Diff_mat_train_tt[[1]] ) )
median_train_nonmat_tt[1] <- median( unlist( Diff_nonmat_train_tt[[1]] ) )

for ( i in 1:30 )
{
  median_train_mat_tt[i+1] <-  median ( unlist( Diff_mat_train_tt[[i+1]] ) )
  median_train_nonmat_tt[i+1] <-  median ( unlist( Diff_nonmat_train_tt[[i+1]] ) )
}


mzscores_mat_tt <- list()
mzscores_nonmat_tt <- list()

mzscores_mat_tt[[1]] <- 0.6745 * ( Diff_mat_train_tt[[1]] - median_train_mat_tt[1]  ) /
  ( median ( abs(  Diff_mat_train_tt[[1]] - median_train_mat_tt[1] )) )

mzscores_nonmat_tt[[1]] <- 0.6745 * ( Diff_nonmat_train_tt[[1]] - median_train_nonmat_tt[1]  ) /
  ( median ( abs(  Diff_nonmat_train_tt[[1]] - median_train_nonmat_tt[1] )) )

for ( i in 1:30 )
{
  if(median ( abs(  Diff_mat_train_tt[[i+1]] - median_train_mat_tt[i+1] )) == 0 ){
    mzscores_mat_tt[[i+1]] <- 0.6745 * ( Diff_mat_train_tt[[i+1]] - median_train_mat_tt[i+1]  ) /
      ( median ( abs(  Diff_mat_train_tt[[i+1]] - median_train_mat_tt[i+1] )) + 0.1 )
    
    mzscores_nonmat_tt[[i+1]] <- 0.6745 * ( Diff_nonmat_train_tt[[i+1]] - median_train_nonmat_tt[i+1]  ) /
      ( median ( abs(  Diff_nonmat_train_tt[[i+1]] - median_train_nonmat_tt[i+1] )) )
  }
  else{
    mzscores_mat_tt[[i+1]] <- 0.6745 * ( Diff_mat_train_tt[[i+1]] - median_train_mat_tt[i+1]  ) /
      ( median ( abs(  Diff_mat_train_tt[[i+1]] - median_train_mat_tt[i+1] )) )
    
    mzscores_nonmat_tt[[i+1]] <- 0.6745 * ( Diff_nonmat_train_tt[[i+1]] - median_train_nonmat_tt[i+1]  ) /
      ( median ( abs(  Diff_nonmat_train_tt[[i+1]] - median_train_nonmat_tt[i+1] )) )
  }
}

mzscoresIndex_mat_tt <- list()
mzscoresIndex_nonmat_tt <- list()

mzscoresIndex_mat_tt[[1]] <- which ( ( unlist (mzscores_mat_tt[[1]]) < 3.5) & (unlist (mzscores_mat_tt[[1]] ) > -3.5  ) ) 
mzscoresIndex_nonmat_tt[[1]] <- which ( ( unlist (mzscores_nonmat_tt[[1]]) < 3.5) & (unlist (mzscores_nonmat_tt[[1]] ) > -3.5  ) )


for ( i in 1:30 )
{
  
  mzscoresIndex_mat_tt[[i+1]] <- 
    which ( ( unlist (mzscores_mat_tt[[i+1]]) < 3.5) & ( unlist (mzscores_mat_tt[[i+1]] ) > -3.5  ) )
  mzscoresIndex_nonmat_tt[[i+1]] <- 
    which ( ( unlist (mzscores_nonmat_tt[[i+1]]) < 3.5) & ( unlist (mzscores_nonmat_tt[[i+1]] ) > -3.5  ) )
  
}

## SU unit
median_train_mat_su <- vector()
median_train_nonmat_su <- vector()

median_train_mat_su[1] <- median( unlist( Diff_mat_train_su[[1]] ) )
median_train_nonmat_su[1] <- median( unlist( Diff_nonmat_train_su[[1]] ) )

for ( i in 1:30 )
{
  median_train_mat_su[i+1] <-  median ( unlist( Diff_mat_train_su[[i+1]] ) )
  median_train_nonmat_su[i+1] <-  median ( unlist( Diff_nonmat_train_su[[i+1]] ) )
}


mzscores_mat_su <- list()
mzscores_nonmat_su <- list()

mzscores_mat_su[[1]] <- 0.6745 * ( Diff_mat_train_su[[1]] - median_train_mat_su[1]  ) /
  ( median ( abs(  Diff_mat_train_su[[1]] - median_train_mat_su[1] )) )

mzscores_nonmat_su[[1]] <- 0.6745 * ( Diff_nonmat_train_su[[1]] - median_train_nonmat_su[1]  ) /
  ( median ( abs(  Diff_nonmat_train_su[[1]] - median_train_nonmat_su[1] )) )

for ( i in 1:30 )
{
  if(  median ( abs(  Diff_mat_train_su[[i+1]] - median_train_mat_su[i+1] )) == 0 ){
    mzscores_mat_su[[i+1]] <- 0.6745 * ( Diff_mat_train_su[[i+1]] - median_train_mat_su[i+1]  ) /
      ( median ( abs(  Diff_mat_train_su[[i+1]] - median_train_mat_su[i+1] ))+0.1 ) 
    mzscores_nonmat_su[[i+1]] <- 0.6745 * ( Diff_nonmat_train_su[[i+1]] - median_train_nonmat_su[i+1]  ) /
      ( median ( abs(  Diff_nonmat_train_su[[i+1]] - median_train_nonmat_su[i+1] )) )
  }
  
  else{
    mzscores_mat_su[[i+1]] <- 0.6745 * ( Diff_mat_train_su[[i+1]] - median_train_mat_su[i+1]  ) /
      ( median ( abs(  Diff_mat_train_su[[i+1]] - median_train_mat_su[i+1] ))) 
    
    mzscores_nonmat_su[[i+1]] <- 0.6745 * ( Diff_nonmat_train_su[[i+1]] - median_train_nonmat_su[i+1]  ) /
      ( median ( abs(  Diff_nonmat_train_su[[i+1]] - median_train_nonmat_su[i+1] )) )
  }
 
}

mzscoresIndex_mat_su <- list()
mzscoresIndex_nonmat_su <- list()

mzscoresIndex_mat_su[[1]] <- which ( ( unlist (mzscores_mat_su[[1]]) < 3.5) & (unlist (mzscores_mat_su[[1]] ) > -3.5  ) ) 
mzscoresIndex_nonmat_su[[1]] <- which ( ( unlist (mzscores_nonmat_su[[1]]) < 3.5) & (unlist (mzscores_nonmat_su[[1]] ) > -3.5  ) )


for ( i in 1:30 )
{
  
  mzscoresIndex_mat_su[[i+1]] <- 
    which ( ( unlist (mzscores_mat_su[[i+1]]) < 3.5) & ( unlist (mzscores_mat_su[[i+1]] ) > -3.5  ) )
  mzscoresIndex_nonmat_su[[i+1]] <- 
    which ( ( unlist (mzscores_nonmat_su[[i+1]]) < 3.5) & ( unlist (mzscores_nonmat_su[[i+1]] ) > -3.5  ) )
  
}


# collect only clean data - Non-normalized data
## TT unit
Diff_mat_train_c_tt <- list()
Diff_nonmat_train_c_tt <- list()

Diff_mat_train_c_tt[[1]] <- Diff_mat_train_tt[[1]][ mzscoresIndex_mat_tt[[1]] ]
Diff_nonmat_train_c_tt[[1]] <- Diff_nonmat_train_tt[[1]][  mzscoresIndex_nonmat_tt[[1]] ]

for ( i in 1:30 )
{ 
  Diff_mat_train_c_tt[[i+1]] <- Diff_mat_train_tt[[i+1]][  mzscoresIndex_mat_tt[[i+1]] ]
  Diff_nonmat_train_c_tt[[i+1]] <- Diff_nonmat_train_tt[[i+1]][ mzscoresIndex_nonmat_tt[[i+1]] ]
}

## SU unit
Diff_mat_train_c_su <- list()
Diff_nonmat_train_c_su <- list()

Diff_mat_train_c_su[[1]] <- Diff_mat_train_su[[1]][ mzscoresIndex_mat_su[[1]] ]
Diff_nonmat_train_c_su[[1]] <- Diff_nonmat_train_su[[1]][  mzscoresIndex_nonmat_su[[1]] ]

for ( i in 1:30 )
{ 
  Diff_mat_train_c_su[[i+1]] <- Diff_mat_train_su[[i+1]][  mzscoresIndex_mat_su[[i+1]] ]
  Diff_nonmat_train_c_su[[i+1]] <- Diff_nonmat_train_su[[i+1]][ mzscoresIndex_nonmat_su[[i+1]] ]
}



# Normalized Difference (train) 
## TT unit
max_train_mat_tt <- vector()
min_train_mat_tt <- vector()
max_train_nonmat_tt <- vector()
min_train_nonmat_tt <- vector()
max_train_mat_tt[1] <- max( unlist( Diff_mat_train_c_tt[[1]] ) )
min_train_mat_tt[1] <- min( unlist( Diff_mat_train_c_tt[[1]] ) )
max_train_nonmat_tt[1] <- max( unlist( Diff_nonmat_train_c_tt[[1]] ) )
min_train_nonmat_tt[1] <- min( unlist( Diff_nonmat_train_c_tt[[1]] ) )


for ( i in 1:30 )
{
  max_train_mat_tt[i+1] <-  max ( unlist( Diff_mat_train_c_tt[[i+1]] ) )
  min_train_mat_tt[i+1] <-  min ( unlist( Diff_mat_train_c_tt[[i+1]] ) )
  max_train_nonmat_tt[i+1] <-  max ( unlist( Diff_nonmat_train_c_tt[[i+1]] ) )
  min_train_nonmat_tt[i+1] <-  min ( unlist( Diff_nonmat_train_c_tt[[i+1]] ) )
}


Diff_mat_train_n_tt <- list()
Diff_nonmat_train_n_tt <- list()
Diff_mat_train_n_tt[[1]] <- na.omit ( ( Diff_mat_train_c_tt[[1]]  - min_train_mat_tt[1] ) / 
                                     ( max_train_mat_tt[1] - min_train_mat_tt[1] )  )
Diff_nonmat_train_n_tt[[1]] <- na.omit (  ( Diff_nonmat_train_c_tt[[1]] - min_train_nonmat_tt[1] ) / 
                                         ( max_train_nonmat_tt[1] - min_train_nonmat_tt[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_n_tt[[i+1]] <- na.omit ( ( Diff_mat_train_c_tt[[1+i]] - min_train_mat_tt[1+i] ) /  
                                         ( max_train_mat_tt[1+i] - min_train_mat_tt[1+i] )  )
  Diff_nonmat_train_n_tt[[i+1]] <- na.omit ( ( Diff_nonmat_train_c_tt[[1+i]] -  min_train_nonmat_tt[1+i] ) /  
                                            ( max_train_nonmat_tt[1+i] - min_train_nonmat_tt[1+i] )  ) 
}

## SU unit
max_train_mat_su <- vector()
min_train_mat_su <- vector()
max_train_nonmat_su <- vector()
min_train_nonmat_su <- vector()
max_train_mat_su[1] <- max( unlist( Diff_mat_train_c_su[[1]] ) )
min_train_mat_su[1] <- min( unlist( Diff_mat_train_c_su[[1]] ) )
max_train_nonmat_su[1] <- max( unlist( Diff_nonmat_train_c_su[[1]] ) )
min_train_nonmat_su[1] <- min( unlist( Diff_nonmat_train_c_su[[1]] ) )


for ( i in 1:30 )
{
  max_train_mat_su[i+1] <-  max ( unlist( Diff_mat_train_c_su[[i+1]] ) )
  min_train_mat_su[i+1] <-  min ( unlist( Diff_mat_train_c_su[[i+1]] ) )
  max_train_nonmat_su[i+1] <-  max ( unlist( Diff_nonmat_train_c_su[[i+1]] ) )
  min_train_nonmat_su[i+1] <-  min ( unlist( Diff_nonmat_train_c_su[[i+1]] ) )
}


Diff_mat_train_n_su <- list()
Diff_nonmat_train_n_su <- list()
Diff_mat_train_n_su[[1]] <- na.omit ( ( Diff_mat_train_c_su[[1]]  - min_train_mat_su[1] ) / 
                                        ( max_train_mat_su[1] - min_train_mat_su[1] )  )
Diff_nonmat_train_n_su[[1]] <- na.omit (  ( Diff_nonmat_train_c_su[[1]] - min_train_nonmat_su[1] ) / 
                                            ( max_train_nonmat_su[1] - min_train_nonmat_su[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_n_su[[i+1]] <- na.omit ( ( Diff_mat_train_c_su[[1+i]] - min_train_mat_su[1+i] ) /  
                                            ( max_train_mat_su[1+i] - min_train_mat_su[1+i] )  )
  Diff_nonmat_train_n_su[[i+1]] <- na.omit ( ( Diff_nonmat_train_c_su[[1+i]] -  min_train_nonmat_su[1+i] ) /  
                                               ( max_train_nonmat_su[1+i] - min_train_nonmat_su[1+i] )  ) 
}


# remove NA
Diff_mat_train_c_tt[[1]] <- na.omit (  Diff_mat_train_c_tt[[1]]  )
Diff_nonmat_train_c_tt[[1]] <- na.omit (  Diff_nonmat_train_c_tt[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_c_tt[[i+1]] <- na.omit (  Diff_mat_train_c_tt[[1+i]]  )
  Diff_nonmat_train_c_tt[[i+1]] <- na.omit (  Diff_nonmat_train_c_tt[[1+i]]  ) 
}



Diff_mat_train_n_tt[[1]] <- na.omit (  Diff_mat_train_n_tt[[1]]  )
Diff_nonmat_train_n_tt[[1]] <- na.omit (  Diff_nonmat_train_n_tt[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_n_tt[[i+1]] <- na.omit (  Diff_mat_train_n_tt[[1+i]]  )
  Diff_nonmat_train_n_tt[[i+1]] <- na.omit (  Diff_nonmat_train_n_tt[[1+i]]  ) 
}



# remove NA
Diff_mat_train_c_su[[1]] <- na.omit (  Diff_mat_train_c_su[[1]]  )
Diff_nonmat_train_c_su[[1]] <- na.omit (  Diff_nonmat_train_c_su[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_c_su[[i+1]] <- na.omit (  Diff_mat_train_c_su[[1+i]]  )
  Diff_nonmat_train_c_su[[i+1]] <- na.omit (  Diff_nonmat_train_c_su[[1+i]]  ) 
}



Diff_mat_train_n_su[[1]] <- na.omit (  Diff_mat_train_n_su[[1]]  )
Diff_nonmat_train_n_su[[1]] <- na.omit (  Diff_nonmat_train_n_su[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_n_su[[i+1]] <- na.omit (  Diff_mat_train_n_su[[1+i]]  )
  Diff_nonmat_train_n_su[[i+1]] <- na.omit (  Diff_nonmat_train_n_su[[1+i]]  ) 
}


###################### kerdel design

# kernel (nonparametric) : non-normalized differences - WIM
# TT unit
kernel_mat_c_tt <- list()
kernel_nonmat_c_tt <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_n_tt[[i]]) != 0) {    
    kernel_mat_c_tt[[i]] <- density(Diff_mat_train_c_tt[[i]], kernel = "epanechnikov", bw=5)   
    kernel_nonmat_c_tt[[i]] <- density(Diff_nonmat_train_c_tt[[i]], kernel = "epanechnikov", bw=5)
  }
  
  else
  {
    kernel_mat_c_tt$x[[i]] <- NA
    kernel_mat_c_tt$y[[i]] <- NA
    kernel_nonmat_c_tt$x[[i]] <-  NA
    kernel_nonmat_c_tt$y[[i]] <-  NA
  }
  
}

# SU unit
kernel_mat_c_su <- list()
kernel_nonmat_c_su <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_n_su[[i]]) != 0) {    
    kernel_mat_c_su[[i]] <- density(Diff_mat_train_c_su[[i]], kernel = "epanechnikov", bw=5)   
    kernel_nonmat_c_su[[i]] <- density(Diff_nonmat_train_c_su[[i]], kernel = "epanechnikov", bw=5)
  }
  
  else
  {
    kernel_mat_c_su$x[[i]] <- NA
    kernel_mat_c_su$y[[i]] <- NA
    kernel_nonmat_c_su$x[[i]] <-  NA
    kernel_nonmat_c_su$y[[i]] <-  NA
  }
  
}

# kernel (nonparametric) : non-normalized differences - SIG
# TT unit
kernel_sig_mat_c_tt <- list()
kernel_sig_nonmat_c_tt <- list()

for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train_tt[[i]]) != 0) {
    
    kernel_sig_mat_c_tt[[i]] <- density(Diff_sig_mat_train_tt[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c_tt[[i]] <- density(Diff_sig_nonmat_train_tt[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c_tt$x[[i]] <- NA
    kernel_sig_mat_c_tt$y[[i]] <- NA
    kernel_sig_nonmat_c_tt$x[[i]] <-  NA
    kernel_sig_nonmat_c_tt$y[[i]] <-  NA
  }
}

# SU unit
kernel_sig_mat_c_su <- list()
kernel_sig_nonmat_c_su <- list()

for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train_su[[i]]) != 0) {
    
    kernel_sig_mat_c_su[[i]] <- density(Diff_sig_mat_train_su[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c_su[[i]] <- density(Diff_sig_nonmat_train_su[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c_su$x[[i]] <- NA
    kernel_sig_mat_c_su$y[[i]] <- NA
    kernel_sig_nonmat_c_su$x[[i]] <-  NA
    kernel_sig_nonmat_c_su$y[[i]] <-  NA
  }
}


# kernel (nonparametric) : normalized differences
# TT unit
kernel_mat_n_tt <- list()
kernel_nonmat_n_tt <- list()


for ( i in 1: 31 )
{
  if ((sum ( Diff_mat_train_n_tt[[i]]) != 0) && (!is.na(sum( Diff_mat_train_n_tt[[i]]) ))){
    
    kernel_mat_n_tt[[i]] <- density(Diff_mat_train_n_tt[[i]], kernel = "epanechnikov")
    kernel_nonmat_n_tt[[i]] <- density(Diff_nonmat_train_n_tt[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_mat_n_tt$x[[i]] <- NA
    kernel_mat_n_tt$y[[i]] <- NA
    kernel_nonmat_n_tt$x[[i]] <-  NA
    kernel_nonmat_n_tt$y[[i]] <-  NA
  }
  
}


# SU unit

kernel_mat_n_su <- list()
kernel_nonmat_n_su <- list()


for ( i in 1: 31 )
{
  if ((sum ( Diff_mat_train_n_su[[i]]) != 0) && (!is.na(sum( Diff_mat_train_n_su[[i]]) ))){
    
    kernel_mat_n_su[[i]] <- density(Diff_mat_train_n_su[[i]], kernel = "epanechnikov")
    kernel_nonmat_n_su[[i]] <- density(Diff_nonmat_train_n_su[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_mat_n_su$x[[i]] <- NA
    kernel_mat_n_su$y[[i]] <- NA
    kernel_nonmat_n_su$x[[i]] <-  NA
    kernel_nonmat_n_su$y[[i]] <-  NA
  }
  
}

q=13
plot(kernel_sig_mat_c_su[[q]]$x, kernel_sig_mat_c_su[[q]]$y, type="l", col="red" )
par(new=TRUE)
plot(kernel_sig_nonmat_c_su[[q]]$x, kernel_sig_nonmat_c_su[[q]]$y , type="l", col="green", add=TRUE )



###histogram with non_normalized data

# TT unit

hist_mat_c_tt <- list()
hist_nonmat_c_tt <- list()
histdensity_c_tt <- list()
multiplier_hist_mat_c_tt <- list()
density_hist_mat_c_tt <- list()
density_smooth_hist_mat_c_tt <- list()

multiplier_hist_nonmat_c_tt <- list()
density_hist_nonmat_c_tt <- list()
density_smooth_hist_nonmat_c_tt <- list()

min_tt <- vector()
max_tt <- vector()

diffseq_mat_c_tt <- list ()   
normal_mat_c_tt <- list()
diffseq_nonmat_c_tt <- list ()   
normal_nonmat_c_tt <- list()

interval  <- c(200000, 1, 1, 0.5, 0.5, 0.5, 0.5,  NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5, 0.5, 0.5,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 1 )

for ( i in 1 : length(Diff_mat_train_c_tt)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min_tt[i] <- round_any (min(Diff_nonmat_train_c_tt[[i]] , Diff_mat_train_c_tt[[i]]) , interval[i], f= floor)
    max_tt[i] <- round_any (max(Diff_nonmat_train_c_tt[[i]] , Diff_mat_train_c_tt[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_c_tt[[i]] <- hist(Diff_mat_train_c_tt[[i]],  breaks=seq(min_tt[i] ,max_tt[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_c_tt[[i]] <- ( hist_mat_c_tt[[i]]$counts / sum( hist_mat_c_tt[[i]]$counts)) / hist_mat_c_tt[[i]]$density
    density_hist_mat_c_tt[[i]] <- density (Diff_mat_train_c_tt[[i]]) 
    density_hist_mat_c_tt[[i]]$y <- density_hist_mat_c_tt[[i]]$y *
      multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]
    hist_mat_c_tt[[i]]$density <-  hist_mat_c_tt[[i]]$counts / sum(hist_mat_c_tt[[i]]$counts )   
    density_smooth_hist_mat_c_tt[[i]] <-  smooth.spline(density_hist_mat_c_tt[[i]]$x, density_hist_mat_c_tt[[i]]$y,spar=0.8)
    density_smooth_hist_mat_c_tt[[i]]$y [density_smooth_hist_mat_c_tt[[i]]$y < 0] <- 0
    
    
    hist_nonmat_c_tt[[i]] <- hist(Diff_nonmat_train_c_tt[[i]],  breaks=seq(min_tt[i],max_tt[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_c_tt[[i]] <- ( hist_nonmat_c_tt[[i]]$counts / sum( hist_nonmat_c_tt[[i]]$counts)) / hist_nonmat_c_tt[[i]]$density
    density_hist_nonmat_c_tt[[i]] <- density (Diff_nonmat_train_c_tt[[i]]) 
    density_hist_nonmat_c_tt[[i]]$y <- density_hist_nonmat_c_tt[[i]]$y *
      multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]
    hist_nonmat_c_tt[[i]]$density <-  hist_nonmat_c_tt[[i]]$counts / sum(hist_nonmat_c_tt[[i]]$counts )
    density_smooth_hist_nonmat_c_tt[[i]] <-  smooth.spline(density_hist_nonmat_c_tt[[i]]$x, density_hist_nonmat_c_tt[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_c_tt[[i]]$y [density_smooth_hist_nonmat_c_tt[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_c_tt[[i]] <- seq(min (Diff_nonmat_train_c_tt[[i]] ) , max( Diff_nonmat_train_c_tt[[i]]) , length.out=100)  
    diffseq_nonmat_c_tt[[i]] <- seq(min (Diff_nonmat_train_c_tt[[i]] ) , max( Diff_nonmat_train_c_tt[[i]]) , length.out=100)  
    normal_mat_c_tt[[i]] <- dnorm (x=diffseq_mat_c_tt[[i]] , mean= mean (Diff_mat_train_c_tt[[i]]) , sd = sd(Diff_mat_train_c_tt[[i]]) )
    normal_nonmat_c_tt[[i]] <- dnorm (x=diffseq_nonmat_c_tt[[i]] , mean= mean (Diff_nonmat_train_c_tt[[i]]) , sd = sd(Diff_nonmat_train_c_tt[[i]]) )
    
    
    #histogram
    histdensity_c_tt[[i]] <- cbind(hist_mat_c_tt[[i]]$mids - interval[i]/2, hist_mat_c_tt[[i]]$mids + interval[i]/2 , 
                                hist_mat_c_tt[[i]]$counts / sum(hist_mat_c_tt[[i]]$counts) ,  
                                hist_nonmat_c_tt[[i]]$counts / sum(hist_nonmat_c_tt[[i]]$counts) ) 
    
    hist_mat_c_tt[[i]]$counts <-  hist_mat_c_tt[[i]]$counts / sum ( hist_mat_c_tt[[i]]$counts)
    hist_nonmat_c_tt[[i]]$counts <-  hist_nonmat_c_tt[[i]]$counts / sum ( hist_nonmat_c_tt[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_c_tt[[i]] <- NA
    hist_nonmat_c_tt[[i]] <- NA
    histdensity_c_tt[[i]] <- NA
  }
  
}


## SU unit



hist_mat_c_su <- list()
hist_nonmat_c_su <- list()
histdensity_c_su <- list()
multiplier_hist_mat_c_su <- list()
density_hist_mat_c_su <- list()
density_smooth_hist_mat_c_su <- list()

multiplier_hist_nonmat_c_su <- list()
density_hist_nonmat_c_su <- list()
density_smooth_hist_nonmat_c_su <- list()

min_su <- vector()
max_su <- vector()

diffseq_mat_c_su <- list ()   
normal_mat_c_su <- list()
diffseq_nonmat_c_su <- list ()   
normal_nonmat_c_su <- list()

interval  <- c(200000, 1, 1, 0.5, 0.5, 0.5, 0.5,  NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5, 0.5, 0.5,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 1 )

for ( i in 1 : length(Diff_mat_train_c_su)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min_su[i] <- round_any (min(Diff_nonmat_train_c_su[[i]] , Diff_mat_train_c_su[[i]]) , interval[i], f= floor)
    max_su[i] <- round_any (max(Diff_nonmat_train_c_su[[i]] , Diff_mat_train_c_su[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_c_su[[i]] <- hist(Diff_mat_train_c_su[[i]],  breaks=seq(min_su[i] ,max_su[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_c_su[[i]] <- ( hist_mat_c_su[[i]]$counts / sum( hist_mat_c_su[[i]]$counts)) / hist_mat_c_su[[i]]$density
    density_hist_mat_c_su[[i]] <- density (Diff_mat_train_c_su[[i]]) 
    density_hist_mat_c_su[[i]]$y <- density_hist_mat_c_su[[i]]$y *
      multiplier_hist_mat_c_su[[i]][ which.min(is.na( multiplier_hist_mat_c_su[[i]] ) ) ]
    hist_mat_c_su[[i]]$density <-  hist_mat_c_su[[i]]$counts / sum(hist_mat_c_su[[i]]$counts )   
    density_smooth_hist_mat_c_su[[i]] <-  smooth.spline(density_hist_mat_c_su[[i]]$x, density_hist_mat_c_su[[i]]$y,spar=0.8)
    density_smooth_hist_mat_c_su[[i]]$y [density_smooth_hist_mat_c_su[[i]]$y < 0] <- 0
    
    
    hist_nonmat_c_su[[i]] <- hist(Diff_nonmat_train_c_su[[i]],  breaks=seq(min_su[i],max_su[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_c_su[[i]] <- ( hist_nonmat_c_su[[i]]$counts / sum( hist_nonmat_c_su[[i]]$counts)) / hist_nonmat_c_su[[i]]$density
    density_hist_nonmat_c_su[[i]] <- density (Diff_nonmat_train_c_su[[i]]) 
    density_hist_nonmat_c_su[[i]]$y <- density_hist_nonmat_c_su[[i]]$y *
      multiplier_hist_nonmat_c_su[[i]][ which.min(is.na( multiplier_hist_nonmat_c_su[[i]] ) ) ]
    hist_nonmat_c_su[[i]]$density <-  hist_nonmat_c_su[[i]]$counts / sum(hist_nonmat_c_su[[i]]$counts )
    density_smooth_hist_nonmat_c_su[[i]] <-  smooth.spline(density_hist_nonmat_c_su[[i]]$x, density_hist_nonmat_c_su[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_c_su[[i]]$y [density_smooth_hist_nonmat_c_su[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_c_su[[i]] <- seq(min (Diff_nonmat_train_c_su[[i]] ) , max( Diff_nonmat_train_c_su[[i]]) , length.out=100)  
    diffseq_nonmat_c_su[[i]] <- seq(min (Diff_nonmat_train_c_su[[i]] ) , max( Diff_nonmat_train_c_su[[i]]) , length.out=100)  
    normal_mat_c_su[[i]] <- dnorm (x=diffseq_mat_c_su[[i]] , mean= mean (Diff_mat_train_c_su[[i]]) , sd = sd(Diff_mat_train_c_su[[i]]) )
    normal_nonmat_c_su[[i]] <- dnorm (x=diffseq_nonmat_c_su[[i]] , mean= mean (Diff_nonmat_train_c_su[[i]]) , sd = sd(Diff_nonmat_train_c_su[[i]]) )
    
    
    #histogram
    histdensity_c_su[[i]] <- cbind(hist_mat_c_su[[i]]$mids - interval[i]/2, hist_mat_c_su[[i]]$mids + interval[i]/2 , 
                                   hist_mat_c_su[[i]]$counts / sum(hist_mat_c_su[[i]]$counts) ,  
                                   hist_nonmat_c_su[[i]]$counts / sum(hist_nonmat_c_su[[i]]$counts) ) 
    
    hist_mat_c_su[[i]]$counts <-  hist_mat_c_su[[i]]$counts / sum ( hist_mat_c_su[[i]]$counts)
    hist_nonmat_c_su[[i]]$counts <-  hist_nonmat_c_su[[i]]$counts / sum ( hist_nonmat_c_su[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_c_su[[i]] <- NA
    hist_nonmat_c_su[[i]] <- NA
    histdensity_c_su[[i]] <- NA
  }
  
}


# histogram and normal kernal - normalized data
## TT unit

hist_mat_n_tt <- list()
hist_nonmat_n_tt  <- list()
histdensity_n_tt  <- list()
multiplier_hist_mat_n_tt <- list()
density_hist_mat_n_tt <- list()
density_smooth_hist_mat_n_tt <- list()

multiplier_hist_nonmat_n_tt <- list()
density_hist_nonmat_n_tt <- list()
density_smooth_hist_nonmat_n_tt <- list()

min <- vector()
max <- vector()

diffseq_mat_n_tt <- list ()   
normal_mat_n_tt <- list()
diffseq_nonmat_n_tt <- list ()   
normal_nonmat_n_tt <- list()



interval  <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,  NA, NA, NA, NA, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,0.1, 0.1, 0.1, 0.1,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.1, 0.1 )

for ( i in 1 : length(Diff_mat_train_n_tt)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min_tt[i] <- round_any (min(Diff_nonmat_train_n_tt[[i]] , Diff_mat_train_n_tt[[i]]) , interval[i], f= floor)
    max_tt[i] <- round_any (max(Diff_nonmat_train_n_tt[[i]] , Diff_mat_train_n_tt[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_n_tt[[i]] <- hist(Diff_mat_train_n_tt[[i]],  breaks=seq(min_tt[i] ,max_tt[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_n_tt[[i]] <- ( hist_mat_n_tt[[i]]$counts / sum( hist_mat_n_tt[[i]]$counts)) / hist_mat_n_tt[[i]]$density
    density_hist_mat_n_tt[[i]] <- density (Diff_mat_train_n_tt[[i]]) 
    density_hist_mat_n_tt[[i]]$y <- density_hist_mat_n_tt[[i]]$y *
      multiplier_hist_mat_n_tt[[i]][ which.min(is.na( multiplier_hist_mat_n_tt[[i]] ) ) ]
    hist_mat_n_tt[[i]]$density <-  hist_mat_n_tt[[i]]$counts / sum(hist_mat_n_tt[[i]]$counts )   
    density_smooth_hist_mat_n_tt[[i]] <-  smooth.spline(density_hist_mat_n_tt[[i]]$x, density_hist_mat_n_tt[[i]]$y,spar=0.8)
    density_smooth_hist_mat_n_tt[[i]]$y [density_smooth_hist_mat_n_tt[[i]]$y < 0] <- 0
    
    
    hist_nonmat_n_tt[[i]] <- hist(Diff_nonmat_train_n_tt[[i]],  breaks=seq(min_tt[i],max_tt[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_n_tt[[i]] <- ( hist_nonmat_n_tt[[i]]$counts / sum( hist_nonmat_n_tt[[i]]$counts)) / hist_nonmat_n_tt[[i]]$density
    density_hist_nonmat_n_tt[[i]] <- density (Diff_nonmat_train_n_tt[[i]]) 
    density_hist_nonmat_n_tt[[i]]$y <- density_hist_nonmat_n_tt[[i]]$y *
      multiplier_hist_nonmat_n_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[i]] ) ) ]
    hist_nonmat_n_tt[[i]]$density <-  hist_nonmat_n_tt[[i]]$counts / sum(hist_nonmat_n_tt[[i]]$counts )
    density_smooth_hist_nonmat_n_tt[[i]] <-  smooth.spline(density_hist_nonmat_n_tt[[i]]$x, density_hist_nonmat_n_tt[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_n_tt[[i]]$y [density_smooth_hist_nonmat_n_tt[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_n_tt[[i]] <- seq(min (Diff_nonmat_train_n_tt[[i]] ) , max( Diff_nonmat_train_n_tt[[i]]) , length.out=100)  
    diffseq_nonmat_n_tt[[i]] <- seq(min (Diff_nonmat_train_n_tt[[i]] ) , max( Diff_nonmat_train_n_tt[[i]]) , length.out=100)  
    normal_mat_n_tt[[i]] <- dnorm (x=diffseq_mat_n_tt[[i]] , mean= mean (Diff_mat_train_n_tt[[i]]) , sd = sd(Diff_mat_train_n_tt[[i]]) )
    normal_nonmat_n_tt[[i]] <- dnorm (x=diffseq_nonmat_n_tt[[i]] , mean= mean (Diff_nonmat_train_n_tt[[i]]) , sd = sd(Diff_nonmat_train_n_tt[[i]]) )
    
    
    #histogram
    histdensity_n_tt[[i]] <- cbind(hist_mat_n_tt[[i]]$mids - interval[i]/2, hist_mat_n_tt[[i]]$mids + interval[i]/2 , 
                                hist_mat_n_tt[[i]]$counts / sum(hist_mat_n_tt[[i]]$counts) ,  
                                hist_nonmat_n_tt[[i]]$counts / sum(hist_nonmat_n_tt[[i]]$counts) ) 
    
    hist_mat_n_tt[[i]]$counts <-  hist_mat_n_tt[[i]]$counts / sum ( hist_mat_n_tt[[i]]$counts)
    hist_nonmat_n_tt[[i]]$counts <-  hist_nonmat_n_tt[[i]]$counts / sum ( hist_nonmat_n_tt[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_n_tt[[i]] <- NA
    hist_nonmat_n_tt[[i]] <- NA
    histdensity_n_tt[[i]] <- NA
  }
  
}

# SU unit


hist_mat_n_su <- list()
hist_nonmat_n_su  <- list()
histdensity_n_su  <- list()
multiplier_hist_mat_n_su <- list()
density_hist_mat_n_su <- list()
density_smooth_hist_mat_n_su <- list()

multiplier_hist_nonmat_n_su <- list()
density_hist_nonmat_n_su <- list()
density_smooth_hist_nonmat_n_su <- list()

min <- vector()
max <- vector()

diffseq_mat_n_su <- list ()   
normal_mat_n_su <- list()
diffseq_nonmat_n_su <- list ()   
normal_nonmat_n_su <- list()



interval  <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,  NA, NA, NA, NA, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,0.1, 0.1, 0.1, 0.1,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.1, 0.1 )

for ( i in 1 : length(Diff_mat_train_n_su)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min_su[i] <- round_any (min(Diff_nonmat_train_n_su[[i]] , Diff_mat_train_n_su[[i]]) , interval[i], f= floor)
    max_su[i] <- round_any (max(Diff_nonmat_train_n_su[[i]] , Diff_mat_train_n_su[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_n_su[[i]] <- hist(Diff_mat_train_n_su[[i]],  breaks=seq(min_su[i] ,max_su[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_n_su[[i]] <- ( hist_mat_n_su[[i]]$counts / sum( hist_mat_n_su[[i]]$counts)) / hist_mat_n_su[[i]]$density
    density_hist_mat_n_su[[i]] <- density (Diff_mat_train_n_su[[i]]) 
    density_hist_mat_n_su[[i]]$y <- density_hist_mat_n_su[[i]]$y *
      multiplier_hist_mat_n_su[[i]][ which.min(is.na( multiplier_hist_mat_n_su[[i]] ) ) ]
    hist_mat_n_su[[i]]$density <-  hist_mat_n_su[[i]]$counts / sum(hist_mat_n_su[[i]]$counts )   
    density_smooth_hist_mat_n_su[[i]] <-  smooth.spline(density_hist_mat_n_su[[i]]$x, density_hist_mat_n_su[[i]]$y,spar=0.8)
    density_smooth_hist_mat_n_su[[i]]$y [density_smooth_hist_mat_n_su[[i]]$y < 0] <- 0
    
    
    hist_nonmat_n_su[[i]] <- hist(Diff_nonmat_train_n_su[[i]],  breaks=seq(min_su[i],max_su[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_n_su[[i]] <- ( hist_nonmat_n_su[[i]]$counts / sum( hist_nonmat_n_su[[i]]$counts)) / hist_nonmat_n_su[[i]]$density
    density_hist_nonmat_n_su[[i]] <- density (Diff_nonmat_train_n_su[[i]]) 
    density_hist_nonmat_n_su[[i]]$y <- density_hist_nonmat_n_su[[i]]$y *
      multiplier_hist_nonmat_n_su[[i]][ which.min(is.na( multiplier_hist_nonmat_n_su[[i]] ) ) ]
    hist_nonmat_n_su[[i]]$density <-  hist_nonmat_n_su[[i]]$counts / sum(hist_nonmat_n_su[[i]]$counts )
    density_smooth_hist_nonmat_n_su[[i]] <-  smooth.spline(density_hist_nonmat_n_su[[i]]$x, density_hist_nonmat_n_su[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_n_su[[i]]$y [density_smooth_hist_nonmat_n_su[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_n_su[[i]] <- seq(min (Diff_nonmat_train_n_su[[i]] ) , max( Diff_nonmat_train_n_su[[i]]) , length.out=100)  
    diffseq_nonmat_n_su[[i]] <- seq(min (Diff_nonmat_train_n_su[[i]] ) , max( Diff_nonmat_train_n_su[[i]]) , length.out=100)  
    normal_mat_n_su[[i]] <- dnorm (x=diffseq_mat_n_su[[i]] , mean= mean (Diff_mat_train_n_su[[i]]) , sd = sd(Diff_mat_train_n_su[[i]]) )
    normal_nonmat_n_su[[i]] <- dnorm (x=diffseq_nonmat_n_su[[i]] , mean= mean (Diff_nonmat_train_n_su[[i]]) , sd = sd(Diff_nonmat_train_n_su[[i]]) )
    
    
    #histogram
    histdensity_n_su[[i]] <- cbind(hist_mat_n_su[[i]]$mids - interval[i]/2, hist_mat_n_su[[i]]$mids + interval[i]/2 , 
                                   hist_mat_n_su[[i]]$counts / sum(hist_mat_n_su[[i]]$counts) ,  
                                   hist_nonmat_n_su[[i]]$counts / sum(hist_nonmat_n_su[[i]]$counts) ) 
    
    hist_mat_n_su[[i]]$counts <-  hist_mat_n_su[[i]]$counts / sum ( hist_mat_n_su[[i]]$counts)
    hist_nonmat_n_su[[i]]$counts <-  hist_nonmat_n_su[[i]]$counts / sum ( hist_nonmat_n_su[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_n_su[[i]] <- NA
    hist_nonmat_n_su[[i]] <- NA
    histdensity_n_su[[i]] <- NA
  }
  
}


# signature

## TT unit

for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train_tt[[i]]) != 0) {
    
    kernel_sig_mat_c_tt[[i]] <- density(Diff_sig_mat_train_tt[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c_tt[[i]] <- density(Diff_sig_nonmat_train_tt[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c_tt$x[[i]] <- NA
    kernel_sig_mat_c_tt$y[[i]] <- NA
    kernel_sig_nonmat_c_tt$x[[i]] <-  NA
    kernel_sig_nonmat_c_tt$y[[i]] <-  NA
  }
}



intervalsig  <- rep(0.01, 50)

min_sig_tt <- vector()
max_sig_tt <- vector()

hist_mat_c_sig_tt <- list()
hist_nonmat_c_sig_tt <- list()
histdensity_c_sig_tt <- list()
multiplier_hist_mat_c_sig_tt <- list()
density_hist_mat_c_sig_tt <- list()
density_smooth_hist_mat_c_sig_tt <- list()

multiplier_hist_nonmat_c_sig_tt <- list()
density_hist_nonmat_c_sig_tt <- list()
density_smooth_hist_nonmat_c_sig_tt <- list()


diffseq_mat_c_sig_tt <- list ()   
normal_mat_c_sig_tt <- list()
diffseq_nonmat_c_sig_tt <- list ()   
normal_nonmat_c_sig_tt <- list()

for ( i in 1 : length(intervalsig )  ){
  
  
  min_sig_tt[i] <- round_any (min( Diff_sig_mat_train_tt[[i]] ,  Diff_sig_nonmat_train_tt[[i]] ) , intervalsig [i], f= floor)
  max_sig_tt[i] <- round_any (max( Diff_sig_mat_train_tt[[i]] ,  Diff_sig_nonmat_train_tt[[i]] ) , intervalsig [i], f= ceiling)
  
  # non parametric
  hist_mat_c_sig_tt[[i]] <- hist(Diff_sig_mat_train_tt[[i]],  breaks=seq(min_sig_tt[i] ,max_sig_tt[i], by=intervalsig [i]), plot=FALSE)
  multiplier_hist_mat_c_sig_tt[[i]] <- ( hist_mat_c_sig_tt[[i]]$counts / sum( hist_mat_c_sig_tt[[i]]$counts)) / hist_mat_c_sig_tt[[i]]$density
  density_hist_mat_c_sig_tt[[i]] <- density (Diff_sig_mat_train_tt[[i]]) 
  density_hist_mat_c_sig_tt[[i]]$y <- density_hist_mat_c_sig_tt[[i]]$y *
    multiplier_hist_mat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[i]] ) ) ]
  hist_mat_c_sig_tt[[i]]$density <-  hist_mat_c_sig_tt[[i]]$counts / sum(hist_mat_c_sig_tt[[i]]$counts )   
  density_smooth_hist_mat_c_sig_tt[[i]] <-  smooth.spline(density_hist_mat_c_sig_tt[[i]]$x, density_hist_mat_c_sig_tt[[i]]$y,spar=0.8)
  density_smooth_hist_mat_c_sig_tt[[i]]$y [density_smooth_hist_mat_c_sig_tt[[i]]$y < 0] <- 0
  
  
  hist_nonmat_c_sig_tt[[i]] <- hist(Diff_sig_nonmat_train_tt[[i]],  breaks=seq(min_sig_tt[i],max_sig_tt[i],by=intervalsig [i]) ,  plot=FALSE)
  multiplier_hist_nonmat_c_sig_tt[[i]] <- ( hist_nonmat_c_sig_tt[[i]]$counts / sum( hist_nonmat_c_sig_tt[[i]]$counts)) / hist_nonmat_c_sig_tt[[i]]$density
  density_hist_nonmat_c_sig_tt[[i]] <- density (Diff_sig_nonmat_train_tt[[i]]) 
  density_hist_nonmat_c_sig_tt[[i]]$y <- density_hist_nonmat_c_sig_tt[[i]]$y *
    multiplier_hist_nonmat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[i]] ) ) ]
  hist_nonmat_c_sig_tt[[i]]$density <-  hist_nonmat_c_sig_tt[[i]]$counts / sum(hist_nonmat_c_sig_tt[[i]]$counts )
  density_smooth_hist_nonmat_c_sig_tt[[i]] <-  smooth.spline(density_hist_nonmat_c_sig_tt[[i]]$x, density_hist_nonmat_c_sig_tt[[i]]$y,spar=0.8)
  density_smooth_hist_nonmat_c_sig_tt[[i]]$y [density_smooth_hist_nonmat_c_sig_tt[[i]]$y < 0] <- 0 
  
  # normal distribution
  diffseq_mat_c_sig_tt[[i]] <- seq(min (Diff_sig_mat_train_tt[[i]] , Diff_sig_mat_train_tt[[i]] ) , max( Diff_sig_nonmat_train_tt[[i]], Diff_sig_mat_train_tt[[i]]) , length.out=100)  
  diffseq_nonmat_c_sig_tt[[i]] <- seq(min (Diff_sig_nonmat_train_tt[[i]], Diff_sig_nonmat_train_tt[[i]] ) , max( Diff_sig_nonmat_train_tt[[i]], Diff_sig_mat_train_tt[[i]]) , length.out=100)  
  normal_mat_c_sig_tt[[i]] <- dnorm (x=diffseq_mat_c_sig_tt[[i]] , mean= mean (Diff_sig_mat_train_tt[[i]]) , sd = sd(Diff_sig_mat_train_tt[[i]]) )
  normal_nonmat_c_sig_tt[[i]] <- dnorm (x=diffseq_nonmat_c_sig_tt[[i]] , mean= mean (Diff_sig_nonmat_train_tt[[i]]) , sd = sd(Diff_sig_nonmat_train_tt[[i]]) )
  
  
  #histogram
  histdensity_c_sig_tt[[i]] <- cbind(hist_mat_c_sig_tt[[i]]$mids - intervalsig [i]/2, hist_mat_c_sig_tt[[i]]$mids + intervalsig [i]/2 , 
                                  hist_mat_c_sig_tt[[i]]$counts / sum(hist_mat_c_sig_tt[[i]]$counts) ,  
                                  hist_nonmat_c_sig_tt[[i]]$counts / sum(hist_nonmat_c_sig_tt[[i]]$counts) ) 
  
  hist_mat_c_sig_tt[[i]]$counts <-  hist_mat_c_sig_tt[[i]]$counts / sum ( hist_mat_c_sig_tt[[i]]$counts)
  hist_nonmat_c_sig_tt[[i]]$counts <-  hist_nonmat_c_sig_tt[[i]]$counts / sum ( hist_nonmat_c_sig_tt[[i]]$counts)
  
  
}


## SU unit


for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train_su[[i]]) != 0) {
    
    kernel_sig_mat_c_su[[i]] <- density(Diff_sig_mat_train_su[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c_su[[i]] <- density(Diff_sig_nonmat_train_su[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c_su$x[[i]] <- NA
    kernel_sig_mat_c_su$y[[i]] <- NA
    kernel_sig_nonmat_c_su$x[[i]] <-  NA
    kernel_sig_nonmat_c_su$y[[i]] <-  NA
  }
}



intervalsig  <- rep(0.01, 50)

min_sig_su <- vector()
max_sig_su <- vector()

hist_mat_c_sig_su <- list()
hist_nonmat_c_sig_su <- list()
histdensity_c_sig_su <- list()
multiplier_hist_mat_c_sig_su <- list()
density_hist_mat_c_sig_su <- list()
density_smooth_hist_mat_c_sig_su <- list()

multiplier_hist_nonmat_c_sig_su <- list()
density_hist_nonmat_c_sig_su <- list()
density_smooth_hist_nonmat_c_sig_su <- list()


diffseq_mat_c_sig_su <- list ()   
normal_mat_c_sig_su <- list()
diffseq_nonmat_c_sig_su <- list ()   
normal_nonmat_c_sig_su <- list()

for ( i in 1 : length(intervalsig )  ){
  
  
  min_sig_su[i] <- round_any (min( Diff_sig_mat_train_su[[i]] ,  Diff_sig_nonmat_train_su[[i]] ) , intervalsig [i], f= floor)
  max_sig_su[i] <- round_any (max( Diff_sig_mat_train_su[[i]] ,  Diff_sig_nonmat_train_su[[i]] ) , intervalsig [i], f= ceiling)
  
  # non parametric
  hist_mat_c_sig_su[[i]] <- hist(Diff_sig_mat_train_su[[i]],  breaks=seq(min_sig_su[i] ,max_sig_su[i], by=intervalsig [i]), plot=FALSE)
  multiplier_hist_mat_c_sig_su[[i]] <- ( hist_mat_c_sig_su[[i]]$counts / sum( hist_mat_c_sig_su[[i]]$counts)) / hist_mat_c_sig_su[[i]]$density
  density_hist_mat_c_sig_su[[i]] <- density (Diff_sig_mat_train_su[[i]]) 
  density_hist_mat_c_sig_su[[i]]$y <- density_hist_mat_c_sig_su[[i]]$y *
    multiplier_hist_mat_c_sig_su[[i]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[i]] ) ) ]
  hist_mat_c_sig_su[[i]]$density <-  hist_mat_c_sig_su[[i]]$counts / sum(hist_mat_c_sig_su[[i]]$counts )   
  density_smooth_hist_mat_c_sig_su[[i]] <-  smooth.spline(density_hist_mat_c_sig_su[[i]]$x, density_hist_mat_c_sig_su[[i]]$y,spar=0.8)
  density_smooth_hist_mat_c_sig_su[[i]]$y [density_smooth_hist_mat_c_sig_su[[i]]$y < 0] <- 0
  
  
  hist_nonmat_c_sig_su[[i]] <- hist(Diff_sig_nonmat_train_su[[i]],  breaks=seq(min_sig_su[i],max_sig_su[i],by=intervalsig [i]) ,  plot=FALSE)
  multiplier_hist_nonmat_c_sig_su[[i]] <- ( hist_nonmat_c_sig_su[[i]]$counts / sum( hist_nonmat_c_sig_su[[i]]$counts)) / hist_nonmat_c_sig_su[[i]]$density
  density_hist_nonmat_c_sig_su[[i]] <- density (Diff_sig_nonmat_train_su[[i]]) 
  density_hist_nonmat_c_sig_su[[i]]$y <- density_hist_nonmat_c_sig_su[[i]]$y *
    multiplier_hist_nonmat_c_sig_su[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig_su[[i]] ) ) ]
  hist_nonmat_c_sig_su[[i]]$density <-  hist_nonmat_c_sig_su[[i]]$counts / sum(hist_nonmat_c_sig_su[[i]]$counts )
  density_smooth_hist_nonmat_c_sig_su[[i]] <-  smooth.spline(density_hist_nonmat_c_sig_su[[i]]$x, density_hist_nonmat_c_sig_su[[i]]$y,spar=0.8)
  density_smooth_hist_nonmat_c_sig_su[[i]]$y [density_smooth_hist_nonmat_c_sig_su[[i]]$y < 0] <- 0 
  
  # normal distribution
  diffseq_mat_c_sig_su[[i]] <- seq(min (Diff_sig_mat_train_su[[i]] , Diff_sig_mat_train_su[[i]] ) , max( Diff_sig_nonmat_train_su[[i]], Diff_sig_mat_train_su[[i]]) , length.out=100)  
  diffseq_nonmat_c_sig_su[[i]] <- seq(min (Diff_sig_nonmat_train_su[[i]], Diff_sig_nonmat_train_su[[i]] ) , max( Diff_sig_nonmat_train_su[[i]], Diff_sig_mat_train_su[[i]]) , length.out=100)  
  normal_mat_c_sig_su[[i]] <- dnorm (x=diffseq_mat_c_sig_su[[i]] , mean= mean (Diff_sig_mat_train_su[[i]]) , sd = sd(Diff_sig_mat_train_su[[i]]) )
  normal_nonmat_c_sig_su[[i]] <- dnorm (x=diffseq_nonmat_c_sig_su[[i]] , mean= mean (Diff_sig_nonmat_train_su[[i]]) , sd = sd(Diff_sig_nonmat_train_su[[i]]) )
  
  
  #histogram
  histdensity_c_sig_su[[i]] <- cbind(hist_mat_c_sig_su[[i]]$mids - intervalsig [i]/2, hist_mat_c_sig_su[[i]]$mids + intervalsig [i]/2 , 
                                     hist_mat_c_sig_su[[i]]$counts / sum(hist_mat_c_sig_su[[i]]$counts) ,  
                                     hist_nonmat_c_sig_su[[i]]$counts / sum(hist_nonmat_c_sig_su[[i]]$counts) ) 
  
  hist_mat_c_sig_su[[i]]$counts <-  hist_mat_c_sig_su[[i]]$counts / sum ( hist_mat_c_sig_su[[i]]$counts)
  hist_nonmat_c_sig_su[[i]]$counts <-  hist_nonmat_c_sig_su[[i]]$counts / sum ( hist_nonmat_c_sig_su[[i]]$counts)
  
  
}


# save kernels
save(diffseq_mat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_tt.RData")
save(multiplier_hist_mat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_tt.RData")
save(normal_mat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_tt.RData")

save(diffseq_nonmat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_tt.RData")
save(multiplier_hist_nonmat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_tt.RData")
save(normal_nonmat_c_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_tt.RData")

save(diffseq_mat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_su.RData")
save(multiplier_hist_mat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_su.RData")
save(normal_mat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_su.RData")

save(diffseq_nonmat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_su.RData")
save(multiplier_hist_nonmat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_su.RData")
save(normal_nonmat_c_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_su.RData")


save(diffseq_mat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_sig_tt.RData")
save(multiplier_hist_mat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_sig_tt.RData")
save(normal_mat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_sig_tt.RData")

save(diffseq_nonmat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_sig_tt.RData")
save(multiplier_hist_nonmat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_sig_tt.RData")
save(normal_nonmat_c_sig_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_sig_tt.RData")

save(diffseq_mat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_c_sig_su.RData")
save(multiplier_hist_mat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_c_sig_su.RData")
save(normal_mat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_c_sig_su.RData")

save(diffseq_nonmat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_c_sig_su.RData")
save(multiplier_hist_nonmat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_c_sig_su.RData")
save(normal_nonmat_c_sig_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_c_sig_su.RData")

save(diffseq_mat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_n_tt.RData")
save(multiplier_hist_mat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_n_tt.RData")
save(normal_mat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_n_tt.RData")

save(diffseq_nonmat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_n_tt.RData")
save(multiplier_hist_nonmat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_n_tt.RData")
save(normal_nonmat_n_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_n_tt.RData")

save(diffseq_mat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_mat_n_su.RData")
save(multiplier_hist_mat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_mat_n_su.RData")
save(normal_mat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_mat_n_su.RData")

save(diffseq_nonmat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/diffseq_nonmat_n_su.RData")
save(multiplier_hist_nonmat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/ multiplier_hist_nonmat_n_su.RData")
save(normal_nonmat_n_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/normal_nonmat_n_su.RData")

save(max_train_mat_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_mat_su.RData")
save(max_train_mat_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_mat_tt.RData")
save(min_train_mat_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_mat_su.RData")
save(min_train_mat_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_mat_tt.RData")
save(max_train_nonmat_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_nonmat_su.RData")
save(max_train_nonmat_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/max_train_nonmat_tt.RData")
save(min_train_nonmat_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_nonmat_su.RData")
save(min_train_nonmat_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/min_train_nonmat_tt.RData")

save(Upsiglist_train, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist_train.RData" )
save(Upsiglist_test, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist_test.RData" )
save(a_magdif_train, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_train.RData" )
save(a_magdif_test, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_test.RData" )

save(sigfeature_train, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_train.RData" )
save(sigfeature_test, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_test.RData" )
# clustering - SIG
# variance

# v1
# stdmat_sig <- vector()
# stdnonmat_sig <- vector()
# for ( i in 1 : length(sigweight )  ){
#   stdmat_sig[i] = sd(Diff_sig_mat_train[[i]]) 
#   stdnonmat_sig[i] = sd(Diff_sig_nonmat_train[[i]])
# }
# 
# stdall_sig <- vector()
# stdall_sig <- c(stdmat_sig, stdnonmat_sig)




#v2

# TT unit
stdmat_sig_tt <- vector()
stdnonmat_sig_tt <- vector()
for ( i in 1 : length(intervalsig)  ){
  stdmat_sig_tt[i] = sd(Diff_sig_mat_train_tt[[i]]) / mean(abs (Diff_sig_mat_train_tt[[i]]) )
  stdnonmat_sig_tt[i] = sd(Diff_sig_nonmat_train_tt[[i]]) / mean(abs (Diff_sig_nonmat_train_tt[[i]]) )
}

stdall_sig_tt <- vector()
stdall_sig_tt <- c(stdmat_sig_tt, stdnonmat_sig_tt)


X =  sort(stdmat_sig_tt)
X <- X[-50]
fit <- kmeans(X, 2)
# kmeansAIC(fit)
plot(X, col = fit$cluster)
stdthreshold_sig_tt <- 1.20



# SU unit
stdmat_sig_su <- vector()
stdnonmat_sig_su <- vector()
for ( i in 1 : length(intervalsig)  ){
  stdmat_sig_su[i] = sd(Diff_sig_mat_train_su[[i]]) / mean(abs (Diff_sig_mat_train_su[[i]]) )
  stdnonmat_sig_su[i] = sd(Diff_sig_nonmat_train_su[[i]]) / mean(abs (Diff_sig_nonmat_train_su[[i]]) )
}

stdall_sig_su <- vector()
stdall_sig_su <- c(stdmat_sig_su, stdnonmat_sig_su)


X =  sort(stdmat_sig_su)
X <- X[-50] # outlier
fit <- kmeans(X, 2)
# kmeansAIC(fit)
plot(X, col = fit$cluster)
stdthreshold_sig_su <- 1.30


# ks test
## TT unit
ttestpvalue_sig_tt <- vector()
for ( i in 1 : length(intervalsig )  ){
  ttestpvalue_sig_tt[i]<- ks.test( normal_mat_c_sig_tt[[i]],  normal_nonmat_c_sig_tt[[i]])$p.value
}

distidx_tt <- which(ttestpvalue_sig_tt < 0.05)

sigfeatidx_tt <- vector()
for ( i in 1: length(intervalsig)) {
  if ( i %in% distidx_tt){
    if (stdmat_sig_tt[i] > stdthreshold_sig_tt & stdnonmat_sig_tt[i] > stdthreshold_sig_tt ){
      sigfeatidx_tt[i] <- 1
    }
    if (stdmat_sig_tt[i] < stdthreshold_sig_tt & stdnonmat_sig_tt[i] < stdthreshold_sig_tt ){
      sigfeatidx_tt[i] <- 2
    }
    if (stdmat_sig_tt[i] < stdthreshold_sig_tt & stdnonmat_sig_tt[i] > stdthreshold_sig_tt ){
      sigfeatidx_tt[i] <- 3
    }
    if (stdmat_sig_tt[i] > stdthreshold_sig_tt & stdnonmat_sig_tt[i] < stdthreshold_sig_tt ){
      sigfeatidx_tt[i] <- 4
    }
    
  }
  
  else {
    sigfeatidx_tt [i] <- 0  # mat = nonmat
  }
}


# SU unit

ttestpvalue_sig_su <- vector()
for ( i in 1 : length(intervalsig )  ){
  ttestpvalue_sig_su[i]<- ks.test( normal_mat_c_sig_su[[i]],  normal_nonmat_c_sig_su[[i]])$p.value
}

distidx_su <- which(ttestpvalue_sig_su < 0.05)

sigfeatidx_su <- vector()
for ( i in 1: length(intervalsig)) {
  if ( i %in% distidx_su){
    if (stdmat_sig_su[i] > stdthreshold_sig_su & stdnonmat_sig_su[i] > stdthreshold_sig_su ){
      sigfeatidx_su[i] <- 1
    }
    if (stdmat_sig_su[i] < stdthreshold_sig_su & stdnonmat_sig_su[i] < stdthreshold_sig_su ){
      sigfeatidx_su[i] <- 2
    }
    if (stdmat_sig_su[i] < stdthreshold_sig_su & stdnonmat_sig_su[i] > stdthreshold_sig_su ){
      sigfeatidx_su[i] <- 3
    }
    if (stdmat_sig_su[i] > stdthreshold_sig_su & stdnonmat_sig_su[i] < stdthreshold_sig_su ){
      sigfeatidx_su[i] <- 4
    }
    
  }
  
  else {
    sigfeatidx_su [i] <- 0  # mat = nonmat
  }
}




# WIM

# TT unit
# ks test
ttestpvalue_wim_tt <- vector()
for ( i in 1 : 31 ){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    ttestpvalue_wim_tt[i]<- ks.test( normal_mat_c_tt[[i]],  normal_nonmat_c_tt[[i]])$p.value
  }
  else ttestpvalue_wim_tt[i] <- NA
}


distidx_tt <- which(ttestpvalue_wim_tt > 0.05)


# version 2
stdmat_wim_all_tt <- vector()
stdnonmat_wim_all_tt <- vector()

for ( i in 1 : 31  ){
  if (i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
    stdmat_wim_all_tt[i] <-  sd(Diff_mat_train_n_tt[[i]]) / mean(Diff_mat_train_n_tt[[i]])
    stdnonmat_wim_all_tt[i] <- sd(Diff_nonmat_train_n_tt[[i]]) / mean(Diff_nonmat_train_n_tt[[i]])
  }
}


stdall_wim_all_tt <- vector()
stdall_wim_all_tt <- c( stdmat_wim_all_tt ,   stdnonmat_wim_all_tt)

X =  sort(stdall_wim_all_tt )
fit <- kmeans(X, 2)
plot(X, col = fit$cluster)


stdthreshold_wim_all_tt <- 0.43



wimfeatidx_tt <- vector()
for ( i in 1: 30) {
  if (i %in% distidx_tt){
    wimfeatidx_tt[i] <- 0
  }
  else{
    
    if ( i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
      
      if (stdmat_wim_all_tt[i] > stdthreshold_wim_all_tt & stdnonmat_wim_all_tt[i] > stdthreshold_wim_all_tt ){
        wimfeatidx_tt[i] <- 1
      }
      if (stdmat_wim_all_tt[i] < stdthreshold_wim_all_tt & stdnonmat_wim_all_tt[i] < stdthreshold_wim_all_tt ){
        wimfeatidx_tt[i] <- 2
      }
      if (stdmat_wim_all_tt[i] < stdthreshold_wim_all_tt & stdnonmat_wim_all_tt[i] > stdthreshold_wim_all_tt ){
        wimfeatidx_tt[i] <- 3
      }
      if (stdmat_wim_all_tt[i] > stdthreshold_wim_all_tt & stdnonmat_wim_all_tt[i] <  stdthreshold_wim_all_tt ){
        wimfeatidx_tt[i] <- 4
      }
      
    }
    
    else {
      wimfeatidx_tt[i] <- 0
    }
  }
}
wimfeatidx_tt[31] <- 1

# SU unit

# ks test
ttestpvalue_wim_su <- vector()
for ( i in 1 : 31 ){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    ttestpvalue_wim_su[i]<- ks.test( normal_mat_c_su[[i]],  normal_nonmat_c_su[[i]])$p.value
  }
  else ttestpvalue_wim_su[i] <- NA
}


distidx_su <- which(ttestpvalue_wim_su > 0.05)



stdmat_wim_all_su <- vector()
stdnonmat_wim_all_su <- vector()

for ( i in 1 : 31  ){
  if (i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
    stdmat_wim_all_su[i] <-  sd(Diff_mat_train_n_su[[i]]) / mean(Diff_mat_train_n_su[[i]])
    stdnonmat_wim_all_su[i] <- sd(Diff_nonmat_train_n_su[[i]]) / mean(Diff_nonmat_train_n_su[[i]])
  }
}


stdall_wim_all_su <- vector()
stdall_wim_all_su <- c( stdmat_wim_all_su ,   stdnonmat_wim_all_su)

X =  sort(stdall_wim_all_su )
fit <- kmeans(X, 2)
plot(X, col = fit$cluster)


stdthreshold_wim_all_su <- 0.40



wimfeatidx_su <- vector()
for ( i in 1: 30) {
  if (i %in% distidx_su){
    wimfeatidx_su[i] <- 0
  }
  else{
    
    if ( i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
      
      if (stdmat_wim_all_su[i] > stdthreshold_wim_all_su & stdnonmat_wim_all_su[i] > stdthreshold_wim_all_su ){
        wimfeatidx_su[i] <- 1
      }
      if (stdmat_wim_all_su[i] < stdthreshold_wim_all_su & stdnonmat_wim_all_su[i] < stdthreshold_wim_all_su ){
        wimfeatidx_su[i] <- 2
      }
      if (stdmat_wim_all_su[i] < stdthreshold_wim_all_su & stdnonmat_wim_all_su[i] > stdthreshold_wim_all_su ){
        wimfeatidx_su[i] <- 3
      }
      if (stdmat_wim_all_su[i] > stdthreshold_wim_all_su & stdnonmat_wim_all_su[i] <  stdthreshold_wim_all_su ){
        wimfeatidx_su[i] <- 4
      }
      
    }
    
    else {
      wimfeatidx_su[i] <- 0
    }
  }
}
wimfeatidx_su[31] <- 1



# IG
# TT unit
WIMWttemp_tt <- data.frame()

for (j in 1:length(Diff_mat_train_tt[[1]])){
  
  for (i in 1: 30) { 
    WIMWttemp_tt[j,i] <-Diff_mat_train_tt[[i]][j] 
  }
  WIMWttemp_tt[j,i+1] <- "1"
  
}

for (jj in 1:length(Diff_nonmat_train_tt[[1]])){
  
  for (i in 1: 30) { 
    WIMWttemp_tt[j+jj,i] <-Diff_nonmat_train_tt[[i]][jj] 
  }
  WIMWttemp_tt[j+jj,i+1] <- "2"
  
}


WIMWttemp_tt <- na.omit(WIMWttemp_tt)


WIMWttemp_tt <- WIMWttemp_tt[-8:-11]
WIMWttemp_tt <- WIMWttemp_tt[-18:-25]
View(WIMWttemp_tt)
colnames(WIMWttemp_tt) <- c("time", "length" , "weight", "axsp12" , "axsp23" , "axsp34", "axsp45", 
                         "axwt1l" , "axwt1r" ,  "axwt2l" , "axwt2r" , "axwt3l" , "axwt3r" , "axwt4l" , "axwt4r" ,
                         "axwt5l" , "axwt5r" , "duration" , "matchidx")




wimIGweights_tt <- information.gain(matchidx~., WIMWttemp_tt)
print(wimIGweights_tt)
nIG_tt <- sum(wimIGweights_tt > 0)
subset_tt <- cutoff.k(wimIGweights_tt, nIG_tt)
f <- as.simple.formula(subset_tt, "matchidx")
print(f)



# SU unit
WIMWttemp_su <- data.frame()

for (j in 1:length(Diff_mat_train_su[[1]])){
  
  for (i in 1: 30) { 
    WIMWttemp_su[j,i] <-Diff_mat_train_su[[i]][j] 
  }
  WIMWttemp_su[j,i+1] <- "1"
  
}

for (jj in 1:length(Diff_nonmat_train_su[[1]])){
  
  for (i in 1: 30) { 
    WIMWttemp_su[j+jj,i] <-Diff_nonmat_train_su[[i]][jj] 
  }
  WIMWttemp_su[j+jj,i+1] <- "2"
  
}


WIMWttemp_su <- na.omit(WIMWttemp_su)


WIMWttemp_su <- WIMWttemp_su[-8:-11]
WIMWttemp_su <- WIMWttemp_su[-18:-25]
View(WIMWttemp_su)
colnames(WIMWttemp_su) <- c("time", "length" , "weight", "axsp12" , "axsp23" , "axsp34", "axsp45", 
                            "axwt1l" , "axwt1r" ,  "axwt2l" , "axwt2r" , "axwt3l" , "axwt3r" , "axwt4l" , "axwt4r" ,
                            "axwt5l" , "axwt5r" , "duration" , "matchidx")




wimIGweights_su <- information.gain(matchidx~., WIMWttemp_su)
print(wimIGweights_su)
nIG_su <- sum(wimIGweights_su > 0)
subset_su <- cutoff.k(wimIGweights_su, nIG_su)
f <- as.simple.formula(subset_su, "matchidx")
print(f)



# sig weight -IG
## TT uni 
SIGWttemp_tt <- data.frame()

for (j in 1:length(sig_mat_train_tt)){
  
  for (i in 1: 50) { 
    SIGWttemp_tt[j,i] <-  Diff_sig_mat_train_tt [[i]][j] 
  }
  SIGWttemp_tt[j,i+1] <- "1"
  
}

for (jj in 1:length(sig_nonmat_train_tt)){
  
  for (i in 1: 50) { 
    SIGWttemp_tt[j+jj,i] <-  Diff_sig_nonmat_train_tt[[i]][jj] 
  }
  SIGWttemp_tt[j+jj,i+1] <- "2"
  
}


SIGWttemp_tt <- na.omit(SIGWttemp_tt)
View(SIGWttemp_tt)

colnames(SIGWttemp_tt)[51] <- c("sigidx")
SIGIGweights_tt <- information.gain(sigidx~., SIGWttemp_tt)
print(SIGIGweights_tt)
nIG_tt <- sum(SIGIGweights_tt > 0)
subset <- cutoff.k(SIGIGweights_tt, nIG_tt)
f <- as.simple.formula(subset, "sigidx")
print(f)

## SU unit
SIGWttemp_su <- data.frame()

for (j in 1:length(sig_mat_train_su)){
  
  for (i in 1: 50) { 
    SIGWttemp_su[j,i] <-  Diff_sig_mat_train_su [[i]][j] 
  }
  SIGWttemp_su[j,i+1] <- "1"
  
}

for (jj in 1:length(sig_nonmat_train_su)){
  
  for (i in 1: 50) { 
    SIGWttemp_su[j+jj,i] <-  Diff_sig_nonmat_train_su[[i]][jj] 
  }
  SIGWttemp_su[j+jj,i+1] <- "2"
  
}


SIGWttemp_su <- na.omit(SIGWttemp_su)
View(SIGWttemp_su)

colnames(SIGWttemp_su)[51] <- c("sigidx")
SIGIGweights_su <- information.gain(sigidx~., SIGWttemp_su)
print(SIGIGweights_su)
nIG_su <- sum(SIGIGweights_su > 0)
subset <- cutoff.k(SIGIGweights_su, nIG_su)
f <- as.simple.formula(subset, "sigidx")
print(f)

# All unit
# 
# SIGWttemp_all <- data.frame()
# 
# for (j in 1:length(sig_mat_train_tt)){
#   
#   for (i in 1: 50) { 
#     SIGWttemp_all[j,i] <-  Diff_sig_mat_train_tt [[i]][j] 
#   }
#   SIGWttemp_all[j,i+1] <- "1"
#   
# }
# 
# for (jj in 1:length(sig_nonmat_train_tt)){
#   
#   for (i in 1: 50) { 
#     SIGWttemp_all[j+jj,i] <-  Diff_sig_nonmat_train_tt[[i]][jj] 
#   }
#   SIGWttemp_all[j+jj,i+1] <- "2"
#   
# }
# 
# for (jjj in 1:length(sig_mat_train_su)){
#   
#   for (i in 1: 50) { 
#     SIGWttemp_all[jjj,i] <-  Diff_sig_mat_train_su [[i]][jjj] 
#   }
#   SIGWttemp_all[jjj,i+1] <- "1"
#   
# }
# 
# 
# for (jjjj in 1:length(sig_nonmat_train_su)){
#   
#   for (i in 1: 50) { 
#     SIGWttemp_all[j+jjjj,i] <-  Diff_sig_nonmat_train_su[[i]][jjjj] 
#   }
#   SIGWttemp_all[j+jjjj,i+1] <- "2"
#   
# }
# 
# SIGWttemp__all <- na.omit(SIGWttemp_all)
# View(SIGWttemp_all)
# 
# colnames(SIGWttemp_all)[51] <- c("sigidx")
# SIGIGweights_all<- information.gain(sigidx~., SIGWttemp_all)
# print(SIGIGweights_all)
# nIG_all <- sum(SIGIGweights_all > 0)
# subset <- cutoff.k(SIGIGweights_all, nIG_all)
# f <- as.simple.formula(subset, "sigidx")
# print(f)



# SIGIGweights_su <- SIGIGweights_all
# SIGIGweights_tt <- SIGIGweights_all


save(wimIGweights_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_su.RData")
save(wimIGweights_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimIGweights_tt.RData")
save(wimfeatidx_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_su.RData")
save(wimfeatidx_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/wimfeatidx_tt.RData")
save(SIGIGweights_su, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_su.RData")
save(SIGIGweights_tt, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigIGweights_tt.RData")
save(sigfeatidx_su , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_su.RData")
save(sigfeatidx_tt , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeatidx_tt.RData")


#sample plot
# library(ggplot2)
i=19
plot(range(min_tt[i], max_tt[i]), range(density_hist_mat_c_tt[[i]]$y))
lines(density_hist_mat_c_tt[[i]] )
lines(density_smooth_hist_mat_c_tt[[i]], col="blue")
plot(density_smooth_hist_mat_c_tt[[i]]$x, density_smooth_hist_mat_c_tt[[i]]$y)
plot(density_smooth_hist_mat_c_tt[[i]]$data$x, density_smooth_hist_mat_c_tt[[i]]$data$y)

plot(range(min_tt[i], max_tt[i]), range(density_hist_mat_c_tt[[i]]$y))
lines(density_hist_nonmat_c_tt[[i]] )
lines(density_smooth_hist_nonmat_c_tt[[i]], col="blue")
plot(density_smooth_hist_nonmat_c_tt[[i]]$x, density_smooth_hist_nonmat_c_tt[[i]]$y)
plot(density_smooth_hist_nonmat_c_tt[[i]]$data$x, density_smooth_hist_nonmat_c_tt[[i]]$data$y)


i=19
plot(range(min_tt[i], max_tt[i]), range(density_hist_mat_n_tt[[i]]$y))
lines(density_hist_mat_n_tt[[i]] )
lines(density_smooth_hist_mat_n_tt[[i]], col="blue")
plot(density_smooth_hist_mat_n_tt[[i]]$x, density_smooth_hist_mat_n_tt[[i]]$y)
plot(density_smooth_hist_mat_n_tt[[i]]$data$x, density_smooth_hist_mat_n_tt[[i]]$data$y)

plot(range(min_tt[i], max_tt[i]), range(density_hist_mat_n_tt[[i]]$y))
lines(density_hist_nonmat_n_tt[[i]] )
lines(density_smooth_hist_nonmat_n_tt[[i]], col="blue")
plot(density_smooth_hist_nonmat_n_tt[[i]]$x, density_smooth_hist_nonmat_n_tt[[i]]$y)
plot(density_smooth_hist_nonmat_n_tt[[i]]$data$x, density_smooth_hist_nonmat_n_tt[[i]]$data$y)



#plot a = non-parametric but smoothing data
a_mat_tt <- data.frame(density_smooth_hist_mat_c_tt[[i]]$x , density_smooth_hist_mat_c_tt[[i]]$y )
a_nonmat_tt <- data.frame(density_smooth_hist_nonmat_c_tt[[i]]$x, density_smooth_hist_nonmat_c_tt[[i]]$y)
ggplot() +
  geom_line(data=a_mat_tt, aes(x=a_mat_tt[,1] , y=a_mat_tt[,2]) , color='green') + 
  geom_line(data=a_nonmat_tt, aes(x=a_nonmat_tt[,1] , y=a_nonmat_tt[,2]) , color='red') 


a_mat_tt <- data.frame(density_smooth_hist_mat_n_tt[[i]]$x , density_smooth_hist_mat_n_tt[[i]]$y )
a_nonmat_tt <- data.frame(density_smooth_hist_nonmat_n_tt[[i]]$x, density_smooth_hist_nonmat_n_tt[[i]]$y)
ggplot() +
  geom_line(data=a_mat_tt, aes(x=a_mat_tt[,1] , y=a_mat_tt[,2]) , color='green') + 
  geom_line(data=a_nonmat_tt, aes(x=a_nonmat_tt[,1] , y=a_nonmat_tt[,2]) , color='red') 

a_mat_su <- data.frame(density_smooth_hist_mat_c_su[[i]]$x , density_smooth_hist_mat_c_su[[i]]$y )
a_nonmat_su <- data.frame(density_smooth_hist_nonmat_c_su[[i]]$x, density_smooth_hist_nonmat_c_su[[i]]$y)
ggplot() +
  geom_line(data=a_mat_su, aes(x=a_mat_su[,1] , y=a_mat_su[,2]) , color='green') + 
  geom_line(data=a_nonmat_su, aes(x=a_nonmat_su[,1] , y=a_nonmat_su[,2]) , color='red') 


a_mat_su <- data.frame(density_smooth_hist_mat_n_su[[i]]$x , density_smooth_hist_mat_n_su[[i]]$y )
a_nonmat_su <- data.frame(density_smooth_hist_nonmat_n_su[[i]]$x, density_smooth_hist_nonmat_n_su[[i]]$y)
ggplot() +
  geom_line(data=a_mat_su, aes(x=a_mat_su[,1] , y=a_mat_su[,2]) , color='green') + 
  geom_line(data=a_nonmat_su, aes(x=a_nonmat_su[,1] , y=a_nonmat_su[,2]) , color='red') 


#plot b = parametric , Gaussian fitting data
i=2
b_mat_tt <- data.frame(diffseq_mat_c_tt[[i]], 
                    normal_mat_c_tt[[i]]  * multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]  )
b_nonmat_tt <- data.frame( diffseq_nonmat_c_tt[[i]], 
                        normal_nonmat_c_tt[[i]]  * multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat_tt, aes ( x=a_mat_tt[,1] , y=a_mat_tt[,2]  ) , color='blue') + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='black') 

head(b_mat_tt)



b_mat_su <- data.frame(diffseq_mat_c_su[[i]], 
                       normal_mat_c_su[[i]]  * multiplier_hist_mat_c_su[[i]][ which.min(is.na( multiplier_hist_mat_c_su[[i]] ) ) ]  )
b_nonmat_su <- data.frame( diffseq_nonmat_c_su[[i]], 
                           normal_nonmat_c_su[[i]]  * multiplier_hist_nonmat_c_su[[i]][ which.min(is.na( multiplier_hist_nonmat_c_su[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_su, aes ( x=b_nonmat_su[,1] , y=b_nonmat_su[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat_su, aes ( x=a_mat_su[,1] , y=a_mat_su[,2]  ) , color='blue') + 
  geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='black') 

head(b_mat_su)


## normalized 
b_mat_tt <- data.frame(diffseq_mat_n_tt[[i]], 
                    normal_mat_n_tt[[i]]  * multiplier_hist_mat_n_tt[[i]][ which.min(is.na( multiplier_hist_mat_n_tt[[i]] ) ) ]  )
b_nonmat_tt <- data.frame( diffseq_nonmat_n_tt[[i]], 
                        normal_nonmat_n_tt[[i]]  * multiplier_hist_nonmat_n_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat_tt, aes ( x=a_mat_tt[,1] , y=a_mat_tt[,2]  ) , color='blue') + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='black') 

head(b_mat_tt)


b_mat_su <- data.frame(diffseq_mat_n_su[[i]], 
                       normal_mat_n_su[[i]]  * multiplier_hist_mat_n_su[[i]][ which.min(is.na( multiplier_hist_mat_n_su[[i]] ) ) ]  )
b_nonmat_su <- data.frame( diffseq_nonmat_n_su[[i]], 
                           normal_nonmat_n_su[[i]]  * multiplier_hist_nonmat_n_su[[i]][ which.min(is.na( multiplier_hist_nonmat_n_su[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_su, aes ( x=b_nonmat_su[,1] , y=b_nonmat_su[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat_su, aes ( x=a_mat_su[,1] , y=a_mat_su[,2]  ) , color='blue') + 
  geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='black') 

head(b_mat_su)


# plot histogram
k <- 19
plot( hist_nonmat_c_tt[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_c_tt[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_c_tt[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_c_tt[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_c_tt[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_tt[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference - Nonnormalized data')

plot( hist_mat_c_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

#normalized
k <- 19
plot( hist_nonmat_n_tt[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_n_tt[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_n_tt[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_n_tt[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_n_tt[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_n_tt[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference TT - Nonnormalized data')

plot( hist_mat_n_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)


#normalized

plot( hist_nonmat_n_su[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_n_su[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_n_su[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_n_su[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_n_su[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_n_su[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference TT - Nonnormalized data')

plot( hist_mat_n_su[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)


## sig plot

#sig
plot(range(min_sig_tt[i], max_sig_tt[i]), range(density_hist_mat_c_sig_tt[[i]]$y))
lines(density_hist_mat_c_sig_tt[[i]] )
lines(density_smooth_hist_mat_c_sig_tt[[i]], col="blue")
plot(density_smooth_hist_mat_c_sig_tt[[i]]$x, density_smooth_hist_mat_c_sig_tt[[i]]$y)
plot(density_smooth_hist_mat_c_sig_tt[[i]]$data$x, density_smooth_hist_mat_c_sig_tt[[i]]$data$y)

plot(range(min_sig_tt[i], max_sig_tt[i]), range(density_hist_mat_c_sig_tt[[i]]$y))
lines(density_hist_nonmat_c_sig_tt[[i]] )
lines(density_smooth_hist_nonmat_c_sig_tt[[i]], col="blue")
plot(density_smooth_hist_nonmat_c_sig_tt[[i]]$x, density_smooth_hist_nonmat_c_sig_tt[[i]]$y)
plot(density_smooth_hist_nonmat_c_sig_tt[[i]]$data$x, density_smooth_hist_nonmat_c_sig_tt[[i]]$data$y)


#plot a = non-parametric but smoothing data
install.packages("grid")
install.packages("gridExtra")
library(ggplot2)
library(grid)
library(gridExtra)
library(multiplot)



setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/SigFeature") 

for (i in 1: 50){
  pt  <- ggplot()+
    geom_line(data= data.frame(density_smooth_hist_mat_c_sig_tt[[i]]$x , density_smooth_hist_mat_c_sig_tt[[i]]$y ),
              aes(x= data.frame(density_smooth_hist_mat_c_sig_tt[[i]]$x , density_smooth_hist_mat_c_sig_tt[[i]]$y )[,1] ,
                  y= data.frame(density_smooth_hist_mat_c_sig_tt[[i]]$x , density_smooth_hist_mat_c_sig_tt[[i]]$y )[,2]) ,
              color='green') + 
    geom_line(data=data.frame(density_smooth_hist_nonmat_c_sig_tt[[i]]$x, density_smooth_hist_nonmat_c_sig_tt[[i]]$y),
              aes(x=data.frame(density_smooth_hist_nonmat_c_sig_tt[[i]]$x, density_smooth_hist_nonmat_c_sig_tt[[i]]$y)[,1] ,
                  y=data.frame(density_smooth_hist_nonmat_c_sig_tt[[i]]$x, density_smooth_hist_nonmat_c_sig_tt[[i]]$y)[,2]) , 
              color='red' ) + ggtitle(i) 
  
  ggsave(filename = paste("sig feature - tt " , i, ".jpeg" , sep="") , plot = pt)
}

for (i in 1: 50){
  pt  <- ggplot()+
    geom_line(data= data.frame(density_smooth_hist_mat_c_sig_su[[i]]$x , density_smooth_hist_mat_c_sig_su[[i]]$y ),
              aes(x= data.frame(density_smooth_hist_mat_c_sig_su[[i]]$x , density_smooth_hist_mat_c_sig_su[[i]]$y )[,1] ,
                  y= data.frame(density_smooth_hist_mat_c_sig_su[[i]]$x , density_smooth_hist_mat_c_sig_su[[i]]$y )[,2]) ,
              color='green') + 
    geom_line(data=data.frame(density_smooth_hist_nonmat_c_sig_su[[i]]$x, density_smooth_hist_nonmat_c_sig_su[[i]]$y),
              aes(x=data.frame(density_smooth_hist_nonmat_c_sig_su[[i]]$x, density_smooth_hist_nonmat_c_sig_su[[i]]$y)[,1] ,
                  y=data.frame(density_smooth_hist_nonmat_c_sig_su[[i]]$x, density_smooth_hist_nonmat_c_sig_su[[i]]$y)[,2]) , 
              color='red' ) + ggtitle(i) 
  
  ggsave(filename = paste("sig feature - su " , i, ".jpeg" , sep="") , plot = pt)
}





for (i in 1: 50){
  
  b_mat_sig_tt <- data.frame(diffseq_mat_c_sig_tt[[i]], 
                          normal_mat_c_sig_tt[[i]]  * multiplier_hist_mat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[i]] ) ) ]  )
  b_nonmat_sig_tt <- data.frame( diffseq_nonmat_c_sig_tt[[i]], 
                              normal_nonmat_c_sig_tt[[i]]  * multiplier_hist_nonmat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[i]] ) ) ]  )
  pt <- ggplot() + 
    geom_line(data=b_mat_sig_tt, aes ( x=b_mat_sig_tt[,1] , y=b_mat_sig_tt[,2]  ) , color='green') +       
    geom_line(data=b_nonmat_sig_tt, aes ( x=b_nonmat_sig_tt[,1] , y=b_nonmat_sig_tt[,2]  ) , color='red')  + ggtitle(i) 
  
  pt <-   pt + xlim(-0.4, 0.4)
  
  ggsave(filename = paste("sig feature - gaussian tt" , i, ".jpeg" , sep="") , plot = pt)
}

for (i in 1: 50){
  
  b_mat_sig_su <- data.frame(diffseq_mat_c_sig_su[[i]], 
                             normal_mat_c_sig_su[[i]]  * multiplier_hist_mat_c_sig_su[[i]][ which.min(is.na( multiplier_hist_mat_c_sig_su[[i]] ) ) ]  )
  b_nonmat_sig_su <- data.frame( diffseq_nonmat_c_sig_su[[i]], 
                                 normal_nonmat_c_sig_su[[i]]  * multiplier_hist_nonmat_c_sig_su[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig_su[[i]] ) ) ]  )
  pt <- ggplot() + 
    geom_line(data=b_mat_sig_su, aes ( x=b_mat_sig_su[,1] , y=b_mat_sig_su[,2]  ) , color='green') +       
    geom_line(data=b_nonmat_sig_su, aes ( x=b_nonmat_sig_su[,1] , y=b_nonmat_sig_su[,2]  ) , color='red')  + ggtitle(i) 
  
  pt <-   pt + xlim(-0.4, 0.4)
  
  ggsave(filename = paste("sig feature - gaussian su" , i, ".jpeg" , sep="") , plot = pt)
}



setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/WIMFeature") 


# plot - wim 
for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat_tt <- data.frame(diffseq_mat_c_tt[[i]], 
                        normal_mat_c_tt[[i]]  * multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]  )
    b_nonmat_tt <- data.frame( diffseq_nonmat_c_tt[[i]], 
                            normal_nonmat_c_tt[[i]]  * multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') +       
      geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')  + ggtitle(i) 
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature - gaussian tt" , i, ".jpeg" , sep="") , plot = pt)
  }
}


for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat_tt <- data.frame(diffseq_mat_n_tt[[i]], 
                        normal_mat_n_tt[[i]]  * multiplier_hist_mat_n_tt[[i]][ which.min(is.na( multiplier_hist_mat_n_tt[[i]] ) ) ]  )
    b_nonmat_tt <- data.frame( diffseq_nonmat_n_tt[[i]], 
                            normal_nonmat_n_tt[[i]]  * multiplier_hist_nonmat_n_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_n_tt[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') +       
      geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')  + ggtitle(i) 
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature norm - gaussian tt" , i, ".jpeg" , sep="") , plot = pt)
  }
}




# plot - wim 
for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat_su <- data.frame(diffseq_mat_c_su[[i]], 
                           normal_mat_c_su[[i]]  * multiplier_hist_mat_c_su[[i]][ which.min(is.na( multiplier_hist_mat_c_su[[i]] ) ) ]  )
    b_nonmat_su <- data.frame( diffseq_nonmat_c_su[[i]], 
                               normal_nonmat_c_su[[i]]  * multiplier_hist_nonmat_c_su[[i]][ which.min(is.na( multiplier_hist_nonmat_c_su[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='green') +       
      geom_line(data=b_nonmat_su, aes ( x=b_nonmat_su[,1] , y=b_nonmat_su[,2]  ) , color='red')  + ggtitle(i) 
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature - gaussian su" , i, ".jpeg" , sep="") , plot = pt)
  }
}


for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat_su <- data.frame(diffseq_mat_n_su[[i]], 
                           normal_mat_n_su[[i]]  * multiplier_hist_mat_n_su[[i]][ which.min(is.na( multiplier_hist_mat_n_su[[i]] ) ) ]  )
    b_nonmat_su <- data.frame( diffseq_nonmat_n_su[[i]], 
                               normal_nonmat_n_su[[i]]  * multiplier_hist_nonmat_n_su[[i]][ which.min(is.na( multiplier_hist_nonmat_n_su[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat_su, aes ( x=b_mat_su[,1] , y=b_mat_su[,2]  ) , color='green') +       
      geom_line(data=b_nonmat_su, aes ( x=b_nonmat_su[,1] , y=b_nonmat_su[,2]  ) , color='red')  + ggtitle(i) 
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature norm - gaussian su" , i, ".jpeg" , sep="") , plot = pt)
  }
}


# plot histogram
k <- 19

plot( hist_nonmat_c_sig_tt[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_sig_mat_train_tt[[k]]) , intervalsig[k], f= floor)  , 
                     round_any (min(Diff_sig_nonmat_train_tt[[k]]) , intervalsig[k], f= floor) ) ,
               max ( round_any (max(Diff_sig_mat_train_tt[[k]]) , intervalsig[k], f= ceiling) ,
                     round_any (max(Diff_sig_nonmat_train_tt[[k]]) , intervalsig[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_sig[[k]]$count )),
      freq=TRUE,
      xlab = 'sigfeature diff', ylab = 'Density', main = 'Histogram of Sig Feature Difference')

plot( hist_mat_c_sig_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

# http://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_06232015")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_06232015")
#############################################################################################
# end
