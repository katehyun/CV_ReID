rm(list=ls())

# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_04292015")
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_04272015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif_04272015.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_04272015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result_NN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/SOLCFHWAClass.RData" )
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_04272015.RData")

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

# class 9
sub_all <- TargetTable_NN[[5]][,1:4]
sub_all <- cbind(sub_all,TargetTable_NN[[5]][,6],TargetTable_NN[[5]][,8])
sub_all <- cbind( sub_all ,      
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),13:44] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),7] ,
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),13:44] , # should be 6?
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),7] )
#                   Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),12] ) 
sub_all <- na.omit(sub_all)





colnames(sub_all)[5:6] <- c("objup", "upsig")
colnames(sub_all)[7:39] <- c( "downsig", "class", "numax", "utc", "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt",  "duration")

colnames(sub_all)[40:72] <- c("upsig", "class", "numax", "utc", "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp",
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt",  "duration")


# WIM data calibration
# get difference
matchingonly <- data.frame()
matchingonly <- subset(sub_all , sub_all$objup == sub_all$upsig )
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

matching_highest20_wim_diff_median <- data.frame()
matching_highest20_wim_diff_median <- t( apply(matching_highest20_wim_diff, 2, FUN = median)  )
matching_highest20_wim_diff_median <- round(matching_highest20_wim_diff_median, digits = 1)
colnames(matching_highest20_wim_diff_median)[1:28] <- c( "length", "gvw", 
                                                         "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                                                         "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                                                         "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                                                         "ax9lwt", "ax9rwt")

# hist( matching_highest20_wim_diff [,2])
# mean( matching_highest20_wim_diff [,2])
# median( matching_highest20_wim_diff [,2])

# calibrate wim (Up)
Upheader_new_cl <-  Upheader_new
for (i in 1: length( Upheader_new[,1])){
  for (j in 1: length(matching_highest20_wim_diff_median)){
    Upheader_new_cl[i,j+16] <- Upheader_new_cl [i,j+16] + matching_highest20_wim_diff_median[1,j]
  }
}


Upheader_new <- Upheader_new_cl

# reseting the sub_all with calibrated wim data
sub_all <- data.frame()
sub_all <- TargetTable_NN[[5]][,1:4]
sub_all <- cbind(sub_all,TargetTable_NN[[5]][,6],TargetTable_NN[[5]][,8])
sub_all <- cbind( sub_all ,      
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),13:44] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),7] ,
                  Upheader_new_cl[ match( sub_all[,6], as.numeric(Upheader_new_cl[,13])),13:44] , # should be 6?
                  Upheader_new_cl[ match( sub_all[,6], as.numeric(Upheader_new_cl[,13])),7] )


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
DownheaderTrain9Idx <-  which (Downheader_new[,12] > utcbd  &  Downheader_new[,14]== 9)
DownheaderTest9Idx <-  which (Downheader_new[,12] < utcbd  &  Downheader_new[,14]== 9)
Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]
sig_selected_train <- sig_selected[DownheaderTrainIdx]
sig_selected_test <- sig_selected[DownheaderTestIdx]
sig_selected_train_class9 <- sig_selected[DownheaderTrain9Idx]
sig_selected_test_class9 <- sig_selected[DownheaderTest9Idx]
sigfeature_train <- sigfeature[DownheaderTrainIdx]
sigfeature_test <- sigfeature[DownheaderTestIdx]


# prob_train <- prob [ DownheaderTrainIdx]
# prob_test <- prob [ DownheaderTestIdx]

# train set
sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) > utcbd    ) 
sub_matching_train <- subset(sub_all_train  , as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                               as.numeric (sub_all_train [,5]) != 999)
sub_nonmatching_train <- subset(sub_all_train , as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )

# train index
suballtrainIdx <- which ( as.numeric (str_sub (sub_all [,4],-13,-1) ) > utcbd  )
submatching_train_Idx <- which (  as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                                    as.numeric (sub_all_train [,5]) != 999 )
subnonmatching_train_Idx <- which ( as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )

# test set
sub_all_test  <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) <= utcbd     ) 
sub_matching_test <- subset(sub_all_test  , as.numeric (sub_all_test  [,5]) == as.numeric(sub_all_test  [,6]) &
                              as.numeric (sub_all_test [,5]) != 999)
sub_nonmatching_test <- subset(sub_all_test , as.numeric (sub_all_test [,5]) != as.numeric( sub_all_test [,6]) )

# test index
suballtestIdx <- which ( as.numeric (str_sub (sub_all [,4],-13,-1) ) <= utcbd     )
submatching_test_Idx <- which ( as.numeric (sub_all_test  [,5]) == as.numeric(sub_all_test  [,6]) &
                                  as.numeric (sub_all_test [,5]) != 999)
subnonmatching_test_Idx <- which ( as.numeric (sub_all_test [,5]) != as.numeric( sub_all_test [,6]) )



# Difference (train, test) 
Diff_mat_train <- list()
Diff_nonmat_train <- list()
Diff_mat_test <- list()
Diff_nonmat_test <- list()

for ( i in 1:30 )
{
  Diff_mat_train[[i]] <- na.omit ( (sub_matching_train [,9+i] - sub_matching_train[,42+i]) ) # no abs
  Diff_nonmat_train[[i]] <- na.omit ( (sub_nonmatching_train [,9+i] - sub_nonmatching_train[,42+i]) )  
}

Diff_mat_train[[31]] <- na.omit (  (sub_matching_train [,3]  )) 
Diff_nonmat_train[[31]] <- na.omit ( (sub_nonmatching_train [,3]  )) 

for ( i in 1:30 )
{
  Diff_mat_test[[i]] <- na.omit ( (sub_matching_test [,9+i] - sub_matching_test[,42+i]) ) # no abs
  Diff_nonmat_test[[i]] <- na.omit ( (sub_nonmatching_test [,9+i] - sub_nonmatching_test[,42+i]) )  
}

Diff_mat_test[[31]] <- na.omit (  (sub_matching_test [,3]  )) 
Diff_nonmat_test[[31]] <- na.omit ( (sub_nonmatching_test [,3]  )) 

# signature


sig_mat_train   <- sig_selected_train_class9[submatching_train_Idx]
sig_nonmat_train <- sig_selected_train_class9[subnonmatching_train_Idx]
sig_mat_test <- sig_selected_test_class9[submatching_test_Idx]
sig_nonmat_test <- sig_selected_test_class9[subnonmatching_test_Idx]

Diff_sig_mat_train <- list()
Diff_sig_nonmat_train <- list()
Diff_sig_mat_test <- list()
Diff_sig_nonmat_test <- list()


for ( i in 1: length( sig_mat_train [[1]] ) ) {
  Diff_sig_mat_train[[i]] <- do.call(rbind, sig_mat_train)[,i]
}

for ( i in 1: length( sig_nonmat_train [[1]] ) ) {
  Diff_sig_nonmat_train[[i]] <- do.call(rbind, sig_nonmat_train)[,i]
}

for ( i in 1: length( sig_mat_test [[1]] ) ) {
  Diff_sig_mat_test[[i]] <- do.call(rbind, sig_mat_test)[,i]
}

for ( i in 1: length( sig_nonmat_test [[1]] ) ) {
  Diff_sig_nonmat_test[[i]] <- do.call(rbind, sig_nonmat_test)[,i]
}


# remove outliers
median_train_mat <- vector()
median_train_nonmat <- vector()

median_train_mat[1] <- median( unlist( Diff_mat_train[[1]] ) )
median_train_nonmat[1] <- median( unlist( Diff_nonmat_train[[1]] ) )

for ( i in 1:30 )
{
  median_train_mat[i+1] <-  median ( unlist( Diff_mat_train[[i+1]] ) )
  median_train_nonmat[i+1] <-  median ( unlist( Diff_nonmat_train[[i+1]] ) )
}


mzscores_mat <- list()
mzscores_nonmat <- list()

mzscores_mat[[1]] <- 0.6745 * ( Diff_mat_train[[1]] - median_train_mat[1]  ) /
  ( median ( abs(  Diff_mat_train[[1]] - median_train_mat[1] )) )

mzscores_nonmat[[1]] <- 0.6745 * ( Diff_nonmat_train[[1]] - median_train_nonmat[1]  ) /
  ( median ( abs(  Diff_nonmat_train[[1]] - median_train_nonmat[1] )) )

for ( i in 1:30 )
{
  mzscores_mat[[i+1]] <- 0.6745 * ( Diff_mat_train[[i+1]] - median_train_mat[i+1]  ) /
    ( median ( abs(  Diff_mat_train[[i+1]] - median_train_mat[i+1] )) )
  
  mzscores_nonmat[[i+1]] <- 0.6745 * ( Diff_nonmat_train[[i+1]] - median_train_nonmat[i+1]  ) /
    ( median ( abs(  Diff_nonmat_train[[i+1]] - median_train_nonmat[i+1] )) )
}

mzscoresIndex_mat <- list()
mzscoresIndex_nonmat <- list()

mzscoresIndex_mat[[1]] <- which ( ( unlist (mzscores_mat[[1]]) < 3.5) & (unlist (mzscores_mat[[1]] ) > -3.5  ) ) 
mzscoresIndex_nonmat[[1]] <- which ( ( unlist (mzscores_nonmat[[1]]) < 3.5) & (unlist (mzscores_nonmat[[1]] ) > -3.5  ) )


for ( i in 1:30 )
{
  
  mzscoresIndex_mat[[i+1]] <- 
    which ( ( unlist (mzscores_mat[[i+1]]) < 3.5) & ( unlist (mzscores_mat[[i+1]] ) > -3.5  ) )
  mzscoresIndex_nonmat[[i+1]] <- 
    which ( ( unlist (mzscores_nonmat[[i+1]]) < 3.5) & ( unlist (mzscores_nonmat[[i+1]] ) > -3.5  ) )

}



# collect only clean data - Non-normalized data
Diff_mat_train_c <- list()
Diff_nonmat_train_c <- list()

Diff_mat_train_c[[1]] <- Diff_mat_train[[1]][ mzscoresIndex_mat[[1]] ]
Diff_nonmat_train_c[[1]] <- Diff_nonmat_train[[1]][  mzscoresIndex_nonmat[[1]] ]

for ( i in 1:30 )
{ 
  Diff_mat_train_c[[i+1]] <- Diff_mat_train[[i+1]][  mzscoresIndex_mat[[i+1]] ]
  Diff_nonmat_train_c[[i+1]] <- Diff_nonmat_train[[i+1]][ mzscoresIndex_nonmat[[i+1]] ]
}



# Normalized Difference (train) 
max_train_mat <- vector()
min_train_mat <- vector()
max_train_nonmat <- vector()
min_train_nonmat <- vector()
max_train_mat[1] <- max( unlist( Diff_mat_train_c[[1]] ) )
min_train_mat[1] <- min( unlist( Diff_mat_train_c[[1]] ) )
max_train_nonmat[1] <- max( unlist( Diff_nonmat_train_c[[1]] ) )
min_train_nonmat[1] <- min( unlist( Diff_nonmat_train_c[[1]] ) )


for ( i in 1:30 )
{
  max_train_mat[i+1] <-  max ( unlist( Diff_mat_train_c[[i+1]] ) )
  min_train_mat[i+1] <-  min ( unlist( Diff_mat_train_c[[i+1]] ) )
  max_train_nonmat[i+1] <-  max ( unlist( Diff_nonmat_train_c[[i+1]] ) )
  min_train_nonmat[i+1] <-  min ( unlist( Diff_nonmat_train_c[[i+1]] ) )
}


Diff_mat_train_n <- list()
Diff_nonmat_train_n <- list()
Diff_mat_train_n[[1]] <- na.omit ( ( Diff_mat_train_c[[1]]  - min_train_mat[1] ) / 
                                     ( max_train_mat[1] - min_train_mat[1] )  )
Diff_nonmat_train_n[[1]] <- na.omit (  ( Diff_nonmat_train_c[[1]] - min_train_nonmat[1] ) / 
                                         ( max_train_nonmat[1] - min_train_nonmat[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_n[[i+1]] <- na.omit ( ( Diff_mat_train_c[[1+i]] - min_train_mat[1+i] ) /  
                                         ( max_train_mat[1+i] - min_train_mat[1+i] )  )
  Diff_nonmat_train_n[[i+1]] <- na.omit ( ( Diff_nonmat_train_c[[1+i]] -  min_train_nonmat[1+i] ) /  
                                            ( max_train_nonmat[1+i] - min_train_nonmat[1+i] )  ) 
}

# 
# # scailized 
# 
# max_all <- vector()
# min_all <- vector()
# max_all[1] <- max( unlist( Diff_mat_train_c[[1]] ) ,  unlist(Diff_nonmat_train_c[[1]] )  )
# min_all[1] <- min( unlist( Diff_mat_train_c[[1]] ) ,  unlist(Diff_nonmat_train_c[[1]] )  )
# 
# for ( i in 1:30 )
# {
#   max_all[i+1] <-  max( unlist( Diff_mat_train_c[[i+1]] ) ,  unlist(Diff_nonmat_train_c[[i+1]] )  )
#   min_all[i+1] <-  min( unlist( Diff_mat_train_c[[i+1]] ) ,  unlist(Diff_nonmat_train_c[[i+1]] )  )
#   
# }
# 
# 
# Diff_mat_train_cs <- list()
# Diff_nonmat_train_cs <- list()
# Diff_mat_train_cs[[1]] <- na.omit ( ( Diff_mat_train_c[[1]]  - min_all[1] ) / 
#                                       ( max_all[1] - min_all[1] )  )
# Diff_nonmat_train_cs[[1]] <- na.omit (  ( Diff_nonmat_train_c[[1]] - min_all[1] ) / 
#                                           ( max_all[1] - min_all[1] )  )
# 
# for ( i in 1:30 )
# {
#   Diff_mat_train_cs[[i+1]] <- na.omit ( ( Diff_mat_train_c[[1+i]] - min_all[1+i] ) /  
#                                           ( max_all[1+i] - min_all[1+i] )  )
#   Diff_nonmat_train_cs[[i+1]] <- na.omit ( ( Diff_nonmat_train_c[[1+i]] -  min_all[1+i] ) /  
#                                              ( max_all[1+i] - min_all[1+i] )  ) 
# }
# 
# Diff_mat_train_ns <- list()
# Diff_nonmat_train_ns <- list()
# Diff_mat_train_ns[[1]] <- na.omit ( ( Diff_mat_train_n[[1]]  - min_all[1] ) / 
#                                       ( max_all[1] - min_all[1] )  )
# Diff_nonmat_train_ns[[1]] <- na.omit (  ( Diff_nonmat_train_n[[1]] - min_all[1] ) / 
#                                           ( max_all[1] - min_all[1] )  )
# 
# for ( i in 1:30 )
# {
#   Diff_mat_train_ns[[i+1]] <- na.omit ( ( Diff_mat_train_n[[1+i]] - min_all[1+i] ) /  
#                                           ( max_all[1+i] - min_all[1+i] )  )
#   Diff_nonmat_train_ns[[i+1]] <- na.omit ( ( Diff_nonmat_train_n[[1+i]] -  min_all[1+i] ) /  
#                                              ( max_all[1+i] - min_all[1+i] )  ) 
# }
# 


# # Percent Difference (train) 
# Diff_mat_train_pd <- list()
# Diff_nonmat_train_pd <- list()
# 
# 
# for ( i in 1:30 )
# {
#   Diff_mat_train_pd[[i]] <-  Diff_mat_train[[i+1]]/ sub_matching_train [,9+i]  
#   Diff_nonmat_train_pd[[i]] <-  Diff_nonmat_train[[i+1]] / sub_nonmatching_train [,9+i] 
# }
# Diff_mat_train_pd[[31]] <- na.omit (  abs(sub_matching_train [,3]  )) 
# Diff_nonmat_train_pd[[31]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 
# 
# 
# 
# max_train_mat_pd <- vector()
# min_train_mat_pd <- vector()
# max_train_nonmat_pd <- vector()
# min_train_nonmat_pd <- vector()
# max_train_mat_pd[1] <- max( unlist( Diff_mat_train_pd[[1]] ) )
# min_train_mat_pd[1] <- min( unlist( Diff_mat_train_pd[[1]] ) )
# max_train_nonmat_pd[1] <- max( unlist( Diff_nonmat_train_pd[[1]] ) )
# min_train_nonmat_pd[1] <- min( unlist( Diff_nonmat_train_pd[[1]] ) )
# 
# 
# for ( i in 1:30 )
# {
#   max_train_mat_pd[i+1] <-  max ( unlist( Diff_mat_train_pd[[i+1]] ) )
#   min_train_mat_pd[i+1] <-  min ( unlist( Diff_mat_train_pd[[i+1]] ) )
#   max_train_nonmat_pd[i+1] <-  max ( unlist( Diff_nonmat_train_pd[[i+1]] ) )
#   min_train_nonmat_pd[i+1] <-  min ( unlist( Diff_nonmat_train_pd[[i+1]] ) )
# }
# 
# 
# # to make non-zero data
# minval <- 0
# 
# #non-normalized data
# 
# for (i in 1: length( Diff_mat_train_c) ){
#   for (j in 1: length( Diff_mat_train_c[[i]] ) ){
#     
#     if ( length( Diff_mat_train_c[[i]])  > 0 ){
#       
#       if (Diff_mat_train_c[[i]][j] == 0 ){
#         Diff_mat_train_c[[i]][j]  <- minval
#       }
#       
#       if (Diff_mat_train_c[[i]][j] == 99999 ){
#         Diff_mat_train_c[[i]][j] <- NA
#       }
#     }
#   }
# }
# 
# 
# 
# for (i in 1: length( Diff_nonmat_train_c) ){
#   for (j in 1:length( Diff_nonmat_train_c[[i]] ) ){
#     
#     if ( length( Diff_nonmat_train_c[[i]])  > 0 ){
#       
#       if (Diff_nonmat_train_c[[i]][j] == 0 ){
#         Diff_nonmat_train_c[[i]][j] <- minval
#       }
#       
#       
#       if  (Diff_nonmat_train_c[[i]][j] == 99999 )  {
#         Diff_nonmat_train_c[[i]][j] <- NA
#       }
#       
#     }
#   }
# }
# 
# 
# #normalized data
# for (i in 1: length( Diff_mat_train ) ){
#   for (j in 1: length( Diff_mat_train[[i]] ) ){
#     
#     if ( length( Diff_mat_train_n[[i]])  > 0 ){
#       
#       if ((Diff_mat_train_n[[i]][j] == 0 ) && (!is.na(Diff_mat_train_n[[i]][j] ))){
#         Diff_mat_train_n[[i]][j]  <- minval
#       }
#       
#       if  ((Diff_mat_train_n[[i]][j] == 99999 ) && (!is.na(Diff_mat_train_n[[i]][j] ))){
#         Diff_mat_train_n[[i]][j] <- NA
#       }
#       
#     }   
#   }
# }
# 
# 
# 
# for (i in 1: length( Diff_nonmat_train_n ) ){
#   for (j in 1:length( Diff_nonmat_train_n[[i]] ) ){
#     
#     if ( length( Diff_nonmat_train_n[[i]])  > 0 ){
#       
#       if ((Diff_nonmat_train_n[[i]][j] == 0 )&& (!is.na(Diff_nonmat_train_n[[i]][j] ))){
#         Diff_nonmat_train_n[[i]][j] <- minval
#       }
#       
#       else if  ((Diff_nonmat_train_n[[i]][j] == 99999 ) && (!is.na(Diff_nonmat_train_n[[i]][j] ))){
#         Diff_nonmat_train_n[[i]][j] <- NA
#       }
#     }
#   }
# }



# remove NA
Diff_mat_train_c[[1]] <- na.omit (  Diff_mat_train_c[[1]]  )
Diff_nonmat_train_c[[1]] <- na.omit (  Diff_nonmat_train_c[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_c[[i+1]] <- na.omit (  Diff_mat_train_c[[1+i]]  )
  Diff_nonmat_train_c[[i+1]] <- na.omit (  Diff_nonmat_train_c[[1+i]]  ) 
}



Diff_mat_train_n[[1]] <- na.omit (  Diff_mat_train_n[[1]]  )
Diff_nonmat_train_n[[1]] <- na.omit (  Diff_nonmat_train_n[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_n[[i+1]] <- na.omit (  Diff_mat_train_n[[1+i]]  )
  Diff_nonmat_train_n[[i+1]] <- na.omit (  Diff_nonmat_train_n[[1+i]]  ) 
}

# # scalized data - non normalized
# 
# for (i in 1: length( Diff_mat_train_cs) ){
#   for (j in 1: length( Diff_mat_train_cs[[i]] ) ){
#     
#     if ( length( Diff_mat_train_cs[[i]])  > 0 ){
#       
#       if (Diff_mat_train_cs[[i]][j] == 0 ){
#         Diff_mat_train_cs[[i]][j]  <- minval
#       }
#       
#       if (Diff_mat_train_cs[[i]][j] == 99999 ){
#         Diff_mat_train_cs[[i]][j] <- NA
#       }
#     }
#   }
# }
# 
# 
# 
# for (i in 1: length( Diff_nonmat_train_cs) ){
#   for (j in 1:length( Diff_nonmat_train_cs[[i]] ) ){
#     
#     if ( length( Diff_nonmat_train_cs[[i]])  > 0 ){
#       
#       if (Diff_nonmat_train_cs[[i]][j] == 0 ){
#         Diff_nonmat_train_cs[[i]][j] <- minval
#       }
#       
#       
#       if  (Diff_nonmat_train_cs[[i]][j] == 99999 )  {
#         Diff_nonmat_train_cs[[i]][j] <- NA
#       }
#       
#     }
#   }
# }
# 
# 
# # scalized data - normalized data
# 
# for (i in 1: length( Diff_mat_train ) ){
#   for (j in 1: length( Diff_mat_train[[i]] ) ){
#     
#     if ( length( Diff_mat_train_ns[[i]])  > 0 ){
#       
#       if ((Diff_mat_train_ns[[i]][j] == 0 ) && (!is.na(Diff_mat_train_ns[[i]][j] ))){
#         Diff_mat_train_ns[[i]][j]  <- minval
#       }
#       
#       if  ((Diff_mat_train_ns[[i]][j] == 99999 ) && (!is.na(Diff_mat_train_ns[[i]][j] ))){
#         Diff_mat_train_ns[[i]][j] <- NA
#       }
#       
#     }   
#   }
# }
# 
# 
# 
# for (i in 1: length( Diff_nonmat_train_ns ) ){
#   for (j in 1:length( Diff_nonmat_train_ns[[i]] ) ){
#     
#     if ( length( Diff_nonmat_train_ns[[i]])  > 0 ){
#       
#       if ((Diff_nonmat_train_ns[[i]][j] == 0 )&& (!is.na(Diff_nonmat_train_ns[[i]][j] ))){
#         Diff_nonmat_train_ns[[i]][j] <- minval
#       }
#       
#       else if  ((Diff_nonmat_train_ns[[i]][j] == 99999 ) && (!is.na(Diff_nonmat_train_ns[[i]][j] ))){
#         Diff_nonmat_train_ns[[i]][j] <- NA
#       }
#     }
#   }
# }
# 
# 
# Diff_mat_train_cs[[1]] <- na.omit (  Diff_mat_train_cs[[1]]  )
# Diff_nonmat_train_cs[[1]] <- na.omit (  Diff_nonmat_train_cs[[1]]  ) 
# for ( i in 1:30 )
# {
#   Diff_mat_train_cs[[i+1]] <- na.omit (  Diff_mat_train_cs[[1+i]]  )
#   Diff_nonmat_train_cs[[i+1]] <- na.omit (  Diff_nonmat_train_cs[[1+i]]  ) 
# }
# 
# 
# 
# Diff_mat_train_ns[[1]] <- na.omit (  Diff_mat_train_ns[[1]]  )
# Diff_nonmat_train_ns[[1]] <- na.omit (  Diff_nonmat_train_ns[[1]]  ) 
# for ( i in 1:30 )
# {
#   Diff_mat_train_ns[[i+1]] <- na.omit (  Diff_mat_train_ns[[1+i]]  )
#   Diff_nonmat_train_ns[[i+1]] <- na.omit (  Diff_nonmat_train_ns[[1+i]]  ) 
# }


###################### kerdel design

# kernel (nonparametric) : non-normalized differences - WIM
kernel_mat_c <- list()
kernel_nonmat_c <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {    
    kernel_mat_c[[i]] <- density(Diff_mat_train_c[[i]], kernel = "epanechnikov", bw=5)   
    kernel_nonmat_c[[i]] <- density(Diff_nonmat_train_c[[i]], kernel = "epanechnikov", bw=5)
  }
  
  else
  {
    kernel_mat_c$x[[i]] <- NA
    kernel_mat_c$y[[i]] <- NA
    kernel_nonmat_c$x[[i]] <-  NA
    kernel_nonmat_c$y[[i]] <-  NA
  }
  
}


# kernel (nonparametric) : non-normalized differences - SIG
kernel_sig_mat_c <- list()
kernel_sig_nonmat_c <- list()

for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train[[i]]) != 0) {
    
    kernel_sig_mat_c[[i]] <- density(Diff_sig_mat_train[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c[[i]] <- density(Diff_sig_nonmat_train[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c$x[[i]] <- NA
    kernel_sig_mat_c$y[[i]] <- NA
    kernel_sig_nonmat_c$x[[i]] <-  NA
    kernel_sig_nonmat_c$y[[i]] <-  NA
  }
}


# kernel (nonparametric) : normalized differences
kernel_mat_n <- list()
kernel_nonmat_n <- list()


for ( i in 1: 31 )
{
  if ((sum ( Diff_mat_train_n[[i]]) != 0) && (!is.na(sum( Diff_mat_train_n[[i]]) ))){
    
    kernel_mat_n[[i]] <- density(Diff_mat_train_n[[i]], kernel = "epanechnikov")
    kernel_nonmat_n[[i]] <- density(Diff_nonmat_train_n[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_mat_n$x[[i]] <- NA
    kernel_mat_n$y[[i]] <- NA
    kernel_nonmat_n$x[[i]] <-  NA
    kernel_nonmat_n$y[[i]] <-  NA
  }
  
}


# # kernel (nonparametric) : non-normalized but scailized differences
# kernel_mat_cs <- list()
# kernel_nonmat_cs <- list()
# 
# 
# for ( i in 1: 31 )
# {
#   if (sum ( Diff_mat_train_cs[[i]]) != 0) {
#     set.seed(123)
#     kernel_mat_cs[[i]] <- density(Diff_mat_train_cs[[i]])
#     kernel_nonmat_cs[[i]] <- density(Diff_nonmat_train_cs[[i]], kernel = "epanechnikov", bw=5)
#     kernel_nonmat_cs[[i]] <- density(Diff_nonmat_train_cs[[i]])
#   }
#   
#   else
#   {
#     kernel_mat_cs$x[[i]] <- NA
#     kernel_mat_cs$y[[i]] <- NA
#     kernel_nonmat_cs$x[[i]] <-  NA
#     kernel_nonmat_cs$y[[i]] <-  NA
#   }
#   
# }

# plot(kernel_nonmat_cs[[1]]$x, kernel_nonmat_cs[[1]]$y, type="l", col="red" )
# par(new=TRUE)
# plot(kernel_mat_cs[[1]]$x, kernel_mat_cs[[1]]$y , type="l", col="green", add=TRUE )

q=13
plot(kernel_sig_mat_c[[q]]$x, kernel_sig_mat_c[[q]]$y, type="l", col="red" )
par(new=TRUE)
plot(kernel_sig_nonmat_c[[q]]$x, kernel_sig_nonmat_c[[q]]$y , type="l", col="green", add=TRUE )



###histogram with non_normalized data



hist_mat_c <- list()
hist_nonmat_c <- list()
histdensity_c <- list()
multiplier_hist_mat_c <- list()
density_hist_mat_c <- list()
density_smooth_hist_mat_c <- list()

multiplier_hist_nonmat_c <- list()
density_hist_nonmat_c <- list()
density_smooth_hist_nonmat_c <- list()

min <- vector()
max <- vector()

diffseq_mat_c <- list ()   
normal_mat_c <- list()
diffseq_nonmat_c <- list ()   
normal_nonmat_c <- list()

# #test
# myhist <- hist(Diff_mat_train_c[[3]] )
# multiplier <- (myhist$counts / sum(myhist$counts) ) / myhist$density
# mydensity <- density(Diff_mat_train_c[[3]])
# mydensity$y <- mydensity$y * multiplier[1]
# 
# myhist$density <- myhist$counts  / sum(myhist$counts)
# 
# plot(myhist , freq=FALSE)
# lines(mydensity)

interval  <- c(200000, 1, 1, 0.5, 0.5, 0.5, 0.5,  NA, NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5, 0.5, 0.5,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.5, 1 )

for ( i in 1 : length(Diff_mat_train_c)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min[i] <- round_any (min(Diff_nonmat_train_c[[i]] , Diff_mat_train_c[[i]]) , interval[i], f= floor)
    max[i] <- round_any (max(Diff_nonmat_train_c[[i]] , Diff_mat_train_c[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_c[[i]] <- hist(Diff_mat_train_c[[i]],  breaks=seq(min[i] ,max[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_c[[i]] <- ( hist_mat_c[[i]]$counts / sum( hist_mat_c[[i]]$counts)) / hist_mat_c[[i]]$density
    density_hist_mat_c[[i]] <- density (Diff_mat_train_c[[i]]) 
    density_hist_mat_c[[i]]$y <- density_hist_mat_c[[i]]$y *
      multiplier_hist_mat_c[[i]][ which.min(is.na( multiplier_hist_mat_c[[i]] ) ) ]
    hist_mat_c[[i]]$density <-  hist_mat_c[[i]]$counts / sum(hist_mat_c[[i]]$counts )   
    density_smooth_hist_mat_c[[i]] <-  smooth.spline(density_hist_mat_c[[i]]$x, density_hist_mat_c[[i]]$y,spar=0.8)
    density_smooth_hist_mat_c[[i]]$y [density_smooth_hist_mat_c[[i]]$y < 0] <- 0
    
    
    hist_nonmat_c[[i]] <- hist(Diff_nonmat_train_c[[i]],  breaks=seq(min[i],max[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_c[[i]] <- ( hist_nonmat_c[[i]]$counts / sum( hist_nonmat_c[[i]]$counts)) / hist_nonmat_c[[i]]$density
    density_hist_nonmat_c[[i]] <- density (Diff_nonmat_train_c[[i]]) 
    density_hist_nonmat_c[[i]]$y <- density_hist_nonmat_c[[i]]$y *
      multiplier_hist_nonmat_c[[i]][ which.min(is.na( multiplier_hist_nonmat_c[[i]] ) ) ]
    hist_nonmat_c[[i]]$density <-  hist_nonmat_c[[i]]$counts / sum(hist_nonmat_c[[i]]$counts )
    density_smooth_hist_nonmat_c[[i]] <-  smooth.spline(density_hist_nonmat_c[[i]]$x, density_hist_nonmat_c[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_c[[i]]$y [density_smooth_hist_nonmat_c[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_c[[i]] <- seq(min (Diff_nonmat_train_c[[i]] ) , max( Diff_nonmat_train_c[[i]]) , length.out=100)  
    diffseq_nonmat_c[[i]] <- seq(min (Diff_nonmat_train_c[[i]] ) , max( Diff_nonmat_train_c[[i]]) , length.out=100)  
    normal_mat_c[[i]] <- dnorm (x=diffseq_mat_c[[i]] , mean= mean (Diff_mat_train_c[[i]]) , sd = sd(Diff_mat_train_c[[i]]) )
    normal_nonmat_c[[i]] <- dnorm (x=diffseq_nonmat_c[[i]] , mean= mean (Diff_nonmat_train_c[[i]]) , sd = sd(Diff_nonmat_train_c[[i]]) )
    
    
    #histogram
    histdensity_c[[i]] <- cbind(hist_mat_c[[i]]$mids - interval[i]/2, hist_mat_c[[i]]$mids + interval[i]/2 , 
                                hist_mat_c[[i]]$counts / sum(hist_mat_c[[i]]$counts) ,  
                                hist_nonmat_c[[i]]$counts / sum(hist_nonmat_c[[i]]$counts) ) 
    
    hist_mat_c[[i]]$counts <-  hist_mat_c[[i]]$counts / sum ( hist_mat_c[[i]]$counts)
    hist_nonmat_c[[i]]$counts <-  hist_nonmat_c[[i]]$counts / sum ( hist_nonmat_c[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_c[[i]] <- NA
    hist_nonmat_c[[i]] <- NA
    histdensity_c[[i]] <- NA
  }
  
}


# signature

for ( i in 1: 50 )
{
  if (sum ( Diff_sig_mat_train[[i]]) != 0) {
    
    kernel_sig_mat_c[[i]] <- density(Diff_sig_mat_train[[i]], kernel = "epanechnikov")   
    kernel_sig_nonmat_c[[i]] <- density(Diff_sig_nonmat_train[[i]], kernel = "epanechnikov")
  }
  
  else
  {
    kernel_sig_mat_c$x[[i]] <- NA
    kernel_sig_mat_c$y[[i]] <- NA
    kernel_sig_nonmat_c$x[[i]] <-  NA
    kernel_sig_nonmat_c$y[[i]] <-  NA
  }
}



sigweight  <- rep(0.01, 50)

min_sig <- vector()
max_sig <- vector()

hist_mat_c_sig <- list()
hist_nonmat_c_sig <- list()
histdensity_c_sig <- list()
multiplier_hist_mat_c_sig <- list()
density_hist_mat_c_sig <- list()
density_smooth_hist_mat_c_sig <- list()

multiplier_hist_nonmat_c_sig <- list()
density_hist_nonmat_c_sig <- list()
density_smooth_hist_nonmat_c_sig <- list()


diffseq_mat_c_sig <- list ()   
normal_mat_c_sig <- list()
diffseq_nonmat_c_sig <- list ()   
normal_nonmat_c_sig <- list()

for ( i in 1 : length(sigweight )  ){
  
  
  min_sig[i] <- round_any (min( Diff_sig_mat_train[[i]] ,  Diff_sig_nonmat_train[[i]] ) , sigweight[i], f= floor)
  max_sig[i] <- round_any (max( Diff_sig_mat_train[[i]] ,  Diff_sig_nonmat_train[[i]] ) , sigweight[i], f= ceiling)
  
  # non parametric
  hist_mat_c_sig[[i]] <- hist(Diff_sig_mat_train[[i]],  breaks=seq(min_sig[i] ,max_sig[i], by=sigweight[i]), plot=FALSE)
  multiplier_hist_mat_c_sig[[i]] <- ( hist_mat_c_sig[[i]]$counts / sum( hist_mat_c_sig[[i]]$counts)) / hist_mat_c_sig[[i]]$density
  density_hist_mat_c_sig[[i]] <- density (Diff_sig_mat_train[[i]]) 
  density_hist_mat_c_sig[[i]]$y <- density_hist_mat_c_sig[[i]]$y *
    multiplier_hist_mat_c_sig[[i]][ which.min(is.na( multiplier_hist_mat_c_sig[[i]] ) ) ]
  hist_mat_c_sig[[i]]$density <-  hist_mat_c_sig[[i]]$counts / sum(hist_mat_c_sig[[i]]$counts )   
  density_smooth_hist_mat_c_sig[[i]] <-  smooth.spline(density_hist_mat_c_sig[[i]]$x, density_hist_mat_c_sig[[i]]$y,spar=0.8)
  density_smooth_hist_mat_c_sig[[i]]$y [density_smooth_hist_mat_c_sig[[i]]$y < 0] <- 0
  
  
  hist_nonmat_c_sig[[i]] <- hist(Diff_sig_nonmat_train[[i]],  breaks=seq(min_sig[i],max_sig[i],by=sigweight[i]) ,  plot=FALSE)
  multiplier_hist_nonmat_c_sig[[i]] <- ( hist_nonmat_c_sig[[i]]$counts / sum( hist_nonmat_c_sig[[i]]$counts)) / hist_nonmat_c_sig[[i]]$density
  density_hist_nonmat_c_sig[[i]] <- density (Diff_sig_nonmat_train[[i]]) 
  density_hist_nonmat_c_sig[[i]]$y <- density_hist_nonmat_c_sig[[i]]$y *
    multiplier_hist_nonmat_c_sig[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig[[i]] ) ) ]
  hist_nonmat_c_sig[[i]]$density <-  hist_nonmat_c_sig[[i]]$counts / sum(hist_nonmat_c_sig[[i]]$counts )
  density_smooth_hist_nonmat_c_sig[[i]] <-  smooth.spline(density_hist_nonmat_c_sig[[i]]$x, density_hist_nonmat_c_sig[[i]]$y,spar=0.8)
  density_smooth_hist_nonmat_c_sig[[i]]$y [density_smooth_hist_nonmat_c_sig[[i]]$y < 0] <- 0 
  
  # normal distribution
  diffseq_mat_c_sig[[i]] <- seq(min (Diff_sig_nonmat_train[[i]] , Diff_sig_mat_train[[i]] ) , max( Diff_sig_nonmat_train[[i]], Diff_sig_mat_train[[i]]) , length.out=100)  
  diffseq_nonmat_c_sig[[i]] <- seq(min (Diff_sig_nonmat_train[[i]], Diff_sig_mat_train[[i]] ) , max( Diff_sig_nonmat_train[[i]], Diff_sig_mat_train[[i]]) , length.out=100)  
  normal_mat_c_sig[[i]] <- dnorm (x=diffseq_mat_c_sig[[i]] , mean= mean (Diff_sig_mat_train[[i]]) , sd = sd(Diff_sig_mat_train[[i]]) )
  normal_nonmat_c_sig[[i]] <- dnorm (x=diffseq_nonmat_c_sig[[i]] , mean= mean (Diff_sig_nonmat_train[[i]]) , sd = sd(Diff_sig_nonmat_train[[i]]) )
  
  
  #histogram
  histdensity_c_sig[[i]] <- cbind(hist_mat_c_sig[[i]]$mids - sigweight[i]/2, hist_mat_c_sig[[i]]$mids + sigweight[i]/2 , 
                                  hist_mat_c_sig[[i]]$counts / sum(hist_mat_c_sig[[i]]$counts) ,  
                                  hist_nonmat_c_sig[[i]]$counts / sum(hist_nonmat_c_sig[[i]]$counts) ) 
  
  hist_mat_c_sig[[i]]$counts <-  hist_mat_c_sig[[i]]$counts / sum ( hist_mat_c_sig[[i]]$counts)
  hist_nonmat_c_sig[[i]]$counts <-  hist_nonmat_c_sig[[i]]$counts / sum ( hist_nonmat_c_sig[[i]]$counts)
  
  
}


# histogram and normal kernal - normalized data


hist_mat_n <- list()
hist_nonmat_n <- list()
histdensity_n <- list()
multiplier_hist_mat_n <- list()
density_hist_mat_n <- list()
density_smooth_hist_mat_n <- list()

multiplier_hist_nonmat_n <- list()
density_hist_nonmat_n <- list()
density_smooth_hist_nonmat_n <- list()

min <- vector()
max <- vector()

diffseq_mat_n <- list ()   
normal_mat_n <- list()
diffseq_nonmat_n <- list ()   
normal_nonmat_n <- list()



interval  <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,  NA, NA, NA, NA, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,0.1, 0.1, 0.1, 0.1,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.1, 0.1 )

for ( i in 1 : length(Diff_mat_train_n)  ){
  
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ # utc
    
    min[i] <- round_any (min(Diff_nonmat_train_n[[i]] , Diff_mat_train_n[[i]]) , interval[i], f= floor)
    max[i] <- round_any (max(Diff_nonmat_train_n[[i]] , Diff_mat_train_n[[i]]) , interval[i], f= ceiling)
    
    # non parametric
    hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]],  breaks=seq(min[i] ,max[i], by=interval[i]), plot=FALSE)
    multiplier_hist_mat_n[[i]] <- ( hist_mat_n[[i]]$counts / sum( hist_mat_n[[i]]$counts)) / hist_mat_n[[i]]$density
    density_hist_mat_n[[i]] <- density (Diff_mat_train_n[[i]]) 
    density_hist_mat_n[[i]]$y <- density_hist_mat_n[[i]]$y *
      multiplier_hist_mat_n[[i]][ which.min(is.na( multiplier_hist_mat_n[[i]] ) ) ]
    hist_mat_n[[i]]$density <-  hist_mat_n[[i]]$counts / sum(hist_mat_n[[i]]$counts )   
    density_smooth_hist_mat_n[[i]] <-  smooth.spline(density_hist_mat_n[[i]]$x, density_hist_mat_n[[i]]$y,spar=0.8)
    density_smooth_hist_mat_n[[i]]$y [density_smooth_hist_mat_n[[i]]$y < 0] <- 0
    
    
    hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],  breaks=seq(min[i],max[i],by=interval[i]) ,  plot=FALSE)
    multiplier_hist_nonmat_n[[i]] <- ( hist_nonmat_n[[i]]$counts / sum( hist_nonmat_n[[i]]$counts)) / hist_nonmat_n[[i]]$density
    density_hist_nonmat_n[[i]] <- density (Diff_nonmat_train_n[[i]]) 
    density_hist_nonmat_n[[i]]$y <- density_hist_nonmat_n[[i]]$y *
      multiplier_hist_nonmat_n[[i]][ which.min(is.na( multiplier_hist_nonmat_n[[i]] ) ) ]
    hist_nonmat_n[[i]]$density <-  hist_nonmat_n[[i]]$counts / sum(hist_nonmat_n[[i]]$counts )
    density_smooth_hist_nonmat_n[[i]] <-  smooth.spline(density_hist_nonmat_n[[i]]$x, density_hist_nonmat_n[[i]]$y,spar=0.8)
    density_smooth_hist_nonmat_n[[i]]$y [density_smooth_hist_nonmat_n[[i]]$y < 0] <- 0 
    
    # normal distribution
    diffseq_mat_n[[i]] <- seq(min (Diff_mat_train_n[[i]] ) , max( Diff_mat_train_n[[i]]) , length.out=100)  
    diffseq_nonmat_n[[i]] <- seq(min (Diff_nonmat_train_n[[i]] ) , max( Diff_nonmat_train_n[[i]]) , length.out=100)  
    normal_mat_n[[i]] <- dnorm (x=diffseq_mat_n[[i]] , mean= mean (Diff_mat_train_n[[i]]) , sd = sd(Diff_mat_train_n[[i]]) )
    normal_mat_n[[i]] <- dnorm (x=diffseq_mat_n[[i]] , mean= mean (Diff_mat_train_n[[i]]) , sd = sd(Diff_mat_train_n[[i]]) )
    normal_nonmat_n[[i]] <- dnorm (x=diffseq_nonmat_n[[i]] , mean= mean (Diff_nonmat_train_n[[i]]) , sd = sd(Diff_nonmat_train_n[[i]]) )
    
    
    #histogram
    histdensity_n[[i]] <- cbind(hist_mat_n[[i]]$mids - interval[i]/2, hist_mat_n[[i]]$mids + interval[i]/2 , 
                                hist_mat_n[[i]]$counts / sum(hist_mat_n[[i]]$counts) ,  
                                hist_nonmat_n[[i]]$counts / sum(hist_nonmat_n[[i]]$counts) ) 
    
    hist_mat_n[[i]]$counts <-  hist_mat_n[[i]]$counts / sum ( hist_mat_n[[i]]$counts)
    hist_nonmat_n[[i]]$counts <-  hist_nonmat_n[[i]]$counts / sum ( hist_nonmat_n[[i]]$counts)
    
  }
  
  else if ( i %in% c(8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
    hist_mat_n[[i]] <- NA
    hist_nonmat_n[[i]] <- NA
    histdensity_n[[i]] <- NA
  }
  
}



# clustering - SIG
# variance

# v1
stdmat_sig <- vector()
stdnonmat_sig <- vector()
for ( i in 1 : length(sigweight )  ){
  stdmat_sig[i] = sd(Diff_sig_mat_train[[i]]) 
  stdnonmat_sig[i] = sd(Diff_sig_nonmat_train[[i]])
}

stdall_sig <- vector()
stdall_sig <- c(stdmat_sig, stdnonmat_sig)
 
#v2
stdmat_sig <- vector()
stdnonmat_sig <- vector()
for ( i in 1 : length(sigweight )  ){
  stdmat_sig[i] = sd(Diff_sig_mat_train[[i]]) / mean(abs (Diff_sig_mat_train[[i]]) )
  stdnonmat_sig[i] = sd(Diff_sig_nonmat_train[[i]]) / mean(abs (Diff_sig_nonmat_train[[i]]) )
}

stdall_sig <- vector()
stdall_sig <- c(stdmat_sig, stdnonmat_sig)

#kmeans
# install.packages("kmeans")
# library(kmeans)
# 
# kmeansAIC = function(fit){
#   
#   m = ncol(fit$centers)
#   n = length(fit$cluster)
#   k = nrow(fit$centers)
#   D = fit$tot.withinss
#   return(data.frame(AIC = D + 2*m*k,
#                     BIC = D + log(n)*m*k))
# }

X =  sort(stdmat_sig)
fit <- kmeans(X, 2)
# kmeansAIC(fit)
plot(X, col = fit$cluster)
stdthreshold_sig <- 1.18
# stdthreshold_sig <- 0.07

# ks test
ttestpvalue_sig <- vector()
for ( i in 1 : length(sigweight )  ){
  ttestpvalue_sig[i]<- ks.test( normal_mat_c_sig[[i]],  normal_nonmat_c_sig[[i]])$p.value
}

distidx <- which(ttestpvalue_sig < 0.05)

sigfeatidx <- vector()
for ( i in 1: length(sigweight)) {
  if ( i %in% distidx){
    if (stdmat_sig[i] > stdthreshold_sig & stdnonmat_sig[i] > stdthreshold_sig ){
      sigfeatidx[i] <- 1
    }
    if (stdmat_sig[i] < stdthreshold_sig & stdnonmat_sig[i] < stdthreshold_sig ){
      sigfeatidx[i] <- 2
    }
    if (stdmat_sig[i] < stdthreshold_sig & stdnonmat_sig[i] > stdthreshold_sig ){
      sigfeatidx[i] <- 3
    }
    if (stdmat_sig[i] > stdthreshold_sig & stdnonmat_sig[i] < stdthreshold_sig ){
      sigfeatidx[i] <- 4
    }
    
  }
  
  else {
    sigfeatidx [i] <- 0  # mat = nonmat
  }
}


# WIM


# ks test
ttestpvalue_wim <- vector()
for ( i in 1 : 31 ){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    ttestpvalue_wim[i]<- ks.test( normal_mat_c[[i]],  normal_nonmat_c[[i]])$p.value
  }
  else ttestpvalue_wim[i] <- NA
}


# ttestpvalue_wim <- vector()
# for ( i in 1 : 31 ){
#   if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
#     ttestpvalue_wim[i]<- ks.test( normal_mat_n[[i]],  normal_nonmat_n[[i]])$p.value
#   }
#   else ttestpvalue_wim[i] <- NA
# }


distidx <- which(ttestpvalue_wim > 0.05)


# wim kmeans version 1
stdmat_wim_sp <- vector()
stdnonmat_wim_sp <- vector()
stdmat_wim_wt <- vector()
stdnonmat_wim_wt <- vector()
stdmat_wim_mc <- vector()
stdnonmat_wim_mc <- vector()

for ( i in 1 : 31  ){
  if (i %in% c(1,2,3,30,31) ){
    stdmat_wim_mc[i] = sd(Diff_mat_train_n[[i]]) / mean(Diff_mat_train_n[[i]])
    stdnonmat_wim_mc[i] = sd(Diff_nonmat_train_n[[i]]) / mean(Diff_nonmat_train_n[[i]])
  }
  
  if (i %in% c(4,5,6,7) ){ 
  stdmat_wim_sp[i] = sd(Diff_mat_train_n[[i]]) / mean(Diff_mat_train_n[[i]])
  stdnonmat_wim_sp[i] = sd(Diff_nonmat_train_n[[i]]) / mean(Diff_nonmat_train_n[[i]])
  }
  
  if (i %in% c(12,13,14,15,16,17,18,19,20,21) ){ 
    stdmat_wim_wt[i] = sd(Diff_mat_train_n[[i]]) / mean(Diff_mat_train_n[[i]])
    stdnonmat_wim_wt[i] = sd(Diff_nonmat_train_n[[i]]) / mean(Diff_nonmat_train_n[[i]])
  }
  
}

stdmat_wim_all <- vector()
stdnonmat_wim_all <- vector()

for ( i in 1 : 31  ){
   if (i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
     stdmat_wim_all[i] <-  sd(Diff_mat_train_n[[i]]) / mean(Diff_mat_train_n[[i]])
     stdnonmat_wim_all[i] <- sd(Diff_nonmat_train_n[[i]]) / mean(Diff_nonmat_train_n[[i]])
   }
}


stdall_wim_all <- vector()
stdall_wim_all <- c( stdmat_wim_mc ,  stdnonmat_wim_mc , stdmat_wim_sp, stdnonmat_wim_sp,
                     stdmat_wim_wt, stdnonmat_wim_wt)

X =  sort(stdall_wim_all )
fit <- kmeans(X, 2)
plot(X, col = fit$cluster)


stdthreshold_wim_all <- 0.48



wimfeatidx <- vector()
for ( i in 1: 30) {
  if (i %in% distidx){
    wimfeatidx[i] <- 0
  }
  else{
    
    if ( i %in% c(1,2,3, 4,5,6,7 , 12,13, 14,15,16,17,18,19,20,21, 30,31) ){
      
      if (stdmat_wim_all[i] > stdthreshold_wim_all & stdnonmat_wim_all[i] > stdthreshold_wim_all ){
        wimfeatidx[i] <- 1
      }
      if (stdmat_wim_all[i] < stdthreshold_wim_all & stdnonmat_wim_all[i] < stdthreshold_wim_all ){
        wimfeatidx[i] <- 2
      }
      if (stdmat_wim_all[i] < stdthreshold_wim_all & stdnonmat_wim_all[i] > stdthreshold_wim_all ){
        wimfeatidx[i] <- 3
      }
      if (stdmat_wim_all[i] > stdthreshold_wim_all & stdnonmat_wim_all[i] <  stdthreshold_wim_all ){
        wimfeatidx[i] <- 4
      }
      
    }
    
    else {
      wimfeatidx[i] <- 0
    }
  }
}
wimfeatidx[31] <- 1




# # wim kmeans version 2
# stdall_wim_sp <- vector()
# stdall_wim_sp <- c(stdmat_wim_sp, stdnonmat_wim_sp)
# 
# X =  sort(stdall_wim_sp)
# fit <- kmeans(X, 2)
# # kmeansAIC(fit)
# plot(X, col = fit$cluster)
# 
# stdall_wim_wt <- vector()
# stdall_wim_wt <- c(stdmat_wim_wt, stdnonmat_wim_wt)
# X =  sort(stdall_wim_wt)
# fit <- kmeans(X, 2)
# plot(X, col = fit$cluster)
# 
# stdthreshold_wim_sp <- 0.20
# stdthreshold_wim_wt <- 0.21
# 
# 
# 
# wimfeatidx <- vector()
# for ( i in 1: 31) {
#   if (i %in% distidx){
#     wimfeatidx[i] <- 0
#   }
#   else{
#     
#         if ( i %in% c(4,5,6,7) ){
#       
#           if (stdmat_wim_sp[i] > stdthreshold_wim_sp & stdnonmat_wim_sp[i] > stdthreshold_wim_sp ){
#             wimfeatidx[i] <- 1
#           }
#           if (stdmat_wim_sp[i] < stdthreshold_wim_sp & stdnonmat_wim_sp[i] < stdthreshold_wim_sp ){
#             wimfeatidx[i] <- 2
#           }
#           if (stdmat_wim_sp[i] < stdthreshold_wim_sp & stdnonmat_wim_sp[i] > stdthreshold_wim_sp ){
#             wimfeatidx[i] <- 3
#           }
#           if (stdmat_wim_sp[i] > stdthreshold_wim_sp & stdnonmat_wim_sp[i] <  stdthreshold_wim_sp ){
#             wimfeatidx[i] <- 4
#           }
#           
#         }
#         
#         if ( i %in% c(12,13, 14,15,16,17,18,19,20,21) ){ 
#           
#           if (stdmat_wim_wt[i] > stdthreshold_wim_wt & stdnonmat_wim_wt[i] > stdthreshold_wim_wt ){
#             wimfeatidx[i] <- 1
#           }
#           if (stdmat_wim_wt[i] < stdthreshold_wim_wt & stdnonmat_wim_wt[i] < stdthreshold_wim_wt ){
#             wimfeatidx[i] <- 2
#           }
#           if (stdmat_wim_wt[i] < stdthreshold_wim_wt & stdnonmat_wim_wt[i] > stdthreshold_wim_wt ){
#             wimfeatidx[i] <- 3
#           }
#           if (stdmat_wim_wt[i] > stdthreshold_wim_wt & stdnonmat_wim_wt[i] < stdthreshold_wim_wt ){
#             wimfeatidx[i] <- 4
#           }
#           
#           
#         }
#         
#         if ( i %in% c(1,2,3,30,31) ){ 
#         
#             wimfeatidx[i] <- 3
#           
#         }
#         
#         
#         if ( i %in% c(8,9,10,11, 22,23,24,25,26,27,28,29) ){ 
#           wimfeatidx [i] <- 0  # mat = nonmat
#         }
#   }
# }


# IG

# wim attribute weight
# WIMWttemp <- data.frame()
# 
# for (i in 1:length(ResultMisMatching_train[,1])){
#   
#   if ( as.numeric (ResultMisMatching_train [i,1] ) == 9 ) { 
#     if ( ResultMisMatching_train[i,2] == ResultMisMatching_train[i,4] ) {
#       
#       for (m in 1: 30) { 
#         WIMWttemp[i,m] <- Upcandidates_attribute_train_missing[[i]][1,m]   
#       }
#       WIMWttemp[i,m+1] <- "1"
#     }
#     
#     else {
#       for (m in 1: 30) { 
#         WIMWttemp[i,m] <- Upcandidates_attribute_train_missing[[i]][1,m]   
#       }
#       WIMWttemp[i,m+1] <- "2"
#     }
#   }
# }

WIMWttemp <- data.frame()

for (j in 1:length(Diff_mat_train[[1]])){
   
   for (i in 1: 30) { 
     WIMWttemp[j,i] <-Diff_mat_train[[i]][j] 
   }
     WIMWttemp[j,i+1] <- "1"
   
}

for (jj in 1:length(Diff_nonmat_train[[1]])){
  
  for (i in 1: 30) { 
    WIMWttemp[j+jj,i] <-Diff_nonmat_train[[i]][jj] 
  }
  WIMWttemp[j+jj,i+1] <- "2"
  
}
    

WIMWttemp <- na.omit(WIMWttemp)


WIMWttemp <- WIMWttemp[-8:-11]
WIMWttemp <- WIMWttemp[-18:-25]
View(WIMWttemp)
colnames(WIMWttemp) <- c("time", "length" , "weight", "axsp12" , "axsp23" , "axsp34", "axsp45", 
                         "axwt1l" , "axwt1r" ,  "axwt2l" , "axwt2r" , "axwt3l" , "axwt3r" , "axwt4l" , "axwt4r" ,
                         "axwt5l" , "axwt5r" , "duration" , "matchidx")



# evaluator <- function(subset) {
#   #k-fold cross validation
#   k <- 5
#   splits <- runif(nrow(WIMWttemp))
#   results = sapply(1:k, function(i) {
#     test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#     train.idx <- !test.idx
#     test <- WIMWttemp[test.idx, , drop=FALSE]
#     train <- WIMWttemp[train.idx, , drop=FALSE]
#     tree <- rpart(as.simple.formula(subset, "matchidx"), train)
#     error.rate = sum(test$ idx!= predict(tree, test, type="c")) / nrow(test)
#     return(1 - error.rate)
#   })
#   print(subset)
#   print(mean(results))
#   return(mean(results))
# }


# subsetwim <- exhaustive.search( names(WIMWttemp)[-20], evaluator)
# f <- as.simple.formula(subset, "matchidx")
# print(f)



wimIGweights <- information.gain(matchidx~., WIMWttemp)
print(wimIGweights)
nIG <- sum(wimIGweights > 0)
subset <- cutoff.k(wimIGweights, nIG)
f <- as.simple.formula(subset, "matchidx")
print(f)

# sig weight -IG



SIGWttemp2 <- data.frame()

for (j in 1:length(sig_mat_train)){
  
  for (i in 1: 50) { 
    SIGWttemp2[j,i] <-  Diff_sig_mat_train [[i]][j] 
  }
  SIGWttemp2[j,i+1] <- "1"
  
}

for (jj in 1:length(sig_nonmat_train)){
  
  for (i in 1: 50) { 
    SIGWttemp2[j+jj,i] <-  Diff_sig_nonmat_train[[i]][jj] 
  }
  SIGWttemp2[j+jj,i+1] <- "2"
  
}


SIGWttemp2 <- na.omit(SIGWttemp2)
View(SIGWttemp2)

colnames(SIGWttemp2)[51] <- c("sigidx")
SIGIGweights <- information.gain(sigidx~., SIGWttemp2)
print(SIGIGweights)
nIG <- sum(SIGIGweights > 0)
subset <- cutoff.k(SIGIGweights, nIG)
f <- as.simple.formula(subset, "sigidx")
print(f)



#sample plot
# library(ggplot2)
i=19
plot(range(min[i], max[i]), range(density_hist_mat_c[[i]]$y))
lines(density_hist_mat_c[[i]] )
lines(density_smooth_hist_mat_c[[i]], col="blue")
plot(density_smooth_hist_mat_c[[i]]$x, density_smooth_hist_mat_c[[i]]$y)
plot(density_smooth_hist_mat_c[[i]]$data$x, density_smooth_hist_mat_c[[i]]$data$y)

plot(range(min[i], max[i]), range(density_hist_mat_c[[i]]$y))
lines(density_hist_nonmat_c[[i]] )
lines(density_smooth_hist_nonmat_c[[i]], col="blue")
plot(density_smooth_hist_nonmat_c[[i]]$x, density_smooth_hist_nonmat_c[[i]]$y)
plot(density_smooth_hist_nonmat_c[[i]]$data$x, density_smooth_hist_nonmat_c[[i]]$data$y)


i=19
plot(range(min[i], max[i]), range(density_hist_mat_n[[i]]$y))
lines(density_hist_mat_n[[i]] )
lines(density_smooth_hist_mat_n[[i]], col="blue")
plot(density_smooth_hist_mat_n[[i]]$x, density_smooth_hist_mat_n[[i]]$y)
plot(density_smooth_hist_mat_n[[i]]$data$x, density_smooth_hist_mat_n[[i]]$data$y)

plot(range(min[i], max[i]), range(density_hist_mat_n[[i]]$y))
lines(density_hist_nonmat_n[[i]] )
lines(density_smooth_hist_nonmat_n[[i]], col="blue")
plot(density_smooth_hist_nonmat_n[[i]]$x, density_smooth_hist_nonmat_n[[i]]$y)
plot(density_smooth_hist_nonmat_n[[i]]$data$x, density_smooth_hist_nonmat_n[[i]]$data$y)



#plot a = non-parametric but smoothing data
a_mat <- data.frame(density_smooth_hist_mat_c[[i]]$x , density_smooth_hist_mat_c[[i]]$y )
a_nonmat <- data.frame(density_smooth_hist_nonmat_c[[i]]$x, density_smooth_hist_nonmat_c[[i]]$y)
ggplot() +
  geom_line(data=a_mat, aes(x=a_mat[,1] , y=a_mat[,2]) , color='green') + 
  geom_line(data=a_nonmat, aes(x=a_nonmat[,1] , y=a_nonmat[,2]) , color='red') 


a_mat <- data.frame(density_smooth_hist_mat_n[[i]]$x , density_smooth_hist_mat_n[[i]]$y )
a_nonmat <- data.frame(density_smooth_hist_nonmat_n[[i]]$x, density_smooth_hist_nonmat_n[[i]]$y)
ggplot() +
  geom_line(data=a_mat, aes(x=a_mat[,1] , y=a_mat[,2]) , color='green') + 
  geom_line(data=a_nonmat, aes(x=a_nonmat[,1] , y=a_nonmat[,2]) , color='red') 



# plot( diffseq_mat_c[[i]] ,
#       normal_mat_c[[i]]  * multiplier_hist_mat_c[[i]][ which.min(is.na( multiplier_hist_mat_c[[i]] ) ) ] , 
#       xlim=range(min[i] , max[i]), col="red") 
# 
# lines( diffseq_nonmat_c[[i]] ,
#       normal_nonmat_c[[i]]  * multiplier_hist_nonmat_c[[i]][ which.min(is.na( multiplier_hist_nonmat_c[[i]] ) ) ] ,
#       xlim=range(min[i] , max[i]), col="green" ) 

#plot b = parametric , Gaussian fitting data
b_mat <- data.frame(diffseq_mat_c[[i]], 
                    normal_mat_c[[i]]  * multiplier_hist_mat_c[[i]][ which.min(is.na( multiplier_hist_mat_c[[i]] ) ) ]  )
b_nonmat <- data.frame( diffseq_nonmat_c[[i]], 
                        normal_nonmat_c[[i]]  * multiplier_hist_nonmat_c[[i]][ which.min(is.na( multiplier_hist_nonmat_c[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='green') + 
  geom_line(data=b_nonmat, aes ( x=b_nonmat[,1] , y=b_nonmat[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat, aes ( x=a_mat[,1] , y=a_mat[,2]  ) , color='blue') + 
  geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='black') 

head(b_mat)

## normalized 
b_mat <- data.frame(diffseq_mat_n[[i]], 
                    normal_mat_n[[i]]  * multiplier_hist_mat_n[[i]][ which.min(is.na( multiplier_hist_mat_n[[i]] ) ) ]  )
b_nonmat <- data.frame( diffseq_nonmat_n[[i]], 
                        normal_nonmat_n[[i]]  * multiplier_hist_nonmat_n[[i]][ which.min(is.na( multiplier_hist_nonmat_n[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='green') + 
  geom_line(data=b_nonmat, aes ( x=b_nonmat[,1] , y=b_nonmat[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat, aes ( x=a_mat[,1] , y=a_mat[,2]  ) , color='blue') + 
  geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='black') 

head(b_mat)

# plot histogram
k <- 19
plot( hist_nonmat_c[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_c[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_c[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_c[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_c[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference - Nonnormalized data')

plot( hist_mat_c[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

#normalized
k <- 19
plot( hist_nonmat_n[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_n[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_n[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_n[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_n[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_n[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference - Nonnormalized data')

plot( hist_mat_n[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

## sig plot

#sig
plot(range(min_sig[i], max_sig[i]), range(density_hist_mat_c_sig[[i]]$y))
lines(density_hist_mat_c_sig[[i]] )
lines(density_smooth_hist_mat_c_sig[[i]], col="blue")
plot(density_smooth_hist_mat_c_sig[[i]]$x, density_smooth_hist_mat_c_sig[[i]]$y)
plot(density_smooth_hist_mat_c_sig[[i]]$data$x, density_smooth_hist_mat_c_sig[[i]]$data$y)

plot(range(min_sig[i], max_sig[i]), range(density_hist_mat_c_sig[[i]]$y))
lines(density_hist_nonmat_c_sig[[i]] )
lines(density_smooth_hist_nonmat_c_sig[[i]], col="blue")
plot(density_smooth_hist_nonmat_c_sig[[i]]$x, density_smooth_hist_nonmat_c_sig[[i]]$y)
plot(density_smooth_hist_nonmat_c_sig[[i]]$data$x, density_smooth_hist_nonmat_c_sig[[i]]$data$y)


#plot a = non-parametric but smoothing data
install.packages("grid")
install.packages("gridExtra")
library(ggplot2)
library(grid)
library(gridExtra)
library(multiplot)



setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/WIMFeature2") 

for (i in 1: 50){
 pt  <- ggplot()+
    geom_line(data= data.frame(density_smooth_hist_mat_c_sig[[i]]$x , density_smooth_hist_mat_c_sig[[i]]$y ),
              aes(x= data.frame(density_smooth_hist_mat_c_sig[[i]]$x , density_smooth_hist_mat_c_sig[[i]]$y )[,1] ,
                  y= data.frame(density_smooth_hist_mat_c_sig[[i]]$x , density_smooth_hist_mat_c_sig[[i]]$y )[,2]) ,
              color='green') + 
    geom_line(data=data.frame(density_smooth_hist_nonmat_c_sig[[i]]$x, density_smooth_hist_nonmat_c_sig[[i]]$y),
              aes(x=data.frame(density_smooth_hist_nonmat_c_sig[[i]]$x, density_smooth_hist_nonmat_c_sig[[i]]$y)[,1] ,
                  y=data.frame(density_smooth_hist_nonmat_c_sig[[i]]$x, density_smooth_hist_nonmat_c_sig[[i]]$y)[,2]) , 
              color='red' ) + ggtitle(i) 
    
 ggsave(filename = paste("sig feature " , i, ".jpeg" , sep="") , plot = pt)
}






# sample code
# plots <- list()  # new empty list
# for (i in 1:6) {
#   p1 = qplot(1:10, rnorm(10), main = i)
#   plots[[i]] <- p1  # add each plot into plot list
# }

# multiplot(plotlist = plots, cols = 3)



#plot b = parametric , Gaussian fitting data

for (i in 1: 50){

  b_mat_sig <- data.frame(diffseq_mat_c_sig[[i]], 
                          normal_mat_c_sig[[i]]  * multiplier_hist_mat_c_sig[[i]][ which.min(is.na( multiplier_hist_mat_c_sig[[i]] ) ) ]  )
  b_nonmat_sig <- data.frame( diffseq_nonmat_c_sig[[i]], 
                              normal_nonmat_c_sig[[i]]  * multiplier_hist_nonmat_c_sig[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig[[i]] ) ) ]  )
  pt <- ggplot() + 
    geom_line(data=b_mat_sig, aes ( x=b_mat_sig[,1] , y=b_mat_sig[,2]  ) , color='green') +       
    geom_line(data=b_nonmat_sig, aes ( x=b_nonmat_sig[,1] , y=b_nonmat_sig[,2]  ) , color='red')  + ggtitle(i) 
  
  pt <-   pt + xlim(-0.4, 0.4)
   
  ggsave(filename = paste("sig feature - gaussian" , i, ".jpeg" , sep="") , plot = pt)
}


# plot histogram
k <- 19
plot( hist_nonmat_c_sig[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_sig_mat_train[[k]]) , sigweight[k], f= floor)  , 
                     round_any (min(Diff_sig_nonmat_train[[k]]) , sigweight[k], f= floor) ) ,
               max ( round_any (max(Diff_sig_mat_train[[k]]) , sigweight[k], f= ceiling) ,
                     round_any (max(Diff_sig_nonmat_train[[k]]) , sigweight[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_sig[[k]]$count )),
      freq=TRUE,
      xlab = 'sigfeature diff', ylab = 'Density', main = 'Histogram of Sig Feature Difference')

plot( hist_mat_c_sig[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)


# plot - wim 
for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
  
  b_mat <- data.frame(diffseq_mat_c[[i]], 
                          normal_mat_c[[i]]  * multiplier_hist_mat_c[[i]][ which.min(is.na( multiplier_hist_mat_c[[i]] ) ) ]  )
  b_nonmat <- data.frame( diffseq_nonmat_c[[i]], 
                              normal_nonmat_c[[i]]  * multiplier_hist_nonmat_c[[i]][ which.min(is.na( multiplier_hist_nonmat_c[[i]] ) ) ]  )
  pt <- ggplot() + 
    geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='green') +       
    geom_line(data=b_nonmat, aes ( x=b_nonmat[,1] , y=b_nonmat[,2]  ) , color='red')  + ggtitle(i) 
  
#   pt <-   pt + xlim(-0.4, 0.4)
  
  ggsave(filename = paste("wim feature - gaussian" , i, ".jpeg" , sep="") , plot = pt)
}
}


for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat <- data.frame(diffseq_mat_n[[i]], 
                        normal_mat_n[[i]]  * multiplier_hist_mat_n[[i]][ which.min(is.na( multiplier_hist_mat_n[[i]] ) ) ]  )
    b_nonmat <- data.frame( diffseq_nonmat_n[[i]], 
                            normal_nonmat_n[[i]]  * multiplier_hist_nonmat_n[[i]][ which.min(is.na( multiplier_hist_nonmat_n[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat, aes ( x=b_mat[,1] , y=b_mat[,2]  ) , color='green') +       
      geom_line(data=b_nonmat, aes ( x=b_nonmat[,1] , y=b_nonmat[,2]  ) , color='red')  + ggtitle(i) 
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature norm - gaussian" , i, ".jpeg" , sep="") , plot = pt)
  }
}


# http://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_05202015")
#############################################################################################
# end
