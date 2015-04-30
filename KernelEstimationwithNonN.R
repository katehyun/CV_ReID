rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData") 
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new_nonN.RData")
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all, sub_all_train, sub_all_test, sub_matching_train, sub_matching_test, 
   sub_nonmatching_train, sub_nonmatching_test)
options(scipen=999) # non scientific notation

library(MASS)
library(plyr)
library(ggplot2)
library(stringr)
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

sig_selected <- list()
for (i in 1:length(a_magdif)){
  
  a <- unlist(idx_magdif[i])
  sig_selected[i] <- sigfeature[[i]][a] 
  
}


# train (01/09)
DownheaderTrainIdx <- which (Downheader_new[,12] > utcbd )
DownheaderTestIdx <- which (Downheader_new[,12] < utcbd )
Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]
sigfeature_train <- sig_selected[DownheaderTrainIdx]
sigfeature_test <- sig_selected[DownheaderTestIdx]


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
# Diff_mat_train[[1]] <- na.omit (  abs(sub_matching_train [,3]  )) 
# Diff_nonmat_train[[1]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 

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
sig_mat_train   <- sigfeature_train[submatching_train_Idx]
sig_nonmat_train <- sigfeature_train[subnonmatching_train_Idx]
sig_mat_test <- sigfeature_test[submatching_test_Idx]
sig_nonmat_test <- sigfeature_test[subnonmatching_test_Idx]

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

# mzscoresIndex_mat[[1]] <- rbind (which ( unlist (mzscores_mat[[1]] ) > 3.5) , which( unlist (mzscores_mat[[1]] ) < -3.5  ) )
# mzscoresIndex_nonmat[[1]] <- rbind (which ( unlist (mzscores_nonmat[[1]] ) > 3.5) , which( unlist (mzscores_nonmat[[1]] ) < -3.5  ) )

 




for ( i in 1:30 )
{
 
  mzscoresIndex_mat[[i+1]] <- 
   which ( ( unlist (mzscores_mat[[i+1]]) < 3.5) & ( unlist (mzscores_mat[[i+1]] ) > -3.5  ) )
  mzscoresIndex_nonmat[[i+1]] <- 
   which ( ( unlist (mzscores_nonmat[[i+1]]) < 3.5) & ( unlist (mzscores_nonmat[[i+1]] ) > -3.5  ) )
  
#   mzscoresIndex_mat[[i+1]] <- 
#     which ( ( unlist (mzscores_mat[[i+1]]) < 3.5) & (unlist (mzscores_mat[[i+1]] ) > -3.5  ) ) 
#   mzscoresIndex_nonmat[[i+1]] <- 
#     rbind (which ( unlist (mzscores_nonmat[[i+1]] ) > 3.5) , which( unlist (mzscores_nonmat[[i+1]] ) < -3.5  ) )
  
  
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


# scailized 

max_all <- vector()
min_all <- vector()
max_all[1] <- max( unlist( Diff_mat_train_c[[1]] ) ,  unlist(Diff_nonmat_train_c[[1]] )  )
min_all[1] <- min( unlist( Diff_mat_train_c[[1]] ) ,  unlist(Diff_nonmat_train_c[[1]] )  )

for ( i in 1:30 )
{
  max_all[i+1] <-  max( unlist( Diff_mat_train_c[[i+1]] ) ,  unlist(Diff_nonmat_train_c[[i+1]] )  )
  min_all[i+1] <-  min( unlist( Diff_mat_train_c[[i+1]] ) ,  unlist(Diff_nonmat_train_c[[i+1]] )  )
 
}


Diff_mat_train_cs <- list()
Diff_nonmat_train_cs <- list()
Diff_mat_train_cs[[1]] <- na.omit ( ( Diff_mat_train_c[[1]]  - min_all[1] ) / 
                                     ( max_all[1] - min_all[1] )  )
Diff_nonmat_train_cs[[1]] <- na.omit (  ( Diff_nonmat_train_c[[1]] - min_all[1] ) / 
                                         ( max_all[1] - min_all[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_cs[[i+1]] <- na.omit ( ( Diff_mat_train_c[[1+i]] - min_all[1+i] ) /  
                                         ( max_all[1+i] - min_all[1+i] )  )
  Diff_nonmat_train_cs[[i+1]] <- na.omit ( ( Diff_nonmat_train_c[[1+i]] -  min_all[1+i] ) /  
                                            ( max_all[1+i] - min_all[1+i] )  ) 
}

Diff_mat_train_ns <- list()
Diff_nonmat_train_ns <- list()
Diff_mat_train_ns[[1]] <- na.omit ( ( Diff_mat_train_n[[1]]  - min_all[1] ) / 
                                      ( max_all[1] - min_all[1] )  )
Diff_nonmat_train_ns[[1]] <- na.omit (  ( Diff_nonmat_train_n[[1]] - min_all[1] ) / 
                                          ( max_all[1] - min_all[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_ns[[i+1]] <- na.omit ( ( Diff_mat_train_n[[1+i]] - min_all[1+i] ) /  
                                          ( max_all[1+i] - min_all[1+i] )  )
  Diff_nonmat_train_ns[[i+1]] <- na.omit ( ( Diff_nonmat_train_n[[1+i]] -  min_all[1+i] ) /  
                                             ( max_all[1+i] - min_all[1+i] )  ) 
}



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

# to make non-zero data
minval <- 0

#non-normalized data

for (i in 1: length( Diff_mat_train_c) ){
  for (j in 1: length( Diff_mat_train_c[[i]] ) ){
    
    if ( length( Diff_mat_train_c[[i]])  > 0 ){
      
            if (Diff_mat_train_c[[i]][j] == 0 ){
              Diff_mat_train_c[[i]][j]  <- minval
            }
            
      if (Diff_mat_train_c[[i]][j] == 99999 ){
        Diff_mat_train_c[[i]][j] <- NA
      }
    }
  }
}



for (i in 1: length( Diff_nonmat_train_c) ){
  for (j in 1:length( Diff_nonmat_train_c[[i]] ) ){
    
    if ( length( Diff_nonmat_train_c[[i]])  > 0 ){
      
            if (Diff_nonmat_train_c[[i]][j] == 0 ){
              Diff_nonmat_train_c[[i]][j] <- minval
            }
            
            
      if  (Diff_nonmat_train_c[[i]][j] == 99999 )  {
        Diff_nonmat_train_c[[i]][j] <- NA
      }
      
    }
  }
}


#normalized data
for (i in 1: length( Diff_mat_train ) ){
  for (j in 1: length( Diff_mat_train[[i]] ) ){
    
    if ( length( Diff_mat_train_n[[i]])  > 0 ){
      
      if ((Diff_mat_train_n[[i]][j] == 0 ) && (!is.na(Diff_mat_train_n[[i]][j] ))){
        Diff_mat_train_n[[i]][j]  <- minval
      }
      
      if  ((Diff_mat_train_n[[i]][j] == 99999 ) && (!is.na(Diff_mat_train_n[[i]][j] ))){
        Diff_mat_train_n[[i]][j] <- NA
      }
      
    }   
  }
}



for (i in 1: length( Diff_nonmat_train_n ) ){
  for (j in 1:length( Diff_nonmat_train_n[[i]] ) ){
    
    if ( length( Diff_nonmat_train_n[[i]])  > 0 ){
      
      if ((Diff_nonmat_train_n[[i]][j] == 0 )&& (!is.na(Diff_nonmat_train_n[[i]][j] ))){
        Diff_nonmat_train_n[[i]][j] <- minval
      }
      
      else if  ((Diff_nonmat_train_n[[i]][j] == 99999 ) && (!is.na(Diff_nonmat_train_n[[i]][j] ))){
        Diff_nonmat_train_n[[i]][j] <- NA
      }
    }
  }
}


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

# scalized data - non normalized

for (i in 1: length( Diff_mat_train_cs) ){
  for (j in 1: length( Diff_mat_train_cs[[i]] ) ){
    
    if ( length( Diff_mat_train_cs[[i]])  > 0 ){
      
      if (Diff_mat_train_cs[[i]][j] == 0 ){
        Diff_mat_train_cs[[i]][j]  <- minval
      }
      
      if (Diff_mat_train_cs[[i]][j] == 99999 ){
        Diff_mat_train_cs[[i]][j] <- NA
      }
    }
  }
}



for (i in 1: length( Diff_nonmat_train_cs) ){
  for (j in 1:length( Diff_nonmat_train_cs[[i]] ) ){
    
    if ( length( Diff_nonmat_train_cs[[i]])  > 0 ){
      
      if (Diff_nonmat_train_cs[[i]][j] == 0 ){
        Diff_nonmat_train_cs[[i]][j] <- minval
      }
      
      
      if  (Diff_nonmat_train_cs[[i]][j] == 99999 )  {
        Diff_nonmat_train_cs[[i]][j] <- NA
      }
      
    }
  }
}


# scalized data - normalized data

for (i in 1: length( Diff_mat_train ) ){
  for (j in 1: length( Diff_mat_train[[i]] ) ){
    
    if ( length( Diff_mat_train_ns[[i]])  > 0 ){
      
      if ((Diff_mat_train_ns[[i]][j] == 0 ) && (!is.na(Diff_mat_train_ns[[i]][j] ))){
        Diff_mat_train_ns[[i]][j]  <- minval
      }
      
      if  ((Diff_mat_train_ns[[i]][j] == 99999 ) && (!is.na(Diff_mat_train_ns[[i]][j] ))){
        Diff_mat_train_ns[[i]][j] <- NA
      }
      
    }   
  }
}



for (i in 1: length( Diff_nonmat_train_ns ) ){
  for (j in 1:length( Diff_nonmat_train_ns[[i]] ) ){
    
    if ( length( Diff_nonmat_train_ns[[i]])  > 0 ){
      
      if ((Diff_nonmat_train_ns[[i]][j] == 0 )&& (!is.na(Diff_nonmat_train_ns[[i]][j] ))){
        Diff_nonmat_train_ns[[i]][j] <- minval
      }
      
      else if  ((Diff_nonmat_train_ns[[i]][j] == 99999 ) && (!is.na(Diff_nonmat_train_ns[[i]][j] ))){
        Diff_nonmat_train_ns[[i]][j] <- NA
      }
    }
  }
}


Diff_mat_train_cs[[1]] <- na.omit (  Diff_mat_train_cs[[1]]  )
Diff_nonmat_train_cs[[1]] <- na.omit (  Diff_nonmat_train_cs[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_cs[[i+1]] <- na.omit (  Diff_mat_train_cs[[1+i]]  )
  Diff_nonmat_train_cs[[i+1]] <- na.omit (  Diff_nonmat_train_cs[[1+i]]  ) 
}



Diff_mat_train_ns[[1]] <- na.omit (  Diff_mat_train_ns[[1]]  )
Diff_nonmat_train_ns[[1]] <- na.omit (  Diff_nonmat_train_ns[[1]]  ) 
for ( i in 1:30 )
{
  Diff_mat_train_ns[[i+1]] <- na.omit (  Diff_mat_train_ns[[1+i]]  )
  Diff_nonmat_train_ns[[i+1]] <- na.omit (  Diff_nonmat_train_ns[[1+i]]  ) 
}


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
    
    kernel_mat_n[[i]] <- density(Diff_mat_train_n[[i]], kernel = "epanechnikov", bw=2.1)
    kernel_nonmat_n[[i]] <- density(Diff_nonmat_train_n[[i]], kernel = "epanechnikov", bw=2.1)
  }
  
  else
  {
    kernel_mat_n$x[[i]] <- NA
    kernel_mat_n$y[[i]] <- NA
    kernel_nonmat_n$x[[i]] <-  NA
    kernel_nonmat_n$y[[i]] <-  NA
  }
  
}


# kernel (nonparametric) : non-normalized but scailized differences
kernel_mat_cs <- list()
kernel_nonmat_cs <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_cs[[i]]) != 0) {
    set.seed(123)
    kernel_mat_cs[[i]] <- density(Diff_mat_train_cs[[i]])
    kernel_nonmat_cs[[i]] <- density(Diff_nonmat_train_cs[[i]], kernel = "epanechnikov", bw=5)
    kernel_nonmat_cs[[i]] <- density(Diff_nonmat_train_cs[[i]])
  }
  
  else
  {
    kernel_mat_cs$x[[i]] <- NA
    kernel_mat_cs$y[[i]] <- NA
    kernel_nonmat_cs$x[[i]] <-  NA
    kernel_nonmat_cs$y[[i]] <-  NA
  }
  
}

plot(kernel_nonmat_cs[[1]]$x, kernel_nonmat_cs[[1]]$y, type="l", col="red" )
par(new=TRUE)
plot(kernel_mat_cs[[1]]$x, kernel_mat_cs[[1]]$y , type="l", col="green", add=TRUE )

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


#sample plot
# library(ggplot2)
i=31
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



#plot a = non-parametric but smoothing data
a_mat <- data.frame(density_smooth_hist_mat_c[[i]]$x , density_smooth_hist_mat_c[[i]]$y )
a_nonmat <- data.frame(density_smooth_hist_nonmat_c[[i]]$x, density_smooth_hist_nonmat_c[[i]]$y)
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

# plot histogram
k <- 31
plot( hist_nonmat_c[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_c[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_c[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_c[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_c[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c[[k]]$count )),
      freq=TRUE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference - Nonnormalized data')

plot( hist_mat_c[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

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
i=25
a_mat_sig <- data.frame(density_smooth_hist_mat_c_sig[[i]]$x , density_smooth_hist_mat_c_sig[[i]]$y )
a_nonmat_sig <- data.frame(density_smooth_hist_nonmat_c_sig[[i]]$x, density_smooth_hist_nonmat_c_sig[[i]]$y)
ggplot() +
  geom_line(data=a_mat_sig, aes(x=a_mat_sig[,1] , y=a_mat_sig[,2]) , color='green') + 
  geom_line(data=a_nonmat_sig, aes(x=a_nonmat_sig[,1] , y=a_nonmat_sig[,2]) , color='red') 


#plot b = parametric , Gaussian fitting data
b_mat_sig <- data.frame(diffseq_mat_c_sig[[i]], 
                    normal_mat_c_sig[[i]]  * multiplier_hist_mat_c_sig[[i]][ which.min(is.na( multiplier_hist_mat_c_sig[[i]] ) ) ]  )
b_nonmat_sig <- data.frame( diffseq_nonmat_c_sig[[i]], 
                        normal_nonmat_c_sig[[i]]  * multiplier_hist_nonmat_c_sig[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_sig, aes ( x=b_mat_sig[,1] , y=b_mat_sig[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_sig, aes ( x=b_nonmat_sig[,1] , y=b_nonmat_sig[,2]  ) , color='red')


ggplot() +
  geom_line(data=a_mat_sig, aes ( x=a_mat_sig[,1] , y=a_mat_sig[,2]  ) , color='blue') + 
  geom_line(data=b_mat_sig, aes ( x=b_mat_sig[,1] , y=b_mat_sig[,2]  ) , color='black') 


# plot histogram
k <- 35
plot( hist_nonmat_c_sig[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_sig_mat_train[[k]]) , sigweight[k], f= floor)  , 
                     round_any (min(Diff_sig_nonmat_train[[k]]) , sigweight[k], f= floor) ) ,
               max ( round_any (max(Diff_sig_mat_train[[k]]) , sigweight[k], f= ceiling) ,
                     round_any (max(Diff_sig_nonmat_train[[k]]) , sigweight[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_sig[[k]]$count )),
      freq=TRUE,
      xlab = 'sigfeature diff', ylab = 'Density', main = 'Histogram of Sig Feature Difference')

plot( hist_mat_c_sig[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

# http://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_04272015")
#############################################################################################
# end

# example
# myhist <- hist(Diff_mat_train_c[[3]] )
# multiplier <- (myhist$counts / sum(myhist$counts) ) / myhist$density
# mydensity <- density(Diff_mat_train_c[[3]])
# mydensity$y <- mydensity$y * multiplier[1]
# 
# myhist$density <- myhist$counts  / sum(myhist$counts)
# 
# plot(myhist , freq=FALSE)
# lines(mydensity)
# density_smooth <-  smooth.spline(mydensity$x, mydensity$y,spar=0.8)
# plot(density_smooth$x, density_smooth$y)
# plot(density_smooth$data$x, density_smooth$data$y)
# 
# # plot v2
# k <- 21
# barplot ( histdensity_c[[k]][,3],  names.arg = histdensity_c[[k]][,1],
#           xlim = c( min (histdensity_c[[k]][,1]) , max (histdensity_c[[k]][,1] ) ),  col="red", 
#           xlab = 'GVW Difference', ylab = 'Frequency', main = 'Histogram of GVW Difference - Nonnormalized data' ) #matching
# barplot(histdensity_c[[k]][,4], col = rgb(0,1,0,0.5), add=TRUE)
# 
# normalfit <- dnorm (Diff_mat_train_c[[k]] ) 
# plot ( Diff_mat_train_c[[k]] , normalfit , add=TRUE )
# lines ( Diff_mat_train_c[[k]] , normalfit )
# curve ( dnorm ( x , mean=mean ( Diff_mat_train_c[[k]] ), sd=sd( Diff_mat_train_c[[k]]) )  ,               
#         min ( Diff_mat_train_c[[k]] , Diff_nonmat_train_c[[k]]) ,
#         max ( Diff_mat_train_c[[k]] , Diff_nonmat_train_c[[k]]) ,
#         col = "blue", lwd = 2 , add=TRUE ) 
# 
# curve ( dnorm ( x , mean=mean ( Diff_mat_train_c[[k]] ), sd=sd( Diff_mat_train_c[[k]]) )  ,                     
#         col = "blue", lwd = 2 , add=TRUE , yaxt="n") 
# 
# histogram (Diff_mat_train_c[[k]])
# histogram (Diff_nonmat_train_c[[k]], add= TRUE)
# 
# h_mat <- hist( Diff_mat_train_c[[k]], plot=F)
# h_mat$counts <- h_mat$counts / sum (h_mat$counts)
# plot ( h_mat, col="red",
#         xlim = c(min ( round_any (min(Diff_mat_train_c[[k]]) , interval_utc, f= floor)  , 
#                        round_any (min(Diff_nonmat_train_c[[k]]) , interval_utc, f= floor) ) ,
#                  max ( round_any (max(Diff_mat_train_c[[k]]) , interval_utc, f= ceiling) ,
#                        round_any (max(Diff_nonmat_train_c[[k]]) , interval_utc, f= ceiling) )),
#         ylim= c(0,max(h_mat$counts)), freq=TRUE,
#         xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of GVW Difference - Nonnormalized data')
# 
# 
# h_nonmat <- hist( Diff_nonmat_train_c[[k]], plot=F)
# h_nonmat$counts <- h_nonmat$counts / sum (h_nonmat$counts)
# plot( h_nonmat, col = rgb(0,1,0,0.5), freq=FALSE, add=T)
# 
# 
# curve ( dnorm(x, mean=mean ( Diff_mat_train_cs[[k]] ), sd= sd( Diff_mat_train_cs[[k]]) )  , 
#         col = "blue", lwd = 2 ) 
# 
# curve ( dnorm ( x , mean=mean ( Diff_mat_train_c[[k]] ), sd=sd( Diff_mat_train_c[[k]]) )  ,               
#         min ( Diff_mat_train_c[[k]] , Diff_nonmat_train_c[[k]]) ,
#         max ( Diff_mat_train_c[[k]] , Diff_nonmat_train_c[[k]]) ,
#         col = "blue", lwd = 2 , add=TRUE ) 
# 
# barplot ( histdensity_c[[k]][,3],  xlim = c(0, 100)), col="red",
#           xlab = 'GVW Difference', ylab = 'Frequency', main = 'Histogram of GVW Difference - Nonnormalized data' ) #matching
# barplot(histdensity_c[[k]][,4], col = rgb(0,1,0,0.5), add=TRUE)
# # if want to make gamma dist.
# curve(dgamma(x, shape = mean(Diff_mat_train_c[[3]])^2/var(Diff_mat_train_c[[3]]),
#              scale = var(Diff_mat_train_c[[3]])/mean (Diff_mat_train_c[[3]]) ) , add=T )


### histogram with normalized data
hist_mat_n <- list()
hist_nonmat_n <- list()
histdensity_n <- list()


for ( i in 1 : length( Diff_mat_train_n)  ){
  
  if (i == 1){ # utc
    interval_utc <- 0.1
    min <- round_any (min(Diff_nonmat_train_n[[i]]) , interval_utc, f= floor)
    max <- round_any (max(Diff_nonmat_train_n[[i]]) , interval_utc, f= ceiling)
    #       hist_mat[[i]] <- hist(Diff_mat_train_n[[i]], freq=FALSE, breaks=seq(min ,max, by=interval_utc), plot=FALSE)
    #       hist_nonmat[[i]] <- hist(Diff_nonmat_train_n[[i]], freq=FALSE, breaks=seq(min,max,by=interval_utc) ,  plot=FALSE)
    #       histdensity[[i]] <- cbind(hist_mat[[i]]$mids - interval_utc/2, hist_mat[[i]]$mids + interval_utc/2 , 
    #                                 hist_mat[[i]]$density ,  hist_nonmat[[i]]$density) 
    
    hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]],  breaks=seq(min ,max, by=interval_utc), plot=FALSE)
    hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],  breaks=seq(min,max,by=interval_utc) ,  plot=FALSE)
    histdensity_n[[i]] <- cbind(hist_mat_n[[i]]$mids - interval_utc/2, hist_mat_n[[i]]$mids + interval_utc/2 , 
                                hist_mat_n[[i]]$counts / sum(hist_mat_n[[i]]$counts) ,  
                                hist_nonmat_n[[i]]$counts / sum(hist_nonmat_n[[i]]$counts) ) 
  }
  
  if (i == 2){ # length
    interval_len <- 0.1
    min <- round_any (min(Diff_nonmat_train_n[[i]]) , interval_len, f= floor)
    max <- round_any (max(Diff_nonmat_train_n[[i]]) , interval_len, f= ceiling)
    #       hist_mat[[i]] <- hist(Diff_mat_train_n[[i]], freq=FALSE, breaks=seq(min ,max, by=interval_len) , plot=FALSE)
    #       hist_nonmat[[i]] <- hist(Diff_nonmat_train_n[[i]], freq=FALSE, breaks=seq(min,max,by=interval_len) ,  plot=FALSE)
    #       histdensity[[i]] <- cbind(hist_mat[[i]]$mids - interval_len/2, hist_mat[[i]]$mids   + interval_len/2,  
    #                                 hist_mat[[i]]$density , hist_nonmat[[i]]$density)      
    hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]],  breaks=seq(min ,max, by=interval_len) , plot=FALSE)
    hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],  breaks=seq(min,max,by=interval_len) ,  plot=FALSE)
    histdensity_n[[i]] <- cbind(hist_mat_n[[i]]$mids - interval_len/2, hist_mat_n[[i]]$mids   + interval_len/2,  
                                hist_mat_n[[i]]$counts / sum(hist_mat_n[[i]]$counts), 
                                hist_nonmat_n[[i]]$counts / sum(hist_nonmat_n[[i]]$counts))      
  }
  
  
  if (i == 3){ # gvw
    interval_gvw <- 0.1
    min <- round_any (min(Diff_nonmat_train_n[[i]]) , interval_gvw, f= floor)
    max <- round_any (max(Diff_nonmat_train_n[[i]]) , interval_gvw, f= ceiling)
    #       hist_mat[[i]] <- hist(Diff_mat_train_n[[i]], freq=FALSE, breaks=seq(min ,max, by=interval_gvw) , plot=FALSE)
    #       hist_nonmat[[i]] <- hist(Diff_nonmat_train_n[[i]], freq=FALSE, breaks=seq(min,max,by=interval_gvw) ,  plot=FALSE)
    #       histdensity[[i]] <- cbind(hist_mat[[i]]$mids - interval_gvw/2, hist_mat[[i]]$mids   + interval_gvw/2, 
    #                                 hist_mat[[i]]$density,  hist_nonmat[[i]]$density)      
    hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]], breaks=seq(min ,max, by=interval_gvw) , plot=FALSE)
    hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],  breaks=seq(min,max,by=interval_gvw) ,  plot=FALSE)
    histdensity_n[[i]] <- cbind(hist_mat_n[[i]]$mids - interval_gvw/2, hist_mat_n[[i]]$mids   + interval_gvw/2, 
                                hist_mat_n[[i]]$counts / sum(hist_mat_n[[i]]$counts), 
                                hist_nonmat_n[[i]]$counts / sum(hist_nonmat_n[[i]]$counts))  
  }
  
  if (i > 3) {
    
    if ( i %in% c (4,5,6,7,12,13,14,15,16,17,18,19,20,21,30)  ){
      
      interval_others <- 0.1 # others
      min <- min ( round_any (min(Diff_mat_train_n[[i]]) , interval_others, f= floor)  , 
                   round_any (min(Diff_nonmat_train_n[[i]]) , interval_others, f= floor) )
      max <- max (round_any (max(Diff_mat_train_n[[i]]) , interval_others, f= ceiling) ,
                  round_any (max(Diff_nonmat_train_n[[i]]) , interval_others, f= ceiling) )
      #           hist_mat[[i]] <- hist(Diff_mat_train_n[[i]], freq=FALSE, breaks=seq(min ,max, by=interval_others), plot=FALSE)
      #           hist_nonmat[[i]] <- hist(Diff_nonmat_train_n[[i]], freq=FALSE, breaks=seq(min,max,by=interval_others), plot=FALSE)
      #           histdensity[[i]] <- cbind( round_any (hist_mat[[i]]$mids - interval_others/2, interval_others),
      #                                      round_any (hist_mat[[i]]$mids + interval_others/2 ,interval_others), 
      #                                      hist_mat[[i]]$density,  hist_nonmat[[i]]$density)  
      hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]], breaks=seq(min ,max, by=interval_others), plot=FALSE)
      hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],breaks=seq(min,max,by=interval_others), plot=FALSE)
      histdensity_n[[i]] <- cbind( round_any (hist_mat_n[[i]]$mids - interval_others/2, interval_others),
                                   round_any (hist_mat_n[[i]]$mids + interval_others/2 ,interval_others), 
                                   hist_mat_n[[i]]$counts / sum( hist_mat_n[[i]]$counts ), 
                                   hist_nonmat_n[[i]]$counts / sum( hist_nonmat_n[[i]]$counts ))    
    }
    
    if (i == 31){
      
      interval_dur <- 0.1 #duration
      min <- min ( round_any (min(Diff_mat_train_n[[i]]) , interval_dur, f= floor)  , 
                   round_any (min(Diff_nonmat_train_n[[i]]) , interval_dur, f= floor) )
      max <- max (round_any (max(Diff_mat_train_n[[i]]) , interval_dur, f= ceiling) ,
                  round_any (max(Diff_nonmat_train_n[[i]]) , interval_dur, f= ceiling) )
      #           hist_mat[[i]] <- hist(Diff_mat_train_n[[i]], freq=FALSE, breaks=seq(min ,max, by=interval_dur), plot=FALSE)
      #           hist_nonmat[[i]] <- hist(Diff_nonmat_train_n[[i]], freq=FALSE, breaks=seq(min,max,by=interval_dur), plot=FALSE)
      #           histdensity[[i]] <- cbind(hist_mat[[i]]$mids - interval_dur/2, hist_mat[[i]]$mids   + interval_dur/2, 
      #                                     hist_mat[[i]]$density,  hist_nonmat[[i]]$density)  
      hist_mat_n[[i]] <- hist(Diff_mat_train_n[[i]],  breaks=seq(min ,max, by=interval_dur), plot=FALSE)
      hist_nonmat_n[[i]] <- hist(Diff_nonmat_train_n[[i]],  breaks=seq(min,max,by=interval_dur), plot=FALSE)
      histdensity_n[[i]] <- cbind(hist_mat_n[[i]]$mids - interval_dur/2, hist_mat_n[[i]]$mids   + interval_dur/2, 
                                  hist_mat_n[[i]]$counts / sum( hist_mat_n[[i]]$counts  ),  
                                  hist_nonmat_n[[i]]$counts / sum( hist_nonmat_n[[i]]$counts ))  
      
      
    }
    
    if ( i %in% c (8,9,10, 11, 22,23,24,25,26,27,28,29)  ){
      hist_mat_n[[i]] <- NA
      hist_nonmat_n[[i]] <- NA
      histdensity_n[[i]] <- NA
    }
    
  } 
}



# plot v1
k <- 1
plot( hist_nonmat_n[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_n[[k]]) , interval_utc, f= floor)  , 
                     round_any (min(Diff_nonmat_train_n[[k]]) , interval_utc, f= floor) ) ,
               max ( round_any (max(Diff_mat_train_n[[k]]) , interval_utc, f= ceiling) ,
                     round_any (max(Diff_nonmat_train_n[[k]]) , interval_utc, f= ceiling) )),
      ylim= c(0,max(hist_mat_n[[k]]$density )), freq=FALSE,
      xlab = 'GVW Difference', ylab = 'Density', main = 'Histogram of utc Difference - normalized data')

plot( hist_mat_n[[k]], col = rgb(0,1,0,0.5), freq=FALSE, add=T)






### param kernel : normalized differences

install.packages("fitdistrplus")
library(fitdistrplus)

y <- data.frame()
bicall_mat <- data.frame(0,0,0,0,0)
bicall_nonmat <- data.frame(0,0,0,0,0)


for (i in 1: length(Diff_mat_train_n))
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {
    

    set.seed(123)
    y <- as.numeric ( Diff_mat_train_n[[i]] )
    fitw_mat <- fitdist(y, "weibull", method="qme", probs=c(0.25, 0.75))
    fitg_mat <- fitdist(y, "gamma", method="mge",gof="CvM")
#     fitln_mat <- fitdist(y, "lnorm")
    fite_mat <- fitdist(y, "exp")
#     bicall_mat[i,] <- c(fitw_mat$bic, fitg_mat$bic, fitln_mat$bic, fite_mat$bic, 0)
    bicall_mat[i,] <- c(fitw_mat$bic, fitg_mat$bic, 0, fite_mat$bic, 0)
    bicall_mat[i,5] <- which.min(bicall_mat[i,1:4])
    

    set.seed(123)
    y <- as.numeric ( Diff_nonmat_train_n[[i]] )
    fitw_nonmat <- fitdist(y, "weibull", method="qme", probs=c(0.25, 0.75))
    fitg_nonmat <- fitdist(y, "gamma",method="mge",gof="CvM")
#     fitln_nonmat <- fitdist(y, "lnorm")
    fite_nonmat <- fitdist(y, "exp")
#     bicall_nonmat[i,] <- c(fitw_nonmat$bic, fitg_nonmat$bic, fitln_nonmat$bic, fite_nonmat$bic, 0)
    bicall_nonmat[i,] <- c(fitw_nonmat$bic, fitg_nonmat$bic, 0, fite_nonmat$bic, 0)
    bicall_nonmat[i,5] <- which.min(bicall_nonmat[i,1:4])
  }
  else
  {
    bicall_mat[i,] <- NA
    bicall_nonmat[i,] <- NA
  }
}

print(fitw_mat)


# make kernel mat (normalized differences)
kernel_para_mat <- list()
kernel_para_nonmat <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {
    

    if (bicall_mat[i,5] == 1 ){
      y <- Diff_mat_train_n[[i]]
      fitw_mat <- fitdist(y, "weibull")
      kernel_para_mat[[i]] <- curve( dweibull(x, shape = fitw_mat$estimate[[1]] , scale = fitw_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 2 ){
      y <- Diff_mat_train_n[[i]]
      fitg_mat <- fitdist(y, "gamma")
      kernel_para_mat[[i]] <- curve( dgamma(x, shape = fitg_mat$estimate[[1]] , rate = fitg_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 3 ){
      y <- Diff_mat_train_n[[i]]
      fitl_mat <- fitdist(y, "lnorm")
      kernel_para_mat[[i]] <- curve( dlnorm(x, meanlog = fitl_mat$estimate[[1]] , sdlog = fitg_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 4 ){
      y <- Diff_mat_train_n[[i]]
      fite_mat <- fitdist(y, "exp")
      kernel_para_mat[[i]] <- curve( dexp(x, rate = fite_mat$estimate[[1]]  ) )
    }
    kernel_para_mat[[i]]$y[is.infinite(kernel_para_mat[[i]]$y)] <- 10  
  }
  
  else
  {
    kernel_para_mat$x[[i]] <- NA
    kernel_para_mat$y[[i]] <- NA
  }
  
}


# make kernel nonmat (normalized differences)

for ( i in 1: 31 )
{
  if (sum ( Diff_nonmat_train_n[[i]]) != 0) {
    
    
    if (bicall_nonmat[i,5] == 1 ){
      y <- Diff_nonmat_train_n[[i]]
      fitw_nonmat <- fitdist(y, "weibull")
      kernel_para_nonmat[[i]] <- curve( dweibull(x, shape = fitw_nonmat$estimate[[1]] , scale = fitw_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 2 ){
      y <- Diff_nonmat_train_n[[i]]
      fitg_nonmat <- fitdist(y, "gamma")
      kernel_para_nonmat[[i]] <- curve( dgamma(x, shape = fitg_nonmat$estimate[[1]] , rate = fitg_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 3 ){
      y <- Diff_nonmat_train_n[[i]]
      fitl_nonmat <- fitdist(y, "lnorm")
      kernel_para_nonmat[[i]] <- curve( dlnorm(x, meanlog = fitl_nonmat$estimate[[1]] , sdlog = fitg_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 4 ){
      y <- Diff_nonmat_train_n[[i]]
      fite_nonmat <- fitdist(y, "exp")
      kernel_para_nonmat[[i]] <- curve( dexp(x, rate = fite_nonmat$estimate[[1]]  ) )
    }
    kernel_para_nonmat[[i]]$y[is.infinite(kernel_para_nonmat[[i]]$y)] <- 10
  }
  
  else
  {
    kernel_para_nonmat$x[[i]] <- NA
    kernel_para_nonmat$y[[i]] <- NA    
  }
}



### NON-Normalized data

# param kernel : normalized differences
bicall_mat <- data.frame(0,0,0,0,0)
bicall_nonmat <- data.frame(0,0,0,0,0)
j <- 0
k <- 0

for (i in 1: length(Diff_mat_train_c))
{
  if (sum ( Diff_mat_train_c[[i]]) != 0) {
    
    #     j<-j+1
    set.seed(123)
    y <- Diff_mat_train_c[[i]]
    fitw_mat <- fitdist(y, "weibull", method="qme", probs=c(0.25, 0.75))
    fitg_mat <- fitdist(y, "gamma", method="mge",gof="CvM")
    fitln_mat <- fitdist(y, "lnorm")
    fite_mat <- fitdist(y, "exp")
    bicall_mat[i,] <- c(fitw_mat$bic, fitg_mat$bic, fitln_mat$bic, fite_mat$bic, 0)
    bicall_mat[i,5] <- which.min(bicall_mat[i,1:4])
    
    #     k<-k+1
    set.seed(123)
    y <- Diff_nonmat_train_c[[i]]
    fitw_nonmat <- fitdist(y, "weibull", method="qme", probs=c(0.25, 0.75))
    fitg_nonmat <- fitdist(y, "gamma",method="mge",gof="CvM")
    fitln_nonmat <- fitdist(y, "lnorm")
    fite_nonmat <- fitdist(y, "exp")
    bicall_nonmat[i,] <- c(fitw_nonmat$bic, fitg_nonmat$bic, fitln_nonmat$bic, fite_nonmat$bic, 0)
    bicall_nonmat[i,5] <- which.min(bicall_nonmat[i,1:4])
  }
  else
  {
    bicall_mat[i,] <- NA
    bicall_nonmat[i,] <- NA
  }
}

print(fitw_mat)


# make kernel mat (normalized differences)
kernel_para_mat <- list()
kernel_para_nonmat <- list()
j <- 0

for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_c[[i]]) != 0) {
    
    
    
    if (bicall_mat[i,5] == 1 ){
      y <- Diff_mat_train_c[[i]]
      fitw_mat <- fitdist(y, "weibull")
      kernel_para_mat[[i]] <- curve( dweibull(x, shape = fitw_mat$estimate[[1]] , scale = fitw_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 2 ){
      y <- Diff_mat_train_c[[i]]
      fitg_mat <- fitdist(y, "gamma")
      kernel_para_mat[[i]] <- curve( dgamma(x, shape = fitg_mat$estimate[[1]] , rate = fitg_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 3 ){
      y <- Diff_mat_train_c[[i]]
      fitl_mat <- fitdist(y, "lnorm")
      kernel_para_mat[[i]] <- curve( dlnorm(x, meanlog = fitl_mat$estimate[[1]] , sdlog = fitg_mat$estimate[[2]] ) )
    }
    
    else if (bicall_mat[i,5] == 4 ){
      y <- Diff_mat_train_c[[i]]
      fite_mat <- fitdist(y, "exp")
      kernel_para_mat[[i]] <- curve( dexp(x, rate = fite_mat$estimate[[1]]  ) )
    }
    kernel_para_mat[[i]]$y[is.infinite(kernel_para_mat[[i]]$y)] <- 10  
  }
  
  else
  {
    kernel_para_mat$x[[i]] <- NA
    kernel_para_mat$y[[i]] <- NA
    
  }
  
}


# make kernel nonmat (normalized differences)
j <- 0
for ( i in 1: 31 )
{
  if (sum ( Diff_nonmat_train_c[[i]]) != 0) {
    
    #     j <- j+1
    
    if (bicall_nonmat[i,5] == 1 ){
      y <- Diff_nonmat_train_c[[i]]
      fitw_nonmat <- fitdist(y, "weibull")
      kernel_para_nonmat[[i]] <- curve( dweibull(x, shape = fitw_nonmat$estimate[[1]] , scale = fitw_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 2 ){
      y <- Diff_nonmat_train_c[[i]]
      fitg_nonmat <- fitdist(y, "gamma")
      kernel_para_nonmat[[i]] <- curve( dgamma(x, shape = fitg_nonmat$estimate[[1]] , rate = fitg_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 3 ){
      y <- Diff_nonmat_train_c[[i]]
      fitl_nonmat <- fitdist(y, "lnorm")
      kernel_para_nonmat[[i]] <- curve( dlnorm(x, meanlog = fitl_nonmat$estimate[[1]] , sdlog = fitg_nonmat$estimate[[2]] ) )
    }
    
    else if (bicall_nonmat[i,5] == 4 ){
      y <- Diff_nonmat_train_c[[i]]
      fite_nonmat <- fitdist(y, "exp")
      kernel_para_nonmat[[i]] <- curve( dexp(x, rate = fite_nonmat$estimate[[1]]  ) )
    }
    kernel_para_nonmat[[i]]$y[is.infinite(kernel_para_nonmat[[i]]$y)] <- 10
  }
  
  else
  {
    kernel_para_nonmat$x[[i]] <- NA
    kernel_para_nonmat$y[[i]] <- NA    
  }
}



# compare kernel and histogram
kernel_para_mat_g <- list()
kernel_para_nonmat_g <- list()

k <- 16
x <- seq(0,1, by= 0.01)

bicall_nonmat[3,5] <-4

for ( i in  k)
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {
    
    
    hist(Diff_mat_train_n[[i]] , prob=TRUE)
    
    
    if (bicall_mat[i,5] == 1 ){
      y <- Diff_mat_train_n[[i]]
      fitw_mat <- fitdist(y, "weibull")
      kernel_para_mat_g[[i]] <- dweibull(x, shape = fitw_mat$estimate[[1]] , scale = fitw_mat$estimate[[2]] ) 
      lines( x, kernel_para_mat_g[[i]] )
    }
    
    else if (bicall_mat[i,5] == 2 ){
      y <- Diff_mat_train_n[[i]]
      fitg_mat <- fitdist(y, "gamma")
      kernel_para_mat_g[[i]] <- dgamma(x, shape = fitg_mat$estimate[[1]] , rate = fitg_mat$estimate[[2]] ) 
      lines( x, kernel_para_mat_g[[i]] )
    }
    
    else if (bicall_mat[i,5] == 3 ){
      y <- Diff_mat_train_n[[i]]
      fitl_mat <- fitdist(y, "lnorm")
      kernel_para_mat_g[[i]] <- dlnorm(x, meanlog = fitl_mat$estimate[[1]] , sdlog = fitg_mat$estimate[[2]] ) 
      lines( x, kernel_para_mat_g[[i]] )
    }
    
    else if (bicall_mat[i,5] == 4 ){
      y <- Diff_mat_train_n[[i]]
      fite_mat <- fitdist(y, "exp")
      kernel_para_mat_g[[i]] <- dexp(x, rate = fite_mat$estimate[[1]]  ) 
      lines( x, kernel_para_mat_g[[i]] )
      
    }
    kernel_para_mat_g[[i]][is.infinite(kernel_para_mat_g[[i]])] <- 10  
  }
  
  else
  {
    #     kernel_para_mat$x[[i]] <- NA
    kernel_para_mat_g[[i]] <- NA
  }
  
}

j <- 0

for ( i in k )
{
  if (sum ( Diff_nonmat_train_n[[i]]) != 0) {
    
    hist(Diff_nonmat_train_n[[i]], prob=TRUE )
    
    if (bicall_nonmat[i,5] == 1 ){
      y <- Diff_nonmat_train_n[[i]]
      fitw_nonmat <- fitdist(y, "weibull")
      kernel_para_nonmat_g[[i]] <-  dweibull(x, shape = fitw_nonmat$estimate[[1]] , scale = fitw_nonmat$estimate[[2]] ) 
      lines( x, kernel_para_nonmat_g[[i]] )
    }
    
    else if (bicall_nonmat[i,5] == 2 ){
      y <- Diff_nonmat_train_n[[i]]
      fitg_nonmat <- fitdist(y, "gamma")
      kernel_para_nonmat_g[[i]] <- dgamma(x, shape = fitg_nonmat$estimate[[1]] , rate = fitg_nonmat$estimate[[2]] ) 
      lines( x, kernel_para_nonmat_g[[i]] )
    }
    
    else if (bicall_nonmat[i,5] == 3 ){
      y <- Diff_nonmat_train_n[[i]]
      fitl_nonmat <- fitdist(y, "lnorm")
      kernel_para_nonmat_g[[i]] <- dlnorm(x, meanlog = fitl_nonmat$estimate[[1]] , sdlog = fitg_nonmat$estimate[[2]] )
      lines( x, kernel_para_nonmat_g[[i]] )
    }
    
    else if (bicall_nonmat[i,5] == 4 ){
      y <- Diff_nonmat_train_n[[i]]
      fite_nonmat <- fitdist(y, "exp")
      kernel_para_nonmat_g[[i]] <-dexp(x, rate = fite_nonmat$estimate[[1]]  ) 
      lines( x, kernel_para_nonmat_g[[i]] )
    }
    kernel_para_nonmat_g[[i]][is.infinite(kernel_para_nonmat_g[[i]])] <- 10
  }
  
  else
  {
    #     kernel_para_nonmat$x[[i]] <- NA
    kernel_para_nonmat_g[[i]] <- NA    
  }
}

# KS test
kstestresult <- data.frame(0)

for ( i in 1: length(Diff_mat_train_n) ){
  
  if ( sum ( Diff_mat_train_n[[i]] ) == 0) {    
    kstestresult[i] <- NA
  }
  
  else 
  {
    kstestresult[i] <- ks.test( kernel_para_mat[[i]]$y,  kernel_para_nonmat[[i]]$y  )[[2]]
  } 
  
}



## end

op <- options(digits = 3)
set.seed(123)
normaldist<- fitdistr(Diff_mat_train_n[[2]], "normal")
set.seed(123)
hist(Diff_nonmat_train_n[[2]])
fitdistr(Diff_nonmat_train_n[[2]], "gamma")
tes <- curve(dgamma(x, scale=0.0587, shape=0.2723),from=0, to=1, main="Gamma distribution")


set.seed(123)
fitdistr(Diff_nonmat_train_n[[2]],densfun=dweibull,start=list(scale=1,shape=2))
set.seed(123)
fitdistr(Diff_nonmat_train_n[[2]], "beta", start=list(shape1=0.2, shape2=0.01))
set.seed(123)
fitdistr(Diff_nonmat_train_n[[2]], "negative binomial")
fitdistr(Diff_nonmat_train_n[[2]], dgamma, list(shape = 1, rate = 0.1), lower = 0.001, log=TRUE)



library(fitdistrplus)
plotdist (y)
descdist(y)
descdist(y, boot=50)

f1 <- fitdist(y , "gamma")
b1 <- bootdist(f1, niter=51)
print(b1)
plot(b1)
summary(b1)

fln <- fitdist(y, "normal")
summary(fln)
plot(fln)
qqcomp(fln, addlegend=FALSE)
denscomp(fln, addlegend=FALSE)

y <- Diff_mat_train_n[[3]]
fitW <- fitdist(y, "weibull")
fitg <- fitdist(y, "gamma")
fitln <- fitdist(y, "lnorm")
fite <- fitdist(y, "exp")
# fitb <- fitdist(y, "beta")

summary(fitW)
summary(fitg)
summary(fitln)
summary(fite)
# summary(fitb)
cdfcomp(list(fitW, fitg, fitln, fite), legendtext=c("Weibull", "gamma", "lognormal", "exponential"))
denscomp(list(fitW, fitg, fitln, fite), legendtext=c("Weibull", "gamma", "lognormal", "exponential"))
qqcomp(list(fitW, fitg, fitln, fite), legendtext=c("Weibull", "gamma", "lognormal", "exponential"))
ppcomp(list(fitW, fitg, fitln, fite), legendtext=c("Weibull", "gamma", "lognormal", "exponential"))
gofstat(list(fitW, fitg, fitln, fite), fitnames=c("Weibull", "gamma", "lognormal", "exponential"))





# utils:::menuInstallPkgs() 
# library(bootstrap)
# theta <- function(x){mean(x)}
# y_bt <- bootstrap(y,100, theta)

fitw <- fitdist(y, "weibull")
fll <- fitdist(y, "logistic")
fitln <- fitdist(y, "lnorm")

dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))
fitgumbel <- fitdist(y, "gumbel", start=list(a=10, b=10))

qqcomp(list(fitw,fln,f1),legendtext=c("Weibull","normal","gamma"),
       main="length fits",xlegend = "bottomright",line01 = TRUE,
       xlim = c(0,1), ylim = c(0,1), fitpch=16)
# sub_matching_train <- cbind( sub_matching_train ,      
#                              na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),13:44] ),
#                              na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),7] ),
#                              na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),12] ),
#                              na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),13:44] ),
#                              na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),7] ),
#                              na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),12] ) )
# 
# sub_nonmatching_train  <- cbind( sub_nonmatching_train ,
#                                  Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),13:44],
#                                  Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),7],
#                                  Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),12],
#                                  Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),13:44],
#                                  Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),7],
#                                  Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),12])





# sub_matching_test <- cbind( sub_matching_test ,      
#                             na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),13:44] ),
#                             na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),7] ),
#                             na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),12] ),
#                             na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),13:44] ),
#                             na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),7] ),
#                             na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),12] ) )
# 
# sub_nonmatching_test  <- cbind( sub_nonmatching_test ,
#                                 Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),13:44],
#                                 Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),7],
#                                 Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),12],
#                                 Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),13:20],
#                                 Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),7],
#                                 Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),12])


# # normalized factor (train set)
# max_train <- vector()
# max_train[1] <- max(abs( na.omit(sub_all_train[,9] - sub_all_train[,19]) )) #length
# max_train[2] <- max(abs( na.omit(sub_all_train[,10] -sub_all_train[,20]) )) # gvw
# max_train[3] <- max(abs( na.omit(sub_all_train[,11] - sub_all_train[,21]) )) # ax12sp
# max_train[4] <- max(abs( na.omit(sub_all_train[,12] - sub_all_train[,22]) )) # ax23sp
# max_train[5] <- max(abs( na.omit(sub_all_train[,13] - sub_all_train[,23]) )) # ax34sp
# max_train[6] <- max(abs( na.omit(sub_all_train[,14] - sub_all_train[,24]) )) # ax45sp
# max_train[7] <- max(abs( na.omit(sub_all_train[,15] - sub_all_train[,25]) )) # dur
# max_train[8] <- max(abs( na.omit(sub_all_train[,16] - sub_all_train[,26]) )) #utc
# max_train[9] <- max( na.omit(sub_all_train[,3]) ) # sum mag diff
# 
# min_train <- vector()
# min_train[1] <- min(abs( na.omit(sub_all_train[,9] - sub_all_train[,19]) )) #length
# min_train[2] <- min(abs( na.omit(sub_all_train[,10] -sub_all_train[,20]) )) # gvw
# min_train[3] <- min(abs( na.omit(sub_all_train[,11] - sub_all_train[,21]) )) # ax12sp
# min_train[4] <- min(abs( na.omit(sub_all_train[,12] - sub_all_train[,22]) )) # ax23sp
# min_train[5] <- min(abs( na.omit(sub_all_train[,13] - sub_all_train[,23]) )) # ax34sp
# min_train[6] <- min(abs( na.omit(sub_all_train[,14] - sub_all_train[,24]) )) # ax45sp
# min_train[7] <- min(abs( na.omit(sub_all_train[,15] - sub_all_train[,25]) )) # dur
# min_train[8] <- min(abs( na.omit(sub_all_train[,16] - sub_all_train[,26]) )) #utc
# min_train[9] <- min( na.omit(sub_all_train[,3]) ) # sum mag diff
# 
# # normalized factor (test set)
# max_test <- vector()
# max_test[1] <- max(abs( na.omit(sub_all_test[,9] - sub_all_test[,19]) ))
# max_test[2] <- max(abs( na.omit(sub_all_test[,10] - sub_all_test[,20]) ))
# max_test[3] <- max(abs( na.omit(sub_all_test[,11] - sub_all_test[,21]) ))
# max_test[4] <- max(abs( na.omit(sub_all_test[,12] - sub_all_test[,22]) ))
# max_test[5] <- max(abs( na.omit(sub_all_test[,13] - sub_all_test[,23]) ))
# max_test[6] <- max(abs( na.omit(sub_all_test[,14] - sub_all_test[,24]) ))
# max_test[7] <- max(abs( na.omit(sub_all_test[,15] - sub_all_test[,25]) ))
# max_test[8] <- max(abs( na.omit(sub_all_test[,16] - sub_all_test[,26]) ))
# max_test[9] <- max( na.omit(sub_all_test[,3]) )
# 
# min_test <- vector()
# min_test[1] <- min(abs( na.omit(sub_all_test[,9] - sub_all_test[,19]) ))
# min_test[2] <- min(abs( na.omit(sub_all_test[,10] - sub_all_test[,20]) ))
# min_test[3] <- min(abs( na.omit(sub_all_test[,11] - sub_all_test[,21]) ))
# min_test[4] <- min(abs( na.omit(sub_all_test[,12] - sub_all_test[,22]) ))
# min_test[5] <- min(abs( na.omit(sub_all_test[,13] - sub_all_test[,23]) ))
# min_test[6] <- min(abs( na.omit(sub_all_test[,14] - sub_all_test[,24]) ))
# min_test[7] <- min(abs( na.omit(sub_all_test[,15] - sub_all_test[,25]) ))
# min_test[8] <- min(abs( na.omit(sub_all_test[,16] - sub_all_test[,26]) ))
# min_test[9] <- min( na.omit(sub_all_test[,3]) )
# # 
# # normalized Difference (train)
# Diff_mat_train <- list()
# Diff_mat_train[[1]] <- na.omit ( abs (sub_matching_train [,9] - sub_matching_train[,19]) )   / max_train[1]
# Diff_mat_train[[2]] <- na.omit ( abs (sub_matching_train [,10] - sub_matching_train[,20]) )  / max_train[2]
# Diff_mat_train[[3]] <- na.omit ( abs (sub_matching_train [,11] - sub_matching_train[,21]) )  / max_train[3]
# Diff_mat_train[[4]] <- na.omit ( abs (sub_matching_train [,12] - sub_matching_train[,22]) )  / max_train[4]
# Diff_mat_train[[5]] <- na.omit ( abs (sub_matching_train [,13] - sub_matching_train[,23]) )  / max_train[5]
# Diff_mat_train[[6]] <- na.omit ( abs (sub_matching_train [,14] - sub_matching_train[,24]) )  / max_train[6]
# Diff_mat_train[[7]] <- na.omit ( abs (sub_matching_train [,15] - sub_matching_train[,25]) )  / max_train[7]
# Diff_mat_train[[8]] <- na.omit ( abs (sub_matching_train [,16] - sub_matching_train[,26]) )  / max_train[8]
# Diff_mat_train[[9]] <- na.omit ( abs (sub_matching_train [,3]  )) / max_train[9]
# 
# Diff_nonmat_train <- list()
# Diff_nonmat_train[[1]] <- na.omit ( abs (sub_nonmatching_train [,9] - sub_nonmatching_train[,19]) )   / max_train[1]
# Diff_nonmat_train[[2]] <- na.omit ( abs (sub_nonmatching_train [,10] - sub_nonmatching_train[,20]) )  / max_train[2]
# Diff_nonmat_train[[3]] <- na.omit ( abs (sub_nonmatching_train [,11] - sub_nonmatching_train[,21]) )  / max_train[3]
# Diff_nonmat_train[[4]] <- na.omit ( abs (sub_nonmatching_train [,12] - sub_nonmatching_train[,22]) )  / max_train[4]
# Diff_nonmat_train[[5]] <- na.omit ( abs (sub_nonmatching_train [,13] - sub_nonmatching_train[,23]) )  / max_train[5]
# Diff_nonmat_train[[6]] <- na.omit ( abs (sub_nonmatching_train [,14] - sub_nonmatching_train[,24]) )  / max_train[6]
# Diff_nonmat_train[[7]] <- na.omit ( abs (sub_nonmatching_train [,15] - sub_nonmatching_train[,25]) )  / max_train[7]
# Diff_nonmat_train[[8]] <- na.omit ( abs (sub_nonmatching_train [,16] - sub_nonmatching_train[,26]) )  / max_train[8]
# Diff_nonmat_train[[9]] <- na.omit ( abs (sub_nonmatching_train [,3]  ))  / max_train[9]
# 
# # normalized Difference (test)
# Diff_mat_test <- list()
# Diff_mat_test[[1]] <- na.omit ( abs (sub_matching_test [,9] - sub_matching_test[,19]) )   / max_test[1]
# Diff_mat_test[[2]] <- na.omit ( abs (sub_matching_test [,10] - sub_matching_test[,20]) )  / max_test[2]
# Diff_mat_test[[3]] <- na.omit ( abs (sub_matching_test [,11] - sub_matching_test[,21]) )  / max_test[3]
# Diff_mat_test[[4]] <- na.omit ( abs (sub_matching_test [,12] - sub_matching_test[,22]) )  / max_test[4]
# Diff_mat_test[[5]] <- na.omit ( abs (sub_matching_test [,13] - sub_matching_test[,23]) )  / max_test[5]
# Diff_mat_test[[6]] <- na.omit ( abs (sub_matching_test [,14] - sub_matching_test[,24]) )  / max_test[6]
# Diff_mat_test[[7]] <- na.omit ( abs (sub_matching_test [,15] - sub_matching_test[,25]) )  / max_test[7]
# Diff_mat_test[[8]] <- na.omit ( abs (sub_matching_test [,16] - sub_matching_test[,26]) )  / max_test[8]
# Diff_mat_test[[9]] <- na.omit ( abs (sub_matching_test [,3]  )) / max_test[9]
# 
# Diff_nonmat_test <- list()
# Diff_nonmat_test[[1]] <- na.omit ( abs (sub_nonmatching_test [,9] - sub_nonmatching_test[,19]) )   / max_test[1]
# Diff_nonmat_test[[2]] <- na.omit ( abs (sub_nonmatching_test [,10] - sub_nonmatching_test[,20]) )  / max_test[2]
# Diff_nonmat_test[[3]] <- na.omit ( abs (sub_nonmatching_test [,11] - sub_nonmatching_test[,21]) )  / max_test[3]
# Diff_nonmat_test[[4]] <- na.omit ( abs (sub_nonmatching_test [,12] - sub_nonmatching_test[,22]) )  / max_test[4]
# Diff_nonmat_test[[5]] <- na.omit ( abs (sub_nonmatching_test [,13] - sub_nonmatching_test[,23]) )  / max_test[5]
# Diff_nonmat_test[[6]] <- na.omit ( abs (sub_nonmatching_test [,14] - sub_nonmatching_test[,24]) )  / max_test[6]
# Diff_nonmat_test[[7]] <- na.omit ( abs (sub_nonmatching_test [,15] - sub_nonmatching_test[,25]) )  / max_test[7]
# Diff_nonmat_test[[8]] <- na.omit ( abs (sub_nonmatching_test [,16] - sub_nonmatching_test[,26]) )  / max_test[8]
# Diff_nonmat_test[[9]] <- na.omit ( abs (sub_nonmatching_test [,3]  )) / max_test[9]
# 
# 
# kernel_mat <- list()
# kernel_nonmat <- list()
# kernel_mat[[1]] <- density(Diff_mat_train[[1]])
# kernel_nonmat[[1]] <- density(Diff_nonmat_train[[1]])
# kernel_mat[[2]] <- density(Diff_mat_train[[2]])
# kernel_nonmat[[2]] <- density(Diff_nonmat_train[[2]])
# kernel_mat[[3]] <- density(Diff_mat_train[[3]])
# kernel_nonmat[[3]] <- density(Diff_nonmat_train[[3]])
# kernel_mat[[4]] <- density(Diff_mat_train[[4]])
# kernel_nonmat[[4]] <- density(Diff_nonmat_train[[4]])
# kernel_mat[[5]] <- density(Diff_mat_train[[5]])
# kernel_nonmat[[5]] <- density(Diff_nonmat_train[[5]])
# kernel_mat[[6]]<- density(Diff_mat_train[[6]])
# kernel_nonmat[[6]] <- density(Diff_nonmat_train[[6]])
# kernel_mat[[7]] <- density(Diff_mat_train[[7]])
# kernel_nonmat[[7]] <- density(Diff_nonmat_train[[7]])
# kernel_mat[[8]] <- density(Diff_mat_train[[8]])
# kernel_nonmat[[8]] <- density(Diff_nonmat_train[[8]])
# kernel_mat[[9]] <- density(Diff_mat_train[[9]])
# kernel_nonmat[[9]] <- density(Diff_nonmat_train[[9]])




# kernel info
# 1 <- sigdistance
# 2 <- utc
# 3, 4 <- length, gvw
# 5 ~ 8 <- axle spacing
# 9 ~ 18 <- axle weight
# 19, 20 <- duration, utc

# Diff_mat_train[[2]] <- na.omit (  abs(sub_matching_train [,10] - sub_matching_train[,44])   / sub_matching_train [,10] ) # utc 
# Diff_mat_train[[3]] <- na.omit (  abs(sub_matching_train [,11] - sub_matching_train[,20])   / sub_matching_train [,10]  ) 
# Diff_mat_train[[4]] <- na.omit (  abs(sub_matching_train [,12] - sub_matching_train[,21])   / sub_matching_train [,11]  ) 
# Diff_mat_train[[5]] <- na.omit (  abs(sub_matching_train [,13] - sub_matching_train[,22])   / sub_matching_train [,12]  ) 
# Diff_mat_train[[6]] <- na.omit (  abs(sub_matching_train [,14] - sub_matching_train[,23])   / sub_matching_train [,13]  ) 
# Diff_mat_train[[7]] <- na.omit (  abs(sub_matching_train [,15] - sub_matching_train[,24])   / sub_matching_train [,14]  ) 
# Diff_mat_train[[8]] <- na.omit (  abs(sub_matching_train [,16] - sub_matching_train[,25])   / sub_matching_train [,15]  ) 
# Diff_mat_train[[9]] <- na.omit (  abs(sub_matching_train [,17] - sub_matching_train[,26])   / sub_matching_train [,16]  ) 



# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[2]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[3]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[4]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[5]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[6]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[7]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[8]] ) ) )
# max_train_mat <- rbind(max_train_mat, max( unlist( Diff_mat_train[[9]] ) ) )

# min_train_mat <- data.frame
# min_train_mat <- min( unlist( Diff_mat_train[[1]] ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[2]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[3]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[4]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[5]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[6]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[7]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[8]] ) ) )
# min_train_mat <- rbind(min_train_mat, min( unlist( Diff_mat_train[[9]] ) ) )
# 
# for (i in 1:length(Diff_mat_train ))
# {
#   Diff_mat_train[[i]] <- ( Diff_mat_train[[i]] - min_train_mat[i,1] ) / ( max_train_mat[i,1] - min_train_mat[i,1])
# }
# 


# normalized Difference (train) - percent difference
# Diff_nonmat_train <- list()
# Diff_nonmat_train[[1]] <- na.omit (  abs(sub_nonmatching_train [,9] - sub_nonmatching_train[,19])    / sub_nonmatching_train [,9]  )
# Diff_nonmat_train[[2]] <- na.omit (  abs(sub_nonmatching_train [,10] - sub_nonmatching_train[,20])   / sub_nonmatching_train [,10]  ) 
# Diff_nonmat_train[[3]] <- na.omit (  abs(sub_nonmatching_train [,11] - sub_nonmatching_train[,21])   / sub_nonmatching_train [,11]  ) 
# Diff_nonmat_train[[4]] <- na.omit (  abs(sub_nonmatching_train [,12] - sub_nonmatching_train[,22])   / sub_nonmatching_train [,12]  ) 
# Diff_nonmat_train[[5]] <- na.omit (  abs(sub_nonmatching_train [,13] - sub_nonmatching_train[,23])   / sub_nonmatching_train [,13]  ) 
# Diff_nonmat_train[[6]] <- na.omit (  abs(sub_nonmatching_train [,14] - sub_nonmatching_train[,24])   / sub_nonmatching_train [,14]  ) 
# Diff_nonmat_train[[7]] <- na.omit (  abs(sub_nonmatching_train [,15] - sub_nonmatching_train[,25])   / sub_nonmatching_train [,15]  ) 
# Diff_nonmat_train[[8]] <- na.omit (  abs(sub_nonmatching_train [,16] - sub_nonmatching_train[,26])   / sub_nonmatching_train [,16]  ) 
# Diff_nonmat_train[[9]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 
# 
# max_train_nonmat <- data.frame
# max_train_nonmat <- max( unlist( Diff_nonmat_train[[1]] ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[2]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[3]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[4]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[5]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[6]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[7]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[8]] ) ) )
# max_train_nonmat <- rbind(max_train_nonmat, max( unlist( Diff_nonmat_train[[9]] ) ) )
# 
# min_train_nonmat <- data.frame
# min_train_nonmat <- min( unlist( Diff_nonmat_train[[1]] ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[2]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[3]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[4]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[5]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[6]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[7]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[8]] ) ) )
# min_train_nonmat <- rbind(min_train_nonmat, min( unlist( Diff_nonmat_train[[9]] ) ) )
# 
# for (i in 1:length(Diff_nonmat_train ))
# {
#   Diff_nonmat_train[[i]] <- ( Diff_nonmat_train[[i]] - min_train_nonmat[i,1] ) / ( max_train_nonmat[i,1] - min_train_nonmat[i,1])
# }
# 

# 
# # normalized Difference (test)
# Diff_mat_test <- list()
# Diff_mat_test[[1]] <- na.omit ( abs (sub_matching_test [,9] - sub_matching_test[,19]) )   
# Diff_mat_test[[2]] <- na.omit ( abs (sub_matching_test [,10] - sub_matching_test[,20]) )  
# Diff_mat_test[[3]] <- na.omit ( abs (sub_matching_test [,11] - sub_matching_test[,21]) ) 
# Diff_mat_test[[4]] <- na.omit ( abs (sub_matching_test [,12] - sub_matching_test[,22]) )  
# Diff_mat_test[[5]] <- na.omit ( abs (sub_matching_test [,13] - sub_matching_test[,23]) )  
# Diff_mat_test[[6]] <- na.omit ( abs (sub_matching_test [,14] - sub_matching_test[,24]) )  
# Diff_mat_test[[7]] <- na.omit ( abs (sub_matching_test [,15] - sub_matching_test[,25]) )  
# Diff_mat_test[[8]] <- na.omit ( abs (sub_matching_test [,16] - sub_matching_test[,26]) )  
# Diff_mat_test[[9]] <- na.omit ( abs (sub_matching_test [,3]  )) 
# 
# 
# 
# 
# max_test_mat <- data.frame
# max_test_mat <- max( unlist( Diff_mat_test[[1]] ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[2]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[3]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[4]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[5]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[6]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[7]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[8]] ) ) )
# max_test_mat <- rbind(max_test_mat, max( unlist( Diff_mat_test[[9]] ) ) )
# 
# for (i in 1:length(Diff_mat_test ))
# {
#   Diff_mat_test[[i]] <- Diff_mat_test[[i]] / max_test_mat[i,1]
# }
# 
# 
# 
# Diff_nonmat_test <- list()
# Diff_nonmat_test[[1]] <- na.omit ( abs (sub_nonmatching_test [,9] - sub_nonmatching_test[,19]) )  
# Diff_nonmat_test[[2]] <- na.omit ( abs (sub_nonmatching_test [,10] - sub_nonmatching_test[,20]) )  
# Diff_nonmat_test[[3]] <- na.omit ( abs (sub_nonmatching_test [,11] - sub_nonmatching_test[,21]) )  
# Diff_nonmat_test[[4]] <- na.omit ( abs (sub_nonmatching_test [,12] - sub_nonmatching_test[,22]) )  
# Diff_nonmat_test[[5]] <- na.omit ( abs (sub_nonmatching_test [,13] - sub_nonmatching_test[,23]) )  
# Diff_nonmat_test[[6]] <- na.omit ( abs (sub_nonmatching_test [,14] - sub_nonmatching_test[,24]) )  
# Diff_nonmat_test[[7]] <- na.omit ( abs (sub_nonmatching_test [,15] - sub_nonmatching_test[,25]) )  
# Diff_nonmat_test[[8]] <- na.omit ( abs (sub_nonmatching_test [,16] - sub_nonmatching_test[,26]) )  
# Diff_nonmat_test[[9]] <- na.omit ( abs (sub_nonmatching_test [,3]  )) / max_test[9]
# 
# 
# max_test_nonmat <- data.frame
# max_test_nonmat <- max( unlist( Diff_nonmat_test[[1]] ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[2]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[3]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[4]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[5]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[6]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[7]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[8]] ) ) )
# max_test_nonmat <- rbind(max_test_nonmat, max( unlist( Diff_nonmat_test[[9]] ) ) )
# 
# for (i in 1:length(Diff_nonmat_test ))
# {
#   Diff_nonmat_test[[i]] <- Diff_nonmat_test[[i]] / max_test_nonmat[i,1]
# }



# kernel_mat[[1]] <- density (Diff_mat_train[[1]])
# kernel_nonmat[[1]] <- density(Diff_nonmat_train[[1]])
# kernel_mat[[2]] <- density(Diff_mat_train[[2]])
# kernel_nonmat[[2]] <- density(Diff_nonmat_train[[2]])
# kernel_mat[[3]] <- density(Diff_mat_train[[3]])
# kernel_nonmat[[3]] <- density(Diff_nonmat_train[[3]])
# kernel_mat[[4]] <- density(Diff_mat_train[[4]])
# kernel_nonmat[[4]] <- density(Diff_nonmat_train[[4]])
# kernel_mat[[5]] <- density(Diff_mat_train[[5]])
# kernel_nonmat[[5]] <- density(Diff_nonmat_train[[5]])
# kernel_mat[[6]] <- density(Diff_mat_train[[6]])
# kernel_nonmat[[6]] <- density(Diff_nonmat_train[[6]])
# kernel_mat[[7]] <- density(Diff_mat_train[[7]])
# kernel_nonmat[[7]] <- density(Diff_nonmat_train[[7]])
# kernel_mat[[8]] <- density(Diff_mat_train[[8]])
# kernel_nonmat[[8]] <- density(Diff_nonmat_train[[8]])
# kernel_mat[[9]] <- density(Diff_mat_train[[9]])
# kernel_nonmat[[9]] <- density(Diff_nonmat_train[[9]])


