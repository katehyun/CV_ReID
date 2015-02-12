rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData") 
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new_nonN.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new_nonN.RData")
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all, sub_all_train, sub_all_test, sub_matching_train, sub_matching_test, 
   sub_nonmatching_train, sub_nonmatching_test)
options(scipen=999) # non scientific notation

#WHAT TO CHAGE
utcbd <- 1357804800000


### kernal estimation based on NN
# class 9

# USING NON-NORMALIZED data
Downheader_new <- Downheader_new_nonN
Upheader_new <- Upheader_new_nonN

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
matching_highest20_a_magdif <- sort(matchingonly$min_a_magdif)[1:(length(matchingonly)/2)]  
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
colnames(matching_highest20_wim_diff_median)[1:28] <- c( "length", "gvw", 
                              "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", #8
                              "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                              "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                              "ax9lwt", "ax9rwt")

hist( matching_highest20_wim_diff [,1])
mean( matching_highest20_wim_diff [,1])
median( matching_highest20_wim_diff [,1])

# calibrate wim (Up)
Upheader_new_cl <- Upheader_new + matching_highest20_wim_diff_median

# install.packages("stringr")
library(stringr)


# train (01/09)
DownheaderTrainIdx <- which (Downheader_new[,12] > utcbd )
DownheaderTestIdx <- which (Downheader_new[,12] < utcbd )
Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]
prob_train <- prob [ DownheaderTrainIdx]
prob_test <- prob [ DownheaderTestIdx]


sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) > utcbd    ) 
rm(sub_nonmatching_train)
sub_matching_train <- subset(sub_all_train  , as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                               as.numeric (sub_all_train [,5]) != 999)
sub_nonmatching_train <- subset(sub_all_train , as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )




# test
sub_all_test  <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) <= utcbd     ) 
sub_matching_test <- subset(sub_all_test  , as.numeric (sub_all_test  [,5]) == as.numeric(sub_all_test  [,6]) &
                              as.numeric (sub_all_test [,5]) != 999)
sub_nonmatching_test <- subset(sub_all_test , as.numeric (sub_all_test [,5]) != as.numeric( sub_all_test [,6]) )




# Difference (train) 
Diff_mat_train <- list()
Diff_nonmat_train <- list()
# Diff_mat_train[[1]] <- na.omit (  abs(sub_matching_train [,3]  )) 
# Diff_nonmat_train[[1]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 

for ( i in 1:30 )
{
  Diff_mat_train[[i]] <- na.omit ( abs(sub_matching_train [,9+i] - sub_matching_train[,42+i]) ) 
  Diff_nonmat_train[[i]] <- na.omit ( abs(sub_nonmatching_train [,9+i] - sub_nonmatching_train[,42+i]) )  
}

Diff_mat_train[[31]] <- na.omit (  abs(sub_matching_train [,3]  )) 
Diff_nonmat_train[[31]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 


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
mzscoresIndex_mat[[1]] <- rbind (which ( unlist (mzscores_mat[[1]] ) > 3.5) , which( unlist (mzscores_mat[[1]] ) < -3.5  ) )
mzscoresIndex_nonmat[[1]] <- rbind (which ( unlist (mzscores_nonmat[[1]] ) > 3.5) , which( unlist (mzscores_nonmat[[1]] ) < -3.5  ) )

for ( i in 1:30 )
{
  mzscoresIndex_mat[[i+1]] <- 
    rbind (which ( unlist (mzscores_mat[[i+1]] ) > 3.5) , which( unlist (mzscores_mat[[i+1]] ) < -3.5  ) )
  mzscoresIndex_nonmat[[i+1]] <- 
    rbind (which ( unlist (mzscores_nonmat[[i+1]] ) > 3.5) , which( unlist (mzscores_nonmat[[i+1]] ) < -3.5  ) )
}

# collect only clean data
Diff_mat_train_c <- list()
Diff_nonmat_train_c <- list()
Diff_mat_train_c[[1]] <- Diff_mat_train[[1]][ , c(mzscoresIndex_mat[[1]] )]

hsb2.small[, c(1, 7, 8)])
Diff_nonmat_train_c[[1]] <- Diff_nonmat_train[[1]][ !Diff_nonmat_train[[1]] %in% mzscoresIndex_nonmat[[1]]]

for ( i in 1:30 )
{ 
  Diff_mat_train_c[[i+1]] <- Diff_mat_train[[i+1]][ !Diff_mat_train[[i+1]] %in% mzscoresIndex_mat[[i+1]]]
  Diff_nonmat_train_c[[i+1]] <- Diff_nonmat_train[[i+1]][ !Diff_nonmat_train[[i+1]] %in% mzscoresIndex_nonmat[[i+1]]]
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


# kernel : wigh normalized differences
kernel_mat <- list()
kernel_nonmat <- list()


for ( i in 1: 31 )
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {
    
    kernel_mat[[i]] <- density(Diff_mat_train_n[[i]])
    kernel_nonmat[[i]] <- density(Diff_nonmat_train_n[[i]])
  }
  
  else
  {
    kernel_mat$x[[i]] <- NA
    kernel_mat$y[[i]] <- NA
    kernel_nonmat$x[[i]] <-  NA
    kernel_nonmat$y[[i]] <-  NA
  }
  
}

# 
# for ( i in 13: 22 )
# {
#   kernel_mat[[k]] <- density(Diff_mat_train_n[[i]])
#   kernel_nonmat[[k]] <- density(Diff_nonmat_train_n[[i]]) 
#   k <- k+1
# }
# 
# for ( i in 31: 32 )
# {
#   kernel_mat[[k]] <- density(Diff_mat_train_n[[i]])
#   kernel_nonmat[[k]] <- density(Diff_nonmat_train_n[[i]]) 
#   k <- k+1
# }

## fitting to the distribution



# utils:::menuInstallPkgs() 
# library(Hmisc)
# 
# testimpute <- with(Diff_nonmat_train_n[[2]], impute(Diff_nonmat_train_n[[2]]), mean)
# 
# approxExtrap(1:3,1:3,xout=c(0,4))
# 
# testimpute <- approx ( Diff_mat_train_n[[2]], n=1000)

install.packages("fitdistrplus")
library(fitdistrplus)

# to make non-zero data
minval <- 0.00001

for (i in 1: length( Diff_mat_train ) ){
  for (j in 1: length( Diff_mat_train[[1]] ) ){
    
    if ( length( Diff_mat_train_n[[i]])  > 0 ){
      
      if (Diff_mat_train_n[[i]][j] == 0 ){
        Diff_mat_train_n[[i]][j]  <- minval
      }
    }
  }
}

for (i in 1: length( Diff_nonmat_train ) ){
  for (j in 1:length( Diff_nonmat_train[[1]] ) ){
    
    if ( length( Diff_mat_train_n[[i]])  > 0 ){
      if (Diff_nonmat_train_n[[i]][j] == 0 ){
        Diff_nonmat_train_n[[i]][j] <- minval
      }
    }
  }
}



# param kernel
bicall_mat <- data.frame(0,0,0,0,0)
bicall_nonmat <- data.frame(0,0,0,0,0)
j <- 0
k <- 0

for (i in 1: length(Diff_mat_train_n))
{
  if (sum ( Diff_mat_train_n[[i]]) != 0) {
    
    #     j<-j+1
    set.seed(123)
    y <- Diff_mat_train_n[[i]]
    fitw_mat <- fitdist(y, "weibull")
    fitg_mat <- fitdist(y, "gamma")
    fitln_mat <- fitdist(y, "lnorm")
    fite_mat <- fitdist(y, "exp")
    bicall_mat[i,] <- c(fitw_mat$bic, fitg_mat$bic, fitln_mat$bic, fite_mat$bic, 0)
    bicall_mat[i,5] <- which.min(bicall_mat[i,1:4])
    
    #     k<-k+1
    set.seed(123)
    y <- Diff_nonmat_train_n[[i]]
    fitw_nonmat <- fitdist(y, "weibull")
    fitg_nonmat <- fitdist(y, "gamma")
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


# make kernel : with normalized differences
kernel_para_mat <- list()
kernel_para_nonmat <- list()
j <- 0

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

j <- 0
for ( i in 1: 31 )
{
  if (sum ( Diff_nonmat_train_n[[i]]) != 0) {
    
    #     j <- j+1
    
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


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_12152014")
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


