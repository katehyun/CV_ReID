rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141125Jan0910.RData") 
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all)
options(scipen=999) # non scientific notation

#WHAT TO CHAGE
utcbd <- 1357804800000


### kernal estimation based on NN
# class 9
sub_all <- TargetTable_NN[[5]][,1:4]
sub_all <- cbind(sub_all,TargetTable_NN[[5]][,6],TargetTable_NN[[5]][,8])
sub_all <- cbind( sub_all ,      
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),13:20] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),7] ,
                  Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),12] ,
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),13:20] , # should be 6?
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),7] ,
                  Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),12] ) 
sub_all <- na.omit(sub_all)

# install.packages("stringr")
library(stringr)


# train (01/09)
DownheaderTrainIdx <- which (Downheader_new[,12] < utcbd )
DownheaderTestIdx <- which (Downheader_new[,12] > utcbd )
Upsiglist_train <- Upsiglist[DownheaderTrainIdx]
Upsiglist_test <- Upsiglist[DownheaderTestIdx]


sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) < utcbd    ) 
sub_matching_train <- subset(sub_all_train  , as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                               as.numeric (sub_all_train [,5]) != 999)
sub_nonmatching_train <- subset(sub_all_train , as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )
sub_matching_train <- cbind( sub_matching_train ,      
                             na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),13:20] ),
                             na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),7] ),
                             na.omit ( Downheader_new[ match( sub_matching_train [,4], as.numeric(Downheader_new[,13])),12] ),
                             na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),13:20] ),
                             na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),7] ),
                             na.omit ( Upheader_new[ match( sub_matching_train [,6], as.numeric(Upheader_new[,13])),12] ) )

sub_nonmatching_train  <- cbind( sub_nonmatching_train ,
                                 Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),13:20],
                                 Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),7],
                                 Downheader_new[ match( sub_nonmatching_train [,4], as.numeric(Downheader_new[,13])),12],
                                 Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),13:20],
                                 Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),7],
                                 Upheader_new[ match( sub_nonmatching_train [,6], as.numeric(Upheader_new[,13])),12])

colnames(sub_matching_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_matching_train )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_train )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_train )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_train )[19:26 ] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")


# test
sub_all_test  <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) >= utcbd     ) 
sub_matching_test <- subset(sub_all_test  , as.numeric (sub_all_test  [,5]) == as.numeric(sub_all_test  [,6]) &
                              as.numeric (sub_all_test [,5]) != 999)
sub_nonmatching_test <- subset(sub_all_test , as.numeric (sub_all_test [,5]) != as.numeric( sub_all_test [,6]) )
sub_matching_test <- cbind( sub_matching_test ,      
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),13:20] ),
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),7] ),
                            na.omit ( Downheader_new[ match( sub_matching_test [,4], as.numeric(Downheader_new[,13])),12] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),13:20] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),7] ),
                            na.omit ( Upheader_new[ match( sub_matching_test [,6], as.numeric(Upheader_new[,13])),12] ) )

sub_nonmatching_test  <- cbind( sub_nonmatching_test ,
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),13:20],
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),7],
                                Downheader_new[ match( sub_nonmatching_test [,4], as.numeric(Downheader_new[,13])),12],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),13:20],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),7],
                                Upheader_new[ match( sub_nonmatching_test [,6], as.numeric(Upheader_new[,13])),12])

colnames(sub_matching_test )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_matching_test )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_test )[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching_test )[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_test)[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all_test)[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")


# normalized factor (train set)
max_train <- vector()
max_train[1] <- max(abs( na.omit(sub_all_train[,9] - sub_all_train[,19]) )) #length
max_train[2] <- max(abs( na.omit(sub_all_train[,10] -sub_all_train[,20]) )) # gvw
max_train[3] <- max(abs( na.omit(sub_all_train[,11] - sub_all_train[,21]) )) # ax12sp
max_train[4] <- max(abs( na.omit(sub_all_train[,12] - sub_all_train[,22]) )) # ax23sp
max_train[5] <- max(abs( na.omit(sub_all_train[,13] - sub_all_train[,23]) )) # ax34sp
max_train[6] <- max(abs( na.omit(sub_all_train[,14] - sub_all_train[,24]) )) # ax45sp
max_train[7] <- max(abs( na.omit(sub_all_train[,15] - sub_all_train[,25]) )) # dur
max_train[8] <- max(abs( na.omit(sub_all_train[,16] - sub_all_train[,26]) )) #utc
max_train[9] <- max( na.omit(sub_all_train[,3]) ) # sum mag diff


# normalized factor (test set)
max_test <- vector()
max_test[1] <- max(abs( na.omit(sub_all_test[,9] - sub_all_test[,19]) ))
max_test[2] <- max(abs( na.omit(sub_all_test[,10] - sub_all_test[,20]) ))
max_test[3] <- max(abs( na.omit(sub_all_test[,11] - sub_all_test[,21]) ))
max_test[4] <- max(abs( na.omit(sub_all_test[,12] - sub_all_test[,22]) ))
max_test[5] <- max(abs( na.omit(sub_all_test[,13] - sub_all_test[,23]) ))
max_test[6] <- max(abs( na.omit(sub_all_test[,14] - sub_all_test[,24]) ))
max_test[7] <- max(abs( na.omit(sub_all_test[,15] - sub_all_test[,25]) ))
max_test[8] <- max(abs( na.omit(sub_all_test[,16] - sub_all_test[,26]) ))
max_test[9] <- max( na.omit(sub_all_test[,3]) )

# normalized Difference (train)
Diff_mat_train <- list()
Diff_mat_train[[1]] <- na.omit ( abs (sub_matching_train [,9] - sub_matching_train[,19]) )   / max_train[1]
Diff_mat_train[[2]] <- na.omit ( abs (sub_matching_train [,10] - sub_matching_train[,20]) )  / max_train[2]
Diff_mat_train[[3]] <- na.omit ( abs (sub_matching_train [,11] - sub_matching_train[,21]) )  / max_train[3]
Diff_mat_train[[4]] <- na.omit ( abs (sub_matching_train [,12] - sub_matching_train[,22]) )  / max_train[4]
Diff_mat_train[[5]] <- na.omit ( abs (sub_matching_train [,13] - sub_matching_train[,23]) )  / max_train[5]
Diff_mat_train[[6]] <- na.omit ( abs (sub_matching_train [,14] - sub_matching_train[,24]) )  / max_train[6]
Diff_mat_train[[7]] <- na.omit ( abs (sub_matching_train [,15] - sub_matching_train[,25]) )  / max_train[7]
Diff_mat_train[[8]] <- na.omit ( abs (sub_matching_train [,16] - sub_matching_train[,26]) )  / max_train[8]
Diff_mat_train[[9]] <- na.omit ( abs (sub_matching_train [,3]  )) / max_train[9]

Diff_nonmat_train <- list()
Diff_nonmat_train[[1]] <- na.omit ( abs (sub_nonmatching_train [,9] - sub_nonmatching_train[,19]) )   / max_train[1]
Diff_nonmat_train[[2]] <- na.omit ( abs (sub_nonmatching_train [,10] - sub_nonmatching_train[,20]) )  / max_train[2]
Diff_nonmat_train[[3]] <- na.omit ( abs (sub_nonmatching_train [,11] - sub_nonmatching_train[,21]) )  / max_train[3]
Diff_nonmat_train[[4]] <- na.omit ( abs (sub_nonmatching_train [,12] - sub_nonmatching_train[,22]) )  / max_train[4]
Diff_nonmat_train[[5]] <- na.omit ( abs (sub_nonmatching_train [,13] - sub_nonmatching_train[,23]) )  / max_train[5]
Diff_nonmat_train[[6]] <- na.omit ( abs (sub_nonmatching_train [,14] - sub_nonmatching_train[,24]) )  / max_train[6]
Diff_nonmat_train[[7]] <- na.omit ( abs (sub_nonmatching_train [,15] - sub_nonmatching_train[,25]) )  / max_train[7]
Diff_nonmat_train[[8]] <- na.omit ( abs (sub_nonmatching_train [,16] - sub_nonmatching_train[,26]) )  / max_train[8]
Diff_nonmat_train[[9]] <- na.omit ( abs (sub_nonmatching_train [,3]  ))  / max_train[9]

# normalized Difference (test)
Diff_mat_test <- list()
Diff_mat_test[[1]] <- na.omit ( abs (sub_matching_test [,9] - sub_matching_test[,19]) )   / max_test[1]
Diff_mat_test[[2]] <- na.omit ( abs (sub_matching_test [,10] - sub_matching_test[,20]) )  / max_test[2]
Diff_mat_test[[3]] <- na.omit ( abs (sub_matching_test [,11] - sub_matching_test[,21]) )  / max_test[3]
Diff_mat_test[[4]] <- na.omit ( abs (sub_matching_test [,12] - sub_matching_test[,22]) )  / max_test[4]
Diff_mat_test[[5]] <- na.omit ( abs (sub_matching_test [,13] - sub_matching_test[,23]) )  / max_test[5]
Diff_mat_test[[6]] <- na.omit ( abs (sub_matching_test [,14] - sub_matching_test[,24]) )  / max_test[6]
Diff_mat_test[[7]] <- na.omit ( abs (sub_matching_test [,15] - sub_matching_test[,25]) )  / max_test[7]
Diff_mat_test[[8]] <- na.omit ( abs (sub_matching_test [,16] - sub_matching_test[,26]) )  / max_test[8]
Diff_mat_test[[9]] <- na.omit ( abs (sub_matching_test [,3]  )) / max_test[9]

Diff_nonmat_test <- list()
Diff_nonmat_test[[1]] <- na.omit ( abs (sub_nonmatching_test [,9] - sub_nonmatching_test[,19]) )   / max_test[1]
Diff_nonmat_test[[2]] <- na.omit ( abs (sub_nonmatching_test [,10] - sub_nonmatching_test[,20]) )  / max_test[2]
Diff_nonmat_test[[3]] <- na.omit ( abs (sub_nonmatching_test [,11] - sub_nonmatching_test[,21]) )  / max_test[3]
Diff_nonmat_test[[4]] <- na.omit ( abs (sub_nonmatching_test [,12] - sub_nonmatching_test[,22]) )  / max_test[4]
Diff_nonmat_test[[5]] <- na.omit ( abs (sub_nonmatching_test [,13] - sub_nonmatching_test[,23]) )  / max_test[5]
Diff_nonmat_test[[6]] <- na.omit ( abs (sub_nonmatching_test [,14] - sub_nonmatching_test[,24]) )  / max_test[6]
Diff_nonmat_test[[7]] <- na.omit ( abs (sub_nonmatching_test [,15] - sub_nonmatching_test[,25]) )  / max_test[7]
Diff_nonmat_test[[8]] <- na.omit ( abs (sub_nonmatching_test [,16] - sub_nonmatching_test[,26]) )  / max_test[8]
Diff_nonmat_test[[9]] <- na.omit ( abs (sub_nonmatching_test [,3]  )) / max_test[9]


kernel_mat <- list()
kernel_nonmat <- list()
kernel_mat[[1]] <- density(Diff_mat_train[[1]])
kernel_nonmat[[1]] <- density(Diff_nonmat_train[[1]])
kernel_mat[[2]] <- density(Diff_mat_train[[2]])
kernel_nonmat[[2]] <- density(Diff_nonmat_train[[2]])
kernel_mat[[3]] <- density(Diff_mat_train[[3]])
kernel_nonmat[[3]] <- density(Diff_nonmat_train[[3]])
kernel_mat[[4]] <- density(Diff_mat_train[[4]])
kernel_nonmat[[4]] <- density(Diff_nonmat_train[[4]])
kernel_mat[[5]] <- density(Diff_mat_train[[5]])
kernel_nonmat[[5]] <- density(Diff_nonmat_train[[5]])
kernel_mat[[6]]<- density(Diff_mat_train[[6]])
kernel_nonmat[[6]] <- density(Diff_nonmat_train[[6]])
kernel_mat[[7]] <- density(Diff_mat_train[[7]])
kernel_nonmat[[7]] <- density(Diff_nonmat_train[[7]])
kernel_mat[[8]] <- density(Diff_mat_train[[8]])
kernel_nonmat[[8]] <- density(Diff_nonmat_train[[8]])
kernel_mat[[9]] <- density(Diff_mat_train[[9]])
kernel_nonmat[[9]] <- density(Diff_nonmat_train[[9]])



save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_12032014")
