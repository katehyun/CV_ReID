rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141215Jan0910.RData") 
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all, sub_all_train, sub_all_test, sub_matching_train, sub_matching_test, 
   sub_nonmatching_train, sub_nonmatching_test)
options(scipen=999) # non scientific notation

#WHAT TO CHAGE
utcbd <- 1357804800000


### kernal estimation based on NN
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





colnames(sub_all)[5:7] <- c("objup", "upsig")
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



max_train_mat <- vector()
min_train_mat <- vector()
max_train_nonmat <- vector()
min_train_nonmat <- vector()
max_train_mat[1] <- max( unlist( Diff_mat_train[[1]] ) )
min_train_mat[1] <- min( unlist( Diff_mat_train[[1]] ) )
max_train_nonmat[1] <- max( unlist( Diff_nonmat_train[[1]] ) )
min_train_nonmat[1] <- min( unlist( Diff_nonmat_train[[1]] ) )

for ( i in 1:30 )
{
  max_train_mat[i+1] <-  max ( unlist( Diff_mat_train[[i+1]] ) )
  min_train_mat[i+1] <-  min ( unlist( Diff_mat_train[[i+1]] ) )
  max_train_nonmat[i+1] <-  max ( unlist( Diff_nonmat_train[[i+1]] ) )
  min_train_nonmat[i+1] <-  min ( unlist( Diff_nonmat_train[[i+1]] ) )
}


# Normalized Difference (train) 
Diff_mat_train_n <- list()
Diff_nonmat_train_n <- list()
Diff_mat_train_n[[1]] <- na.omit ( ( Diff_mat_train[[1]]  - min_train_mat[1] ) / 
                                    ( max_train_mat[1] - min_train_mat[1] )  )
Diff_nonmat_train_n[[1]] <- na.omit (  ( Diff_nonmat_train[[1]] - min_train_nonmat[1] ) / 
                                    ( max_train_nonmat[1] - min_train_nonmat[1] )  )

for ( i in 1:30 )
{
  Diff_mat_train_n[[i+1]] <- na.omit ( ( Diff_mat_train[[1+i]] - min_train_mat[1+i] ) /  
                                         ( max_train_mat[1+i] - min_train_mat[1+i] )  )
  Diff_nonmat_train_n[[i+1]] <- na.omit ( ( Diff_nonmat_train[[1+i]] -  min_train_nonmat[1+i] ) /  
                                         ( max_train_nonmat[1+i] - min_train_nonmat[1+i] )  ) 
}


# Percent Difference (train) 
Diff_mat_train_pd <- list()
Diff_nonmat_train_pd <- list()


for ( i in 1:30 )
{
  Diff_mat_train_pd[[i]] <-  Diff_mat_train[[i+1]]/ sub_matching_train [,9+i]  
  Diff_nonmat_train_pd[[i]] <-  Diff_nonmat_train[[i+1]] / sub_nonmatching_train [,9+i] 
}
Diff_mat_train_pd[[31]] <- na.omit (  abs(sub_matching_train [,3]  )) 
Diff_nonmat_train_pd[[31]] <- na.omit (  abs(sub_nonmatching_train [,3]  )) 



max_train_mat_pd <- vector()
min_train_mat_pd <- vector()
max_train_nonmat_pd <- vector()
min_train_nonmat_pd <- vector()
max_train_mat_pd[1] <- max( unlist( Diff_mat_train_pd[[1]] ) )
min_train_mat_pd[1] <- min( unlist( Diff_mat_train_pd[[1]] ) )
max_train_nonmat_pd[1] <- max( unlist( Diff_nonmat_train_pd[[1]] ) )
min_train_nonmat_pd[1] <- min( unlist( Diff_nonmat_train_pd[[1]] ) )


for ( i in 1:30 )
{
  max_train_mat_pd[i+1] <-  max ( unlist( Diff_mat_train_pd[[i+1]] ) )
  min_train_mat_pd[i+1] <-  min ( unlist( Diff_mat_train_pd[[i+1]] ) )
  max_train_nonmat_pd[i+1] <-  max ( unlist( Diff_nonmat_train_pd[[i+1]] ) )
  min_train_nonmat_pd[i+1] <-  min ( unlist( Diff_nonmat_train_pd[[i+1]] ) )
}



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

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Kernel_12152014")
## end

# utils:::menuInstallPkgs() 
# library(Hmisc)
# 
# testimpute <- with(Diff_nonmat_train_n[[2]], impute(Diff_nonmat_train_n[[2]]), mean)
# 
# approxExtrap(1:3,1:3,xout=c(0,4))
# 
# testimpute <- approx ( Diff_mat_train_n[[2]], n=1000)


library(MASS)

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




fitdistr(Diff_nonmat_train_n[[2]], "beta")

op <- options(digits = 3)
set.seed(123)
normaldist<- fitdistr(Diff_mat_train_n[[2]], "normal")
set.seed(123)
hist(Diff_nonmat_train_n[[2]])
fitdistr(Diff_nonmat_train_n[[2]], "gamma")
curve(dgamma(x, scale=0.0587, shape=0.2723),from=0, to=1, main="Gamma distribution")

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

fitW <- fitdist(y, "weibull")
fitg <- fitdist(y, "gamma")
fitln <- fitdist(y, "lnorm")
summary(fitW)
summary(fitg)
summary(fitln)
cdfcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
denscomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
qqcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
ppcomp(list(fitW, fitg, fitln), legendtext=c("Weibull", "gamma", "lognormal"))
gofstat(list(fitW, fitg, fitln), fitnames=c("Weibull", "gamma", "lognormal"))


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



