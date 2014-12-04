rm(sub_matching, sub_nonmatching, sub_all)
# thresholdall <- seq (20-20+9, 120-20+9, by=1);
# k <- 1

### kernal estimation
# for (threshold in thresholdall) {
sub_all <- TargetTable_NN[[5]][,1:4]
sub_all <- cbind(sub_all,TargetTable_NN[[5]][,6],TargetTable_NN[[5]][,threshold])
sub_all <- cbind( sub_all,      
                      Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),13:20] ,
                      Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),7] ,
                      Downheader_new[ match( sub_all[,4], as.numeric(Downheader_new[,13])),12] ,
                      Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),13:20] , # should be 6?
                      Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),7] ,
                      Upheader_new[ match( sub_all[,6], as.numeric(Upheader_new[,13])),12] ) 
sub_all <- na.omit(sub_all)

# install.packages("stringr")
# library(stringr)
# 01/09 data for training
sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all[,4],-13,-1) ) < 1357804800000    ) 
sub_all_test <- subset(sub_all, as.numeric (str_sub (sub_all[,4],-13,-1) ) >= 1357804800000    ) 

sub_matching <- subset(sub_all_train  , as.numeric (sub_all_train  [,5]) == as.numeric(sub_all_train  [,6]) &
                         as.numeric (sub_all_train [,5]) != 999)
sub_nonmatching <- subset(sub_all_train , as.numeric (sub_all_train [,5]) != as.numeric( sub_all_train [,6]) )
sub_matching <- cbind( sub_matching,      
                na.omit ( Downheader_new[ match( sub_matching[,4], as.numeric(Downheader_new[,13])),13:20] ),
                na.omit ( Downheader_new[ match( sub_matching[,4], as.numeric(Downheader_new[,13])),7] ),
                na.omit ( Downheader_new[ match( sub_matching[,4], as.numeric(Downheader_new[,13])),12] ),
                na.omit ( Upheader_new[ match( sub_matching[,6], as.numeric(Upheader_new[,13])),13:20] ),
                na.omit ( Upheader_new[ match( sub_matching[,6], as.numeric(Upheader_new[,13])),7] ),
                na.omit ( Upheader_new[ match( sub_matching[,6], as.numeric(Upheader_new[,13])),12] ) )

sub_nonmatching <- cbind( sub_nonmatching,
                        Downheader_new[ match( sub_nonmatching[,4], as.numeric(Downheader_new[,13])),13:20],
                        Downheader_new[ match( sub_nonmatching[,4], as.numeric(Downheader_new[,13])),7],
                        Downheader_new[ match( sub_nonmatching[,4], as.numeric(Downheader_new[,13])),12],
                        Upheader_new[ match( sub_nonmatching[,6], as.numeric(Upheader_new[,13])),13:20],
                        Upheader_new[ match( sub_nonmatching[,6], as.numeric(Upheader_new[,13])),7],
                        Upheader_new[ match( sub_nonmatching[,6], as.numeric(Upheader_new[,13])),12])

colnames(sub_matching)[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_matching)[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching)[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_nonmatching)[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all)[9:16] <- c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")
colnames(sub_all)[19:26] <-  c("length", "gvw", "ax12sp", "ax23sp", "ax34sp", "ax45sp", "duration", "utc")


Diff.lengthdiff_m <- na.omit( abs (sub_matching [,9] - sub_matching[,19]) )
Diff.gvwdiff_m    <- na.omit( abs (sub_matching [,10] - sub_matching[,20]) )
Diff.ax12spdiff_m <- na.omit( abs (sub_matching [,11] - sub_matching[,21]) )
Diff.ax23spdiff_m <- na.omit( abs (sub_matching [,12] - sub_matching[,22]) )
Diff.ax34spdiff_m <- na.omit( abs (sub_matching [,13] - sub_matching[,23]) )
Diff.ax45spdiff_m <- na.omit( abs (sub_matching [,14] - sub_matching[,24]) )
Diff.ax45spdiff_m <- na.omit( abs (sub_matching [,14] - sub_matching[,24]) )
Diff.durdiff_m    <- na.omit( abs (sub_matching [,15] - sub_matching[,25]) )
Diff.utcdiff_m    <- na.omit( abs (sub_matching [,16] - sub_matching[,26]) )
Diff.magdiff_m    <- na.omit( abs (sub_matching [,2]  ))


Diff.lengthdiff_nm <- na.omit ( abs (sub_nonmatching [,9] - sub_nonmatching[,19]) )
Diff.gvwdiff_nm    <- na.omit( abs (sub_nonmatching [,10] - sub_nonmatching[,20]) )
Diff.ax12spdiff_nm <- na.omit( abs (sub_nonmatching [,11] - sub_nonmatching[,21]) )
Diff.ax23spdiff_nm <- na.omit( abs (sub_nonmatching [,12] - sub_nonmatching[,22]) )
Diff.ax34spdiff_nm <- na.omit( abs (sub_nonmatching [,13] - sub_nonmatching[,23]) )
Diff.ax45spdiff_nm <- na.omit( abs (sub_nonmatching [,14] - sub_nonmatching[,24]) )
Diff.ax45spdiff_nm <- na.omit( abs (sub_nonmatching [,14] - sub_nonmatching[,24]) )
Diff.durdiff_nm    <- na.omit( abs (sub_nonmatching [,15] - sub_nonmatching[,25]) )
Diff.utcdiff_nm    <- na.omit( abs (sub_nonmatching [,16] - sub_nonmatching[,26]) )
Diff.magdiff_nm   <- na.omit( abs (sub_nonmatching [,2]  ))


# norm.length <- density ( rnorm(n=length(Diff.lengthdiff_m),m=mean(Diff.lengthdiff_m),sd=sd(Diff.lengthdiff_m)) )
# approx(norm.length$x, norm.length$y, c(5, 0, 1, 2)) # plug in new data

kernel.length_m <- density(Diff.lengthdiff_m)
kernel.length_nm <- density(Diff.lengthdiff_nm)
kernel.gvw_m <- density(Diff.gvwdiff_m)
kernel.gvw_nm <- density(Diff.gvwdiff_nm)
kernel.axsp12_m <- density(Diff.ax12spdiff_m)
kernel.axsp12_nm <- density(Diff.ax12spdiff_nm)
kernel.axsp23_m <- density(Diff.ax23spdiff_m)
kernel.axsp23_nm <- density(Diff.ax23spdiff_nm)
kernel.axsp34_m <- density(Diff.ax34spdiff_m)
kernel.axsp34_nm <- density(Diff.ax34spdiff_nm)
kernel.axsp45_m <- density(Diff.ax45spdiff_m)
kernel.axsp45_nm <- density(Diff.ax45spdiff_nm)
kernel.dur_m <- density(Diff.durdiff_m)
kernel.dur_nm <- density(Diff.durdiff_nm)
kernel.utc_m <- density(Diff.utcdiff_m)
kernel.utc_nm <- density(Diff.utcdiff_nm)
kernel.magdif_m <- density(Diff.magdiff_m)
kernel.magdif_nm <- density(Diff.magdiff_nm)


### approaximation

sub_all_diff <- abs(sub_all[,9:16] - sub_all[,19:26])
sub_all_diff <- cbind( sub_all_diff, sub_all[,3])

q=9
sub_all_diff[,q+1] <-(approx(kernel.length_m$x, kernel.length_m$y, sub_all_diff[,1]) )$y
sub_all_diff[,q+2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, sub_all_diff[,2]) )$y
sub_all_diff[,q+3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,3]) )$y
sub_all_diff[,q+4] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,4]) )$y
sub_all_diff[,q+5] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,5]) )$y
sub_all_diff[,q+6] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, sub_all_diff[,6]) )$y
sub_all_diff[,q+7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, sub_all_diff[,7]) )$y
sub_all_diff[,q+8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, sub_all_diff[,8]) )$y
sub_all_diff[,q+9] <-(approx(kernel.magdif_m$x, kernel.magdif_m$y, sub_all_diff[,9]) )$y

qq=9+9
sub_all_diff[,qq+1] <-(approx(kernel.length_nm$x, kernel.length_nm$y, sub_all_diff[,1]) )$y
sub_all_diff[,qq+2] <-(approx(kernel.gvw_nm$x, kernel.gvw_nm$y, sub_all_diff[,2]) )$y
sub_all_diff[,qq+3] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,3]) )$y
sub_all_diff[,qq+4] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,4]) )$y
sub_all_diff[,qq+5] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,5]) )$y
sub_all_diff[,qq+6] <-(approx(kernel.axsp12_nm$x, kernel.axsp12_nm$y, sub_all_diff[,6]) )$y
sub_all_diff[,qq+7] <-(approx(kernel.dur_nm$x, kernel.dur_nm$y, sub_all_diff[,7]) )$y
sub_all_diff[,qq+8] <-(approx(kernel.utc_nm$x, kernel.utc_nm$y, sub_all_diff[,8]) )$y
sub_all_diff[,qq+9] <-(approx(kernel.magdif_nm$x, kernel.magdif_nm$y, sub_all_diff[,9]) )$y

sub_all_diff [is.na(sub_all_diff )] <- 0.000000000001

colnames(sub_all_diff)[10:18] <- c("mlength", "mgvw", "max12sp", "max23sp", "max34sp", "max45sp", 
                                     "mduration", "mutc", "mmagdif")
colnames(sub_all_diff)[19:27] <- c("nmlength", "nmgvw", "nmax12sp", "nmax23sp", "nmax34sp", "nmax45sp", 
                                     "nmduration", "nmutc", "nmmagdif")

q <- 9
# sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2]*
#   sub_all_diff[,q+3] * sub_all_diff[,q+4]* sub_all_diff[,q+5]* sub_all_diff[,q+6]* 
#   sub_all_diff[,q+7]* sub_all_diff[,q+8]* sub_all_diff[,q+9]* 100000000000000 # version 1 : /w distance

sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2]*
  sub_all_diff[,q+3] * sub_all_diff[,q+4]* sub_all_diff[,q+5]* sub_all_diff[,q+6]* 
  sub_all_diff[,q+7]* sub_all_diff[,q+8]* 100000000000000 # version 2 : /wo distance

qq <- 18
# sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
#   sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
#   sub_all_diff[,qq+7]* sub_all_diff[,qq+8]* sub_all_diff[,qq+9] * 100000000000000 # version 1 : /w distance

sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
  sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
  sub_all_diff[,qq+7]* sub_all_diff[,qq+8] * 100000000000000 # version 2 : /wo distance

sub_all[,length(sub_all[1,])+1] <- sub_all[,length(sub_all[1,])-1] > sub_all[,length(sub_all[1,])]


col <- length(sub_all[1,])
for (i in 1: length( sub_all[,col])) {
  if (sub_all[i,col] == TRUE ){
    sub_all[i,col+1] <- sub_all[i,6]  
  }
  else {
    sub_all[i,col+1] <- 999
  } 
}



# test

Target_obj <- sub_all[,5]
a_Upid_after <- sub_all[,30]
# a_Upid_after <- sub_all[,6]


# Target_obj <- TargetTable9[,6]
# # a_Upid_after <- sub_all[,30]
# a_Upid_after <- TargetTable9[,47-20+8]

rm(TargetTable)
TargetTable <- cbind (Target_obj , a_Upid_after )
missing_obj <- length (Target_obj[Target_obj == 999]) 
matching_obj <- length (Target_obj[Target_obj != 999]) 

matching_NN <- sum( Target_obj == a_Upid_after & Target_obj!= 999)

missing_NN <- sum (a_Upid_after == c(999))

CMVeh <-  matching_NN[1]
CVeh <- matching_obj[1]
p <- 2
MVeh <- sum(   (as.numeric( TargetTable[,p])) > 1000 )  

SIMR <- CMVeh / CVeh
SCMR <- CMVeh / MVeh

MMVeh <- length(  subset(TargetTable[,1], as.numeric( Target_obj ) 
                         !=  as.numeric( TargetTable[,p])   ))


Veh <- length(TargetTable[,1])
SER <- MMVeh / Veh

Result<- data.frame(threshold, matching_obj[1], missing_obj[1],              
                     matching_NN[[1]],  missing_NN[[1]],
                     CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
rm(test)
options(digits=6)
test <- cbind(sub_all[,3], sub_all[,5], sub_all[,6],sub_all[,27],sub_all[,28] ,sub_all[,30])
test <- round(test , digits=6)
# sub_all <- sub_all[,-27:-29]

#########################################end
# 
# # non-parametric approaximation
# hist(data, breaks = 5, freq = F, xlab = 'Length (ppb)', ylim = c(0, 0.5), ylab = 'Probability',
#      main = 'Histogram of Matching  Length Data with Kernel Density Plot')
# lines(density(data, na.rm = T, from = 0, to = max(data)))
# dev.off()
# 
# # histogram with normal density curve
# data.histogram = hist(data, breaks = 5, freq = F)
# data.norm = dnorm(data, mean = mean(data), sd = sd(data))
# data.ylim.normal = range(0, data.histogram$density, dnorm(data, mean = mean(data), sd = sd(data)), na.rm = T)
# hist(data, breaks = 5, freq = F, ylim = c(0, 0.5), xlab = 'length', ylab = 'Probability', 
#      main = 'Histogram of Ozone Pollution Data with Normal Density Curve')
# curve(dnorm(x, mean = mean(data), sd = sd(data)), add = T)
# 
# # histogram with  gamma  density curve
# data.histogram = hist(data, breaks = 50, freq = F)
# data.ylim.normal = range(0, data.histogram$density, dnorm(data, mean = mean(data), sd = sd(data)), na.rm = T)
# hist(data, breaks = 15, freq = F, ylim = c(0, 0.1), xlab = 'length', ylab = 'Probability', 
#      main = 'Histogram of Ozone Pollution Data with Gamma Density Curve')
# curve(dgamma(x, shape = mean(data)^2/var(data), scale = var(data)/mean(data)), add = T)
