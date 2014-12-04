rm(sub_all_v2)
UpFinalcandidates <-as.numeric (unlist(UpFinalcandidates) )
Target_baseanalysis_Jan0910_table_class9 <- subset( Target_baseanalysis_Jan0910_table, 
                                                    as.numeric (Target_baseanalysis_Jan0910_table[,1]) == 9  )
sub_all_v2 <-  Target_baseanalysis_Jan0910_table_class9[,1:4]
                     
sub_all_v2 <- cbind(sub_all_v2,Target_baseanalysis_Jan0910_table_class9[,6])
sub_all_v2 <- cbind(sub_all_v2,UpFinalcandidates)
sub_all_v2 <- cbind( sub_all_v2,      
                  Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),13:20] ,
                  Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),7] ,
                  Downheader_new[ match( sub_all_v2[,4], as.numeric(Downheader_new[,13])),12] ,
                  Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),13:20] , # should be 6?
                  Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),7] ,
                  Upheader_new[ match( sub_all_v2[,6], as.numeric(Upheader_new[,13])),12] ) 
sub_all_v2 <- na.omit(sub_all_v2)


### approaximation

sub_all_diff <- abs(sub_all_v2[9:16] - sub_all_v2[,19:26])
sub_all_diff <- cbind(sub_all_diff, sub_all_v2[,3])

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
weight <- 1
sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2] *
  sub_all_diff[,q+3] * sub_all_diff[,q+4] * sub_all_diff[,q+5] * sub_all_diff[,q+6] * 
  sub_all_diff[,q+7] * sub_all_diff[,q+8] * weight * sub_all_diff[,q+9] * 100000000000000 # version 1 : /w distance
# 
# sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,q+1] * sub_all_diff[,q+2]*
#   sub_all_diff[,q+3] * sub_all_diff[,q+4]* sub_all_diff[,q+5]* sub_all_diff[,q+6]* 
#   sub_all_diff[,q+7]* sub_all_diff[,q+8]* 100000000000000 # version 2 : /wo distance

qq <- 18
sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
  sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
  sub_all_diff[,qq+7]* sub_all_diff[,qq+8]*  weight * sub_all_diff[,qq+9] * 100000000000000 # version 1 : /w distance

# sub_all[,length(sub_all[1,])+1] <- sub_all_diff[,qq+1] * sub_all_diff[,qq+2]* sub_all_diff[,qq+3] * 
#   sub_all_diff[,qq+4]* sub_all_diff[,qq+5]* sub_all_diff[,qq+6]*
#   sub_all_diff[,qq+7]* sub_all_diff[,qq+8] * 100000000000000 # version 2 : /wo distance

sub_all_v2[,length(sub_all_v2[1,])+1] <- sub_all_v2[,length(sub_all_v2[1,])-1] > 
  sub_all_v2[,length(sub_all_v2[1,])]


col <- length(sub_all_v2[1,])
for (i in 1: length( sub_all_v2[,col])) {
  if (sub_all_v2[i,col] == TRUE ){
    sub_all_v2[i,col+1] <- as.numeric( sub_all_v2[i,6]  )
  }
  else {
    sub_all_v2[i,col+1] <- 999
  } 
}



# test

Target_obj <- sub_all_v2[,5]
a_Upid_after <- sub_all_v2[,30]
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

Result<- data.frame( matching_obj[1], missing_obj[1],              
                     matching_NN[[1]],  missing_NN[[1]],
                     CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
rm(test)
options(digits=6)
test <- cbind(sub_all_v2[,3], sub_all_v2[,5], sub_all_v2[,6],sub_all_v2[,27],sub_all_v2[,28] ,sub_all_v2[,30])
test <- round(test , digits=6)
# sub_all <- sub_all[,-27:-29]

View(Result)
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
