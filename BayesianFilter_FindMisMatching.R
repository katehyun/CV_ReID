rm(list=ls())
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/20141030Jan0910.RData") 
## kernel estimation 
rm(sub_matching, sub_nonmatching, sub_all)


### kernal estimation based on NN

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

# 01/09 data for training
# class 9
sub_all_train <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) < 1357804800000    ) 
sub_all_test  <- subset(sub_all, as.numeric (str_sub (sub_all [,4],-13,-1) ) >= 1357804800000    ) 

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

## Extract attributes

Upcandidates <- list()
Upcandidates_attribute <- list()
Downtarget_attributes <- data.frame()
thresholdForDif <- 1.5
Attribute_diff <- list()
Attribute_difftemp <- list()
Upcandidatesindex<- list()

Downtarget_attributes <- cbind(Downheader_new[,13:20],Downheader_new[,7],Downheader_new[,12]  )

for (i in 1: length(Upsiglist)) {  
  
  Upcandidatesindex[[i]] <- which(a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )
  Upcandidates[[i]] <- subset (Upsiglist[[i]], a_magdif[[i]] < thresholdForDif * min(a_magdif[[i]]) )

  Upcandidates_attribute[[i]] <- cbind(      
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),13:20] ,
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),7] ,
    Upheader_new[ match( as.numeric(Upcandidates[[i]]), as.numeric(Upheader_new[,13])),12])

    for (j in 1: length(Upcandidates[[i]])) {  
    Attribute_difftemp[[j]] <- abs( as.numeric( (unlist (Upcandidates_attribute[[i]][j,3:length(Upcandidates_attribute[[i]])]))) - 
      as.numeric(Downtarget_attributes[i,3:length(Upcandidates_attribute[[i]]) ])  )
    }
  
  Attribute_diff[[length( Attribute_diff)+1]] <- Attribute_difftemp # list in the list
  Attribute_difftemp <- list()
}

#  utils::View( Upcandidates_attribute[[1]] )
# View( Upcandidates_attribute[[i]][j,])
# View( Downtarget_attributes[i,])

# Only Class 9
jointprobtemp <- data.frame()
jointprob <- list()
idxjointprob <- vector()
UpFinalcandidates <- vector()
k<- 0

for (i in 1:length(Upsiglist)){
  if ( as.numeric (Downtarget_attributes [i,2] ) == 9 ) {  k <- k+1
   
  for (j in 1: length(Upcandidates[[i]])) {
    
        jointprobtemp[j,1] <-(approx(kernel.length_m$x, kernel.length_m$y, Attribute_diff[[i]][[j]][1]) )$y
        jointprobtemp[j,2] <-(approx(kernel.gvw_m$x, kernel.gvw_m$y, Attribute_diff[[i]][[j]][2]) )$y
        jointprobtemp[j,3] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][3]) )$y
        jointprobtemp[j,4] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][4]) )$y
        jointprobtemp[j,5] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][5]) )$y
        jointprobtemp[j,6] <-(approx(kernel.axsp12_m$x, kernel.axsp12_m$y, Attribute_diff[[i]][[j]][6]) )$y
        jointprobtemp[j,7] <-(approx(kernel.dur_m$x, kernel.dur_m$y, Attribute_diff[[i]][[j]][7]) )$y
        jointprobtemp[j,8] <-(approx(kernel.utc_m$x, kernel.utc_m$y, Attribute_diff[[i]][[j]][8]) )$y
        jointprobtemp[j,9] <- as.numeric (prob[[i]][[1]][Upcandidatesindex[[i]][j]] ) # CHECK!!!
        
        jointprobtemp [is.na(jointprobtemp )] <- 0.000000000001
        jointprobtemp[j,10] <- jointprobtemp[j,1]* jointprobtemp[j,2] * jointprobtemp[j,3] * jointprobtemp[j,4] *
        jointprobtemp[j,5] * jointprobtemp[j,6] * jointprobtemp[j,7] * jointprobtemp[j,8] * jointprobtemp[j,9] 
    

  }
  jointprob[[length(jointprob)+1]] <-  jointprobtemp
  jointprobtemp <- data.frame()
  idxjointprob[k] <- which.max(unlist (jointprob[[k]][10]) )
  
  UpFinalcandidates[k] <- Upsiglist[[i]][ Upcandidatesindex[[i]][idxjointprob[k]] ]
  }
}

rm(test2)
test2 <- cbind(Target_baseanalysis_Jan0910_table[Target_baseanalysis_Jan0910_table[,1] == 9, 6], UpFinalcandidates)


# performance 
test2 <- na.omit(test2)
TargetTable <-test2
Target_obj  <- test2[,1]

missing_obj  <- length (Target_obj[Target_obj == 999]) 
matching_obj <- length (Target_obj[Target_obj != 999]) 

matching_NN <- sum ( as.numeric (test2[,1]) == as.numeric (test2[,2]) & as.numeric (test2[,1]) != 999)

missing_NN <- sum ( as.numeric (test2[,2]) == c(999))

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

Result2<- data.frame( matching_obj[1], missing_obj[1],              
                     matching_NN[[1]],  missing_NN[[1]],
                     CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
# rm(test)
# options(digits=6)
# test <- cbind(sub_all[,3], sub_all[,5], sub_all[,6],sub_all[,27],sub_all[,28] ,sub_all[,30])
# test <- round(test , digits=6)