rm(errormag, error_mag, errorsum)

errormag <- data.frame()
maxerror <- c()
minerror <- c()
meanerror <- c()

for (i in 1: length(Target_baseanalysis_Jan0910_table[,1])){
  
  Downsigid <- as.numeric(Target_baseanalysis_Jan0910_table[i,4])
  Upsigid <- as.numeric(Target_baseanalysis_Jan0910_table[i,8])
  FHWAclass<- as.numeric( Target_baseanalysis_Jan0910_table[i,1])

  error_mag <-  f.ErrorMag (Upsigid, Downsigid)
  
        if ( (Target_baseanalysis_Jan0910_table[i,6] ==  Target_baseanalysis_Jan0910_table[i,7]) & 
               (Target_baseanalysis_Jan0910_table[i,7] != 999)){
          error_mag[3] <- 1 }
        else {
          error_mag[3] <- 2
      }

  maxerror[i] <- max(error_mag[5: length(error_mag)])
  minerror[i] <- min(error_mag[5: length(error_mag)])
  meanerror[i] <- mean(error_mag[5: length(error_mag)])
  errormag <- rbind( errormag, error_mag)
  
}

FHWAclass<- as.numeric( Target_baseanalysis_Jan0910_table[,1])
errormag <- cbind(FHWAclass, errormag)

colnames(errormag)[1] <- c("FHWAclass")
colnames(errormag)[2] <- c("Downsigid")
colnames(errormag)[3] <- c("Upsigid")
colnames(errormag)[4] <- c("index")


rm(errorsum)
errorsum <- cbind( errormag$Downsigid, errormag$Upsigid, Target_baseanalysis_Jan0910_table[,1], errormag$index, rowSums(errormag[5:length(errormag[1,])]) , 
                   maxerror, minerror, meanerror )

colnames(errorsum)[1] <- c("Downsigid")
colnames(errorsum)[2] <- c("Upsigid")
colnames(errorsum)[3] <- c("FHWAClass")
colnames(errorsum)[4] <- c("index")
colnames(errorsum)[5] <- c("AUC")

wimsigbodytype=read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/wimsigbodytype.txt", header=T, fill=T,sep="\t")
truckbodytype_Down <- wimsigbodytype[match ( errormag[,2], wimsigbodytype$wimsigid), 8]
trailerbodytype_Down <- wimsigbodytype[match ( errormag[,2], wimsigbodytype$wimsigid), 9]
truckbodytype_Up <- wimsigbodytype[match ( errormag[,3], wimsigbodytype$wimsigid), 8]
trailerbodytype_Up <- wimsigbodytype[match ( errormag[,3], wimsigbodytype$wimsigid), 9]


# aggregated bodytype (agtruck, agtrailer)

trailerbodylookup = read.table("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/RawData/trailerbodyconfiguration.txt", 
                               header=T, sep="," ,fill=T)
  
trailerbodygroup_Down <- trailerbodylookup [ match ( trailerbodytype_Down,  trailerbodylookup[,2]), 4]
trailerbodygroup_Up <- trailerbodylookup [ match ( trailerbodytype_Up,  trailerbodylookup[,2]), 4]

errorsum <- cbind( errorsum , truckbodytype, trailerbodytype,trailerbodygroup_Down, trailerbodygroup_Up)
errormag <- cbind( trailerbodygroup_Down, trailerbodygroup_Up , errormag)

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Result/")
write.table(errorsum, "errorsum.txt", sep="\t", row.names=FALSE)



# class 9
library(graphics)
rm(samples9,matching_errormag,matching_errormag9)

matching_errormag <- errormag[errormag[,6] == 1,]
matching_errormag9 <- matching_errormag[matching_errormag[,3] == 9,]
samples_matching9 <- matching_errormag9 [,7:length(matching_errormag9 [1,])]
samples_matching9 <- na.omit(samples_matching9)


    # fit <- princomp (samples, cor=TRUE)
fit <- kmeans(samples_matching9 , 3) 
matchingsamples9_km  <- data.frame( fit$cluster,  matching_errormag9[,1], matching_errormag9[,2], samples_matching9 )

# matching & nonamtching
rm(all_errormag9, all_samples9, all_sample9_info)
all_errormag9 <- errormag[errormag[,3] == 9,]


all_samples9 <- all_errormag9 [,7:length(all_errormag9 [1,])]
all_samples9 <- cbind(all_errormag9[,1],all_errormag9[,2],all_errormag9[,6], all_errormag9[,3],
                      all_errormag9[,4], all_errormag9[,5],all_samples9)
all_samples9 <- na.omit(all_samples9)
all_samples9_info <- all_samples9[, 1:6]
all_samples9_errordata <- all_samples9[, 7: length(all_samples9[1,])]

fit_all <- kmeans(all_samples9_errordata , 5)
all_samples9_km <- data.frame( fit_all$cluster, all_samples9_info, rowSums(all_samples9[,8:length(all_samples9[1,])]) )

colnames (all_samples9_km) [1] <- c("clusterid")
colnames (all_samples9_km) [2] <- c("bodygroupDown")
colnames (all_samples9_km) [3] <- c("bodygroupUp")
colnames (all_samples9_km) [4] <- c("matchindex")
colnames (all_samples9_km) [5] <- c("FHWAClass")
colnames (all_samples9_km) [6] <- c("Downsigid")
colnames (all_samples9_km) [7] <- c("Upsigid")
colnames (all_samples9_km) [8] <- c("errorsum")


base <- 8
input <- base + 18
Obj <- TargetTable9 [match (all_samples9_km[,6], TargetTable9 [,4]), 6]
Candidate <- TargetTable9 [match ( all_samples9_km[,6], TargetTable9 [,4]), input]


all_samples9_info <- cbind( all_samples9_km, Obj, Candidate)

# plot
xx= 171
sigplot <- f.ErrorComparePlot(xx)


