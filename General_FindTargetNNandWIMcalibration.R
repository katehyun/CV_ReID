# utils:::menuInstallPkgs() 
rm(list=ls())
# load functonbook2
# library(pnn)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
options(scipen=999) 
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/shiftandstretch_06242015.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/a_magdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/a_basemagdif.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/candidate.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/WIM_SIG_pair.RData")


############################################################# do not run when loading RData
### target 1 & 2 :  base and after shift and stretch
# min magdif
min_a_magdif<-vector()
for (i in 1: length(a_magdif)){
  min_a_magdif[i] <- min(a_magdif[[i]])
}

min_a_basemagdif<-vector()
for (i in 1: length(a_basemagdif)){
  min_a_basemagdif[i] <- min(a_basemagdif[[i]])
}


idx_basemagdif <- lapply(a_basemagdif,which.min)
idx_magdif <- lapply(a_magdif,which.min)

base_Upid <-c()
base_Upid_after <-c()

j=1

for (i in 1:length(idx_basemagdif)){
  
  a <- unlist(idx_basemagdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    base_Upid[i] <- Upsiglist[[j]][a]
    j <- j+1
  }
  
}

a_Upid <-c()
base_Upid <-c()
a_Upid_after <-c()

j <- 1
for (i in 1:length(a_magdif)){
  
  a <- unlist(idx_magdif[i])
  b <- unlist(idx_magdif[i])
  
  if (length(Upsiglist[[j]]) == 0 ){
    
    a_Upid[i] <-999
    base_Upid[i] <-999
    j <- j+1}
  
  else {
    
    a_Upid[i] <- Upsiglist[[j]][a]
    base_Upid[i] <- Upsiglist[[j]][b]
    j <- j+1
  }
  
}

# base table
Downtarget <- data.frame()
Downtarget <- Downheader_new$sig_id

FHWAClass <- Downheader_new$FHWAclass

# ix <- seq(from=1, to=64)
# FHWAClass <- FHWAClass[ix]
# Downtarget <- Downtarget[ix]

Target_baseanalysis_table <- data.frame()
Target_baseanalysis_table <- cbind(FHWAClass, min_a_basemagdif, min_a_magdif,  Downtarget, base_Upid, a_Upid )
colnames(Target_baseanalysis_table) <- c("FHWAClass","min_a_basemagdif","min_a_magdif",  "Downtarget", "base_Upid", "a_Upid" )





# wim calibration
# tt
sub_tt <- data.frame()
Downheader_new_tt <- subset(Downheader_new , Downheader_new$FHWAclass >= 8)
Upheader_new_tt <- subset(Upheader_new , Upheader_new$FHWAclass >= 8)
Target_baseanalysis_table_tt <- subset(Target_baseanalysis_table, Target_baseanalysis_table[,1]>=8)

sub_tt <- cbind( Downheader_new_tt[ match(Target_baseanalysis_table_tt[,4], as.numeric(Downheader_new_tt$sig_id)),12] ,
                  Downheader_new_tt[ match(Target_baseanalysis_table_tt[,4], as.numeric(Downheader_new_tt$sig_id)),42] ,
                  Downheader_new_tt[ match(Target_baseanalysis_table_tt[,4], as.numeric(Downheader_new_tt$sig_id)),16:41] ,
                  Upheader_new_tt[ match(Target_baseanalysis_table_tt[,6], as.numeric(Upheader_new_tt$sig_id)),12] ,
                  Upheader_new_tt[ match(Target_baseanalysis_table_tt[,6], as.numeric(Upheader_new_tt$sig_id)),42] ,
                 Upheader_new_tt[ match(Target_baseanalysis_table_tt[,6], as.numeric(Upheader_new_tt$sig_id)),16:41] )

colnames(sub_tt)[1:28] <- c( "length", "gvw",  "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", 
                          "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", "ax5lwt", "ax5rwt", 
                          "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt","ax9lwt", "ax9rwt")
colnames(sub_tt)[29:56] <- c( "length.1", "gvw.1",  "ax12sp.1", "ax23sp.1", "ax34sp.1", "ax45sp.1",  "ax56sp.1", "ax67sp.1", "ax78sp.1",
                               "ax89sp.1", "ax1lwt.1", "ax1rwt.1", "ax2lwt.1", "ax2rwt.1", "ax3lwt.1", "ax3rwt.1", "ax4lwt.1", "ax4rwt.1", 
                               "ax5lwt.1", "ax5rwt.1",  "ax6lwt.1", "ax6rwt.1", "ax7lwt.1", "ax7rwt.1", "ax8lwt.1", "ax8rwt.1","ax9lwt.1", 
                               "ax9rwt.1")


highest20_a_magdif <- sort(min_a_magdif)[1:ceiling(length( min_a_magdif )*0.05)]  # only highest 5%

highest20_wim_idx <- which(Target_baseanalysis_table_tt[,3] < max (highest20_a_magdif)  )
highest20_wim <- sub_tt[highest20_wim_idx,]
highest20_wim_diff <- cbind( (highest20_wim $length - highest20_wim $length.1 ), 
                             (highest20_wim $gvw - highest20_wim $gvw.1 ),  
                             (highest20_wim $ax12sp - highest20_wim $ax12sp.1 ),
                             (highest20_wim $ax23sp - highest20_wim $ax23sp.1 ),
                             (highest20_wim $ax34sp - highest20_wim $ax34sp.1 ),
                             (highest20_wim $ax45sp - highest20_wim $ax45sp.1 ),
                             (highest20_wim $ax56sp - highest20_wim $ax56sp.1 ),
                             (highest20_wim $ax67sp - highest20_wim $ax67sp.1 ),
                             (highest20_wim $ax78sp - highest20_wim $ax78sp.1 ),
                             (highest20_wim $ax89sp - highest20_wim $ax89sp.1 ),
                             (highest20_wim $ax1lwt - highest20_wim $ax1lwt.1 ),
                             (highest20_wim $ax1rwt - highest20_wim $ax1rwt.1 ),
                             (highest20_wim $ax2lwt - highest20_wim $ax2lwt.1 ),
                             (highest20_wim $ax2rwt - highest20_wim $ax2rwt.1 ),
                             (highest20_wim $ax3lwt - highest20_wim $ax3lwt.1 ),
                             (highest20_wim $ax3rwt - highest20_wim $ax3rwt.1 ),
                             (highest20_wim $ax4lwt - highest20_wim $ax4lwt.1 ),
                             (highest20_wim $ax4rwt - highest20_wim $ax4rwt.1 ),
                             (highest20_wim $ax5lwt - highest20_wim $ax5lwt.1 ),
                             (highest20_wim $ax5rwt - highest20_wim $ax5rwt.1 ),
                             (highest20_wim $ax6lwt - highest20_wim $ax6lwt.1 ),
                             (highest20_wim $ax6rwt - highest20_wim $ax6rwt.1 ),
                             (highest20_wim $ax7lwt - highest20_wim $ax7lwt.1 ),
                             (highest20_wim $ax7rwt - highest20_wim $ax7rwt.1 ),
                             (highest20_wim $ax8lwt - highest20_wim $ax8lwt.1 ),
                             (highest20_wim $ax8rwt - highest20_wim $ax8rwt.1 ),
                             (highest20_wim $ax9lwt - highest20_wim $ax9lwt.1 ),
                             (highest20_wim $ax9rwt - highest20_wim $ax9rwt.1 )                                
)

highest20_wim_diff_median_tt <- data.frame()
highest20_wim_diff_median_tt <- t( apply(highest20_wim_diff, 2, FUN = median)  )
highest20_wim_diff_median_tt <- round(highest20_wim_diff_median_tt, digits = 1)
colnames(highest20_wim_diff_median_tt)[1:28] <- c( "length", "gvw", 
                     "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", 
                      "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                      "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                     "ax9lwt", "ax9rwt")


# su
sub_su <- data.frame()
Downheader_new_su <- subset(Downheader_new , Downheader_new$FHWAclass >= 8)
Upheader_new_su <- subset(Upheader_new , Upheader_new$FHWAclass >= 8)
Target_baseanalysis_table_su <- subset(Target_baseanalysis_table, Target_baseanalysis_table[,1]>=8)

sub_su <- cbind( Downheader_new_su[ match(Target_baseanalysis_table_su[,4], as.numeric(Downheader_new_su$sig_id)),12] ,
                 Downheader_new_su[ match(Target_baseanalysis_table_su[,4], as.numeric(Downheader_new_su$sig_id)),42] ,
                 Downheader_new_su[ match(Target_baseanalysis_table_su[,4], as.numeric(Downheader_new_su$sig_id)),16:41] ,
                 Upheader_new_su[ match(Target_baseanalysis_table_su[,6], as.numeric(Upheader_new_su$sig_id)),12] ,
                 Upheader_new_su[ match(Target_baseanalysis_table_su[,6], as.numeric(Upheader_new_su$sig_id)),42] ,
                 Upheader_new_su[ match(Target_baseanalysis_table_su[,6], as.numeric(Upheader_new_su$sig_id)),16:41] )

colnames(sub_su)[1:28] <- c( "length", "gvw",  "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", 
                             "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", "ax5lwt", "ax5rwt", 
                             "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt","ax9lwt", "ax9rwt")
colnames(sub_su)[29:56] <- c( "length.1", "gvw.1",  "ax12sp.1", "ax23sp.1", "ax34sp.1", "ax45sp.1",  "ax56sp.1", "ax67sp.1", "ax78sp.1",
                              "ax89sp.1", "ax1lwt.1", "ax1rwt.1", "ax2lwt.1", "ax2rwt.1", "ax3lwt.1", "ax3rwt.1", "ax4lwt.1", "ax4rwt.1", 
                              "ax5lwt.1", "ax5rwt.1",  "ax6lwt.1", "ax6rwt.1", "ax7lwt.1", "ax7rwt.1", "ax8lwt.1", "ax8rwt.1","ax9lwt.1", 
                              "ax9rwt.1")


highest20_a_magdif <- sort(min_a_magdif)[1:ceiling(length( min_a_magdif )*0.05)]  # only highest 5%

highest20_wim_idx <- which(Target_baseanalysis_table_su[,3] < max (highest20_a_magdif)  )
highest20_wim <- sub_su[highest20_wim_idx,]
highest20_wim_diff <- cbind( (highest20_wim $length - highest20_wim $length.1 ), 
                             (highest20_wim $gvw - highest20_wim $gvw.1 ),  
                             (highest20_wim $ax12sp - highest20_wim $ax12sp.1 ),
                             (highest20_wim $ax23sp - highest20_wim $ax23sp.1 ),
                             (highest20_wim $ax34sp - highest20_wim $ax34sp.1 ),
                             (highest20_wim $ax45sp - highest20_wim $ax45sp.1 ),
                             (highest20_wim $ax56sp - highest20_wim $ax56sp.1 ),
                             (highest20_wim $ax67sp - highest20_wim $ax67sp.1 ),
                             (highest20_wim $ax78sp - highest20_wim $ax78sp.1 ),
                             (highest20_wim $ax89sp - highest20_wim $ax89sp.1 ),
                             (highest20_wim $ax1lwt - highest20_wim $ax1lwt.1 ),
                             (highest20_wim $ax1rwt - highest20_wim $ax1rwt.1 ),
                             (highest20_wim $ax2lwt - highest20_wim $ax2lwt.1 ),
                             (highest20_wim $ax2rwt - highest20_wim $ax2rwt.1 ),
                             (highest20_wim $ax3lwt - highest20_wim $ax3lwt.1 ),
                             (highest20_wim $ax3rwt - highest20_wim $ax3rwt.1 ),
                             (highest20_wim $ax4lwt - highest20_wim $ax4lwt.1 ),
                             (highest20_wim $ax4rwt - highest20_wim $ax4rwt.1 ),
                             (highest20_wim $ax5lwt - highest20_wim $ax5lwt.1 ),
                             (highest20_wim $ax5rwt - highest20_wim $ax5rwt.1 ),
                             (highest20_wim $ax6lwt - highest20_wim $ax6lwt.1 ),
                             (highest20_wim $ax6rwt - highest20_wim $ax6rwt.1 ),
                             (highest20_wim $ax7lwt - highest20_wim $ax7lwt.1 ),
                             (highest20_wim $ax7rwt - highest20_wim $ax7rwt.1 ),
                             (highest20_wim $ax8lwt - highest20_wim $ax8lwt.1 ),
                             (highest20_wim $ax8rwt - highest20_wim $ax8rwt.1 ),
                             (highest20_wim $ax9lwt - highest20_wim $ax9lwt.1 ),
                             (highest20_wim $ax9rwt - highest20_wim $ax9rwt.1 )                                
)

highest20_wim_diff_median_su <- data.frame()
highest20_wim_diff_median_su <- t( apply(highest20_wim_diff, 2, FUN = median)  )
highest20_wim_diff_median_su <- round(highest20_wim_diff_median_su, digits = 1)
colnames(highest20_wim_diff_median_su)[1:28] <- c( "length", "gvw", 
                                                   "ax12sp", "ax23sp", "ax34sp", "ax45sp",  "ax56sp", "ax67sp", "ax78sp", "ax89sp", 
                                                   "ax1lwt", "ax1rwt", "ax2lwt", "ax2rwt", "ax3lwt", "ax3rwt", "ax4lwt", "ax4rwt", 
                                                   "ax5lwt", "ax5rwt", "ax6lwt", "ax6rwt", "ax7lwt", "ax7rwt", "ax8lwt", "ax8rwt",
                                                   "ax9lwt", "ax9rwt")


# calibrate wim (Up)
Upheader_new_cl <-  Upheader_new
colnames(Upheader_new_cl)[12] = c( "length")
colnames(Upheader_new_cl)[16:42] = c( "axsp12" , "axsp23" , "axsp34" , "axsp45",  "axsp56",  "axsp67",  "axsp78",  "axsp89", 
                               "axwt1r" , "axwt2r" , "axwt3r" ,"axwt4r" , "axwt5r", "axwt6r" , "axwt7r" , "axwt8r", "axwt9r",
                               "axwt1l" , "axwt2l" , "axwt3l" ,"axwt4l" , "axwt5l", "axwt6l" , "axwt7l", "axwt8l" , "axwt9l",
                               "gvw")
colnames(Upheader_new_cl)[43] = c( "FHWAclass")

for (i in 1: length( Upheader_new[,1]) ){
  
    if ( Upheader_new_cl$FHWAclass[i] < 8) {
      Upheader_new_cl[i,12] <- Upheader_new_cl[i,12] + highest20_wim_diff_median_su[1,1]
      Upheader_new_cl[i,16:41] <- Upheader_new_cl [i,16:42] + highest20_wim_diff_median_su[1,3:28]
      Upheader_new_cl[i,42] <- Upheader_new_cl [i,43] + highest20_wim_diff_median_su[1,2]
    }
    else{
      Upheader_new_cl[i,12] <- Upheader_new_cl[i,12] + highest20_wim_diff_median_su[1,1]
      Upheader_new_cl[i,16:41] <- Upheader_new_cl [i,16:42] + highest20_wim_diff_median_su[1,3:28]
      Upheader_new_cl[i,42] <- Upheader_new_cl [i,43] + highest20_wim_diff_median_su[1,2]
    }
}


Upheader_new <- Upheader_new_cl

save(Upheader_new,  file= "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upheader_new.RData"  )
save(Target_baseanalysis_table, file= "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Target_baseanalysis_table.RData")
save(FHWAClass,file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/FHWAClass.RData" )
