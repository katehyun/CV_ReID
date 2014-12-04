### function



# FUNCTION - normalization
f.normalization <- function (insig){
  outsig <- transform (insig, newtime=insig[,1] / insig[nrow(insig),1],
                       newsig= insig[,2] / (max(insig[,2]))) 
  outsig[1,4]=0
  outsig[1,5]=0
  return (outsig)
}

# FUNCTION - round
f.round <- function (insig, no_round){
  outsig <- insig * no_round
  outsig = round (outsig)
  outsig = outsig / no_round
}


# FUNCTION - spline interpolation
f.interpolation <- function (insig, num, no_round){ 
  outsig0 <- spline(insig[,4], insig[,5], num)
  outsig1 = data.frame(matrix(unlist(outsig0[1])))
  
  outsig2 <- f.round (outsig1, no_round)
  outtime = outsig2
  
  outmag = data.frame(matrix(unlist(outsig0[2])))
  outsig <- cbind(outtime, outmag)
  return (outsig)
}

f.interpolation_v2 <- function (insig, num, no_round){ 
  outsig0 <- spline(insig[,1], insig[,2], num)
  outsig1 = data.frame(matrix(unlist(outsig0[1])))
  
  outsig2 <- f.round (outsig1, no_round)
  outtime = outsig2
  
  outmag = data.frame(matrix(unlist(outsig0[2])))
  outsig <- cbind(outtime, outmag)
  return (outsig)
}


# 
# # FUNCTION - swift
# f.swift <- function ( insig, splineDown, swift_coeff, num , no_round){
#   
#   
#   Up_swift_mag <- rep(NA, num)
#   Up_swift_time <- rep(NA, num)
#   Up_swift_out <- data.frame (Up_swift_time, Up_swift_mag)
#   swiftmag <- rep(NA, num)
#   swifttime<- rep(NA, num)
#   swift_magdif <- rep(NA,1)
#   
#   time <- seq(from= 0, to= 1, by = 1/(num-1))
#   time <-  f.round(time, no_round)
# 
#   
#   for (i in 1: length(swift_coeff)){
#     
#     
#     Up_swift_time <- time + swift_coeff[i]
#     Up_swift_time <- f.round(Up_swift_time, no_round) # added
#     
#     Up_swift_mag <- insig
# 
#    
#     
#     # LOOKUP!!!!! 
#    
#     swifttime <- cbind( Up_swift_time, swifttime)
#     
#     swift_tempmag <- Up_swift_mag [match (Up_swift_time, time )]
#     swift_tempmag2 <- approx ( Up_swift_time, swift_tempmag, time_feat)
#     swift_tempmag2$y[is.na(swift_tempmag2$y)] <- 0
#     
# #     swift_tempmag <- Up_swift_mag [match ( time_feat ,Up_swift_time)] # modified
# #     swift_tempmag[is.na(swift_tempmag)] <- swift_tempmag # Up_swift_mag
#     
#     splineDown <- splineDown [match (  time_feat, time)] # added
#     swift_magdif2 = abs(splineDown - swift_tempmag2$y )
#     swift_magdif3 = sum( swift_magdif2)
#     
#     swift_magdif <- cbind(swift_magdif3, swift_magdif) 
#     
#     min_swiftmagdif <- which.min (swift_magdif )
#     minvalue <- swift_magdif[min_swiftmagdif]
#     
# #     Up_swift <- cbind(swifttime[,min_swiftmagdif], Up_swift_mag)
#     Up_swift <- cbind(time_feat, swift_tempmag2$y)
#     
#   }
#   return (list(matrix=Up_swift, mv=minvalue))
# }
# 
# 
# # FUNCTION - stret
# f.stret <- function (insig, splineDown, stret_coeff, num, no_round ){
#   
#   time <- seq(from= 0, to= 1, by = 1/(num-1))
#   time <-  f.round(time, no_round)
#  
#   
#   Up_stret_mag <- rep(NA, num)
#   Up_stret_time <- rep(NA, num)
#   
#   stretmag <- rep(NA, num)
#   strettime<- rep(NA, num)
#   stret_magdif <- rep(NA,1)
#   
#   for (i in 1: length(stret_coeff)){
#     
#     
#     Up_stret_time <- insig[,1]* stret_coeff[i]
#     Up_stret_time <- f.round(Up_stret_time, no_round)
#     Up_stret_mag <- insig[,2]
#     
#     Up_stret_sig <- cbind(Up_stret_time, Up_stret_mag)
#     Up_stret_sig  <- approx (  Up_stret_time, Up_stret_mag , n=1000)
#     
#     Up_stret_time <- Up_stret_sig$x 
#     Up_stret_time <- f.round(Up_stret_time, no_round)
#     Up_stret_mag <- Up_stret_sig$y 
#  
#  
#     # LOOKUP!!!!! 
#     
#     stretmag <- cbind(Up_stret_mag, stretmag)
#     strettime <- cbind(Up_stret_time, strettime)
#     
# #     stret_tempmag <- Up_stret_mag [match (Up_stret_time, time )]
#     stret_tempmag <- Up_stret_mag [match ( Up_stret_time,time )]
#     stret_tempmag2 <- approx ( Up_stret_time, stret_tempmag, time_feat)
#     stret_tempmag2$y[is.na(stret_tempmag2$y)] <- Up_stret_mag [match ( Up_stret_time,time )]
# 
# #     stret_tempmag[is.na(stret_tempmag)] <- stret_tempmag # Up_stret_mag
#     
#     splineDown <- splineDown [match (  time_feat, time)] # added
#     stret_magdif2 = abs(splineDown - stret_tempmag2$y ) 
#     stret_magdif3= sum(stret_magdif2)
#     
#     stret_magdif <- cbind(stret_magdif3, stret_magdif) 
#     
#     min_stretmagdif <- which.min (stret_magdif)
#     minvalue <- stret_magdif[min_stretmagdif]
#     
# #     Up_stret <- cbind(strettime[,min_stretmagdif], Up_stret_mag)
#     Up_stret <- cbind(time_feat,  stret_tempmag2$y)
#     
#     
#   }
#   return (list(matrix=Up_stret, mv=minvalue) )
#   
# }


# # FUNCTION - swift
# f.swift <- function ( insig, splineDown, swift_coeff, num , no_round){
#   
#   Up_swift_mag <- rep(NA, num)
#   Up_swift_time <- rep(NA, num)
#   Up_swift_out <- data.frame (Up_swift_time, Up_swift_mag)
#   swiftmag <- rep(NA, num)
#   swifttime<- rep(NA, num)
#   swift_magdif <- rep(NA,1)
#   tempswift <- rep(NA, num)
#  
#   
#   for (i in 1: length(swift_coeff)){
#     
#    insig <-  splineUp
#     
#     Up_swift_time <- time + swift_coeff[i]
#     Up_swift_time <- f.round(Up_swift_time, no_round) # added
#     Up_swift_mag <- insig
#    
#     # LOOKUP!!!!! 
#     
# #     swifttime <- cbind( Up_swift_time, swifttime)
#     
#     swift_tempmag <- Up_swift_mag [match ( time ,Up_swift_time )]
#     swift_tempmag <- na.omit(swift_tempmag)
#     tempswift  <-  approx ( swift_tempmag, n=num)
#     swift_tempmag <-tempswift[[2]] 
#    
# #     swift_tempmag[is.na(swift_tempmag)] <- Up_swift_mag
#     
#     swift_magdif2 <- sum ( abs(splineDown - swift_tempmag ) )
#     swift_magdif <- cbind(swift_magdif2, swift_magdif) 
#     
# #     Up_swift <- cbind(swifttime[,min_swiftmagdif], Up_swift_mag)
#     Up_swift <- cbind(swift_tempmag)
#     
#   }
#     min_swiftmagdif <- which.min (swift_magdif )
#     minvalue <- swift_magdif[min_swiftmagdif]
# 
#   return (list(matrix=Up_swift, mv=minvalue))
# }
# 
# 
# # FUNCTION - stret
# f.stret <- function (insig, splineDown, stret_coeff, num, no_round ){
#   
#   
#   Up_stret_mag <- rep(NA, num)
#   Up_stret_time <- rep(NA, num)
#   
#   stretmag <- rep(NA, num)
#   strettime<- rep(NA, num)
#   stret_magdif <- rep(NA,1)
#   tempstret <- rep(NA, num)
#   stret_tempmag <- rep(NA, num)
#   
#   for (i in 1: length(stret_coeff)){
#     
#     
#     Up_stret_time <- time * stret_coeff[i]
#     Up_stret_time <- f.round(Up_stret_time, no_round)
#     
#     
#     
#     # LOOKUP!!!!! 
#     
# #     stretmag <- cbind(Up_stret_mag, stretmag)
# #     strettime <- cbind(Up_stret_time, strettime)
#     
#     stret_tempmag <- insig [match (time, Up_stret_time )]
#     stret_tempmag <- na.omit(stret_tempmag)
#     tempstret  <-  approx ( stret_tempmag,  n=num)
# 
#     stret_tempmag <- tempstret [[2]] 
# #     stret_tempmag[is.na(stret_tempmag)] <- Up_stret_mag
#     
#     # added
# #       splineDown2 <- approx (splineDown, n=feat_num)
# #       stret_tempmag2 <- approx(stret_tempmag, n=feat_num)
#      
#     ## end
#     
#     stret_magdif2 <- sum( abs(splineDown - stret_tempmag ) )
# #     stret_magdif2 = abs(splineDown2$y - stret_tempmag2$y ) 
# #     stret_magdif3= sum(stret_magdif2)
#     
#     stret_magdif <- cbind(stret_magdif2, stret_magdif) 
#     
#    
#     
# #     Up_stret <- cbind(strettime[,min_stretmagdif], Up_stret_mag)
#     Up_stret <- cbind( stret_tempmag)
#     
#     
#   }
# 
#   min_stretmagdif <- which.min (stret_magdif)
#   minvalue <- stret_magdif[min_stretmagdif]
#   return (list(matrix=Up_stret, mv=minvalue) )
#   
# }
# 

# FUNCTION - swift
f.swift <- function ( insig, splineDown, swift_coeff, num , no_round){
  
  Up_swift <- rep(NA, num)
  swift_tempmag <- rep(NA, num)
  swift_magdif <- rep(NA,1)
  swiftmagdif2 <- rep(NA,1)
  min_swiftmagdif <- rep(NA,1)
  minvalue <- rep(NA,1)

  
  for (i in 1: length(swift_coeff)){
    
    
    swift_tempmag<- insig + swift_coeff[i]
#     Up_swift_time <- f.round(Up_swift_time, no_round) # added
#     Up_swift_mag <- insig
    
    # LOOKUP!!!!! 
    
    
    
#     swift_tempmag <- Up_swift_mag [match ( time ,Up_swift_time )]
#     swift_tempmag <- na.omit(swift_tempmag)
#     tempswift  <-  approx ( swift_tempmag, n=num)
#     swift_tempmag <-tempswift[[2]] 
    
  
    
    swift_magdif2 <- sum ( abs(splineDown - swift_tempmag ) )
    swift_magdif <- cbind(swift_magdif2, swift_magdif) 
    
   
    Up_swift <- cbind(swift_tempmag, Up_swift)
    
  }
  min_swiftmagdif <- which.min (swift_magdif )
  minvalue <- swift_magdif[min_swiftmagdif]
  
  return (list(matrix=Up_swift[,min_swiftmagdif], mv=minvalue))
}


# FUNCTION - stret
f.stret <- function (insig, splineDown, stret_coeff, num, no_round ){
  
  
  Up_stret <- rep(NA, num)
  stret_tempmag  <- rep(NA, num)
  stret_magdif2 <- rep(NA, 1)
  stret_magdif <- rep(NA,1)
  min_stretmagdif <- rep(NA,1) 
  minvalue <- rep(NA,1) 
  
  for (i in 1: length(stret_coeff)){
    
    
    stret_tempmag <- insig * stret_coeff[i]
#     Up_stret_time <- f.round(Up_stret_time, no_round)
    
    
    
    # LOOKUP!!!!! 
   
#     
#     stret_tempmag <- insig [match (time, Up_stret_time )]
#     stret_tempmag <- na.omit(stret_tempmag)
#     tempstret  <-  approx ( stret_tempmag,  n=num)
#     
#     stret_tempmag <- tempstret [[2]] 
   
   
    
    stret_magdif2 <- sum( abs(splineDown - stret_tempmag ) )
    #     stret_magdif2 = abs(splineDown2$y - stret_tempmag2$y ) 
    #     stret_magdif3= sum(stret_magdif2)
    
    stret_magdif <- cbind(stret_magdif2, stret_magdif) 
    
    
    
    #     Up_stret <- cbind(strettime[,min_stretmagdif], Up_stret_mag)
    Up_stret <- cbind(stret_tempmag)
    
    
  }
  
  min_stretmagdif <- which.min (stret_magdif)
  minvalue <- stret_magdif[min_stretmagdif]
  return (list(matrix=Up_stret, mv=minvalue) )
  
}


f.ResultNN <- function (threshold_NN,  TargetTable, p ){

a_magdif <- as.numeric(TargetTable[,3]  )
Target_obj <- TargetTable[,5]
Target_obj2 <- TargetTable[,6]
Result_NNtemp <- data.frame()

for (j in 1: length(threshold_NN)) {

  
  
  for (i in 1:length( TargetTable[,1])){
    
    if (a_magdif[i] < threshold_NN[j]){
      a_Upid_after[i] <- as.numeric(TargetTable[i,8])
    }
    else {
      a_Upid_after[i] <- as.numeric(c(999))
    }
    
  }
  
  TargetTable <- cbind ( TargetTable, a_Upid_after )
  
  missing_obj <- length (Target_obj[is.na(Target_obj)]) 
  matching_obj <- length (Target_obj[!is.na(Target_obj)]) 
  
  matching_NN <-table( Target_obj == a_Upid_after)["TRUE"]
  missing_NN <- table(a_Upid_after == c(999))["TRUE"]
  
  
  CMVeh <-  matching_NN[1]
  CVeh <- matching_obj[1]
  MVeh <- sum(   (as.numeric( TargetTable[,p])) > 1000 )  
  
  
  SIMR <- CMVeh / CVeh
  SCMR <- CMVeh / MVeh
  
  MMVeh <- length(  subset(TargetTable[,1], as.numeric( Target_obj2 ) 
                           !=  as.numeric( TargetTable[,p])   ))
  
  p <- p+1
  Veh <- length(TargetTable[,1])
  SER <- MMVeh / Veh
  
  Result <- data.frame(threshold_NN[j], matching_obj[1], missing_obj[1],              
                       matching_NN[[1]],  missing_NN[[1]],
                       CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
  
  Result_NNtemp <- rbind(Result_NNtemp, Result)
}

return (list(resultnn =Result_NNtemp, tt = TargetTable))
}



f.ResultPNN <- function (threshold_PNN,  TargetTable, p ){
  
  pnn_prob <- as.numeric(TargetTable[,2]  )
  Target_obj <- TargetTable[,4]
  Target_obj2 <- TargetTable[,5]
  Result_PNNtemp <- data.frame()
  
  for (j in 1: length(threshold_PNN)) {
    
    
    
    for (i in 1:length( TargetTable[,1])){
      
      if (max_pnn_prob[i] > threshold_PNN[j]){
        pnn_Upid_after[i] <- as.numeric(TargetTable[i,6])
      }
      else {
        pnn_Upid_after[i] <- as.numeric(c(999))
      }
      
    }
    
    TargetTable <- cbind ( TargetTable, pnn_Upid_after )
    
    missing_obj <- length (Target_obj[is.na(Target_obj)]) 
    matching_obj <- length (Target_obj[!is.na(Target_obj)]) 
    
    matching_NN <-table( Target_obj == pnn_Upid_after)["TRUE"]
    missing_NN <- table(pnn_Upid_after == c(999))["TRUE"]
    
    
    CMVeh <-  matching_NN[1]
    CVeh <- matching_obj[1]
    MVeh <- sum(   (as.numeric( TargetTable[,p])) > 1000 )  
    
    
    SIMR <- CMVeh / CVeh
    SCMR <- CMVeh / MVeh
    
    MMVeh <- length(  subset(TargetTable[,1], as.numeric( Target_obj2 ) 
                             !=  as.numeric( TargetTable[,p])   ))
    
    p <- p+1
    Veh <- length(TargetTable[,1])
    SER <- MMVeh / Veh
    
    Result <- data.frame(threshold_PNN[j], matching_obj[1], missing_obj[1],              
                         matching_NN[[1]],  missing_NN[[1]],
                         CMVeh[[1]], CVeh[[1]], MVeh[[1]], SIMR[[1]], SCMR[[1]], MMVeh[[1]], Veh[[1]], SER[[1]] )
    
    Result_PNNtemp <- rbind(Result_PNNtemp, Result)
  }
  
  return (list(resultpnn =Result_PNNtemp, tt = TargetTable))
}





f.ErrorMag <- function( Upsigid, Downsigid){
  
  time <- seq(from= 0, to= 1, by = 1/(num-1))
  insig_time <- time
  
  Downinsig <- match(Downsigid, Downheader_new$sigid)
  Upinsig <- match(Upsigid, Upheader_new$sigid)
  insig_mag <- abs ( Downobjout[Downinsig,] - Upobjout[Upinsig,] )
  
  errormag <- c(Downsigid, Upsigid,999, insig_mag)
  return (errormag)
}


### draw signature
f.drawDownsignature <- function (no){
 
  time <- seq(from= 0, to= 1, by = 1/(num-1))
  insig_time <- time
  insig_mag <- Downobjout[no,]
  
  
  sigplot <- matplot(insig_time, insig_mag, main=paste("Target (Downstream)", no[1]))
  
  return (sigplot)
}





f.drawUpsignature <- function (n1,n2){
  
    sig_idx <- as.numeric( Upsiglist[[n1]][n2] )
    insig <- match(sig_idx, Upheader_new$sigid)

  
  
    time <- seq(from= 0, to= 1, by = 1/(num-1))
    insig_time <- time
    insig_mag <- Upobjout[insig,]
  
    
    sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Upstream)", n1[1],"-", n2[1]))
    
  return (sigplot)
}


f.Updraw <- function (sigid){
  
  insig <- match(sigid, Upheader_new$sigid)
  
  time <- seq(from= 0, to= 1, by = 1/(num-1))
  insig_time <- time
  insig_mag <- Upobjout[insig,]
    
  sigplot <- plot(insig_time, insig_mag, main=paste("Target (Upstream)", sigid[1]))
  return (sigplot)
}
  


f.Downdraw <- function (sigid){
  
  insig <- match(sigid, Downheader_new$sigid)
  
  
  time <- seq(from= 0, to= 1, by = 1/(num-1))
  insig_time <- time
  insig_mag <- Downobjout[insig,]
  
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (Downstream)", sigid[1]))
  
  return (sigplot)
}


f.UpdrawAfterSS <- function (n1, n2){
  

  
  if (n1==1) {
    insig_mag <- candi_1[[n2]][[1]][,2]
    insig_time <- candi_1[[n2]][[1]][,1]
  }
  
  if (n1==2) {
    insig_mag <- candi_2[[n2]][[1]][,2]
    insig_time <- candi_2[[n2]][[1]][,1]
  }
  
  if (n1==3) {
    insig_mag <- candi_3[[n2]][[1]][,2]
    insig_time <- candi_3[[n2]][[1]][,1]
  }

  
  
  sigplot <- plot(insig_time, insig_mag, main=paste("Candidate (AfterSS_Upstream)", n2[1],"-", n1[1]))
  return (sigplot)
}

f.ErrorDraw <- function ( Upsigid, Downsigid ) {
  
  
    Downinsig <- match(Downsigid, Downheader_new$sigid)
    Upinsig <- match(Upsigid, Upheader_new$sigid)
    
    
    time <- seq(from= 0, to= 1, by = 1/(num-1))
    insig_time <- time
    
    insig_mag <- abs ( Downobjout[Downinsig,] - Upobjout[Upinsig,] )
    
    
    sigplot <- plot(insig_time, insig_mag, main=paste("Error", Downsigid[1] , Upsigid[1]))
    
    return (sigplot)
  
}

f.ErrorComparePlot <- function (xx){
  
  time <- seq(from= 0, to= 1, by = 1/(num-1))
  time <- t(time)
  mag <- all_samples9_errordata [xx,]
  sigplot <- plot(time, mag, type = "p",  pch = NULL, main=(paste(xx)))
  return (sigplot)
}

  
f.pnn <- function ( nn, Downobjout1){
  
  cat <- list()
  prob <- list()
  category <- list()
  probs <- list()
  
  candi_guess <- list(guess(nn, Downobjout1)) 
  cat <- candi_guess[[1]][1]
  prob <- candi_guess[[1]][2]
  
  
  return (list(category <- cat , probs <- prob))
}


f.findpnn <- function (seqlevel, len_find_pnn, candi1, candi2, candi3, candi4, candi5, candi6, candi7, candi8 ){
  
  Upobjout <- c()
  
  if (len_find_pnn == 2) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)    
  }
  
  
  if (len_find_pnn == 3) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 4) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 5) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  if (len_find_pnn == 6) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  
  if (len_find_pnn == 7) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi7)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
  }
  
  
  if (len_find_pnn >= 8) {
    Upobj <- unlist(candi1)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)
    
    Upobj <- unlist(candi2)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi3)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi4)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi5)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi6)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj)  
    
    Upobj <- unlist(candi7)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj) 
    
    Upobj <- unlist(candi8)
    Upobj <-  Upobj[1001:2000]
    Upobj <- Upobj[seq(1, length(Upobj),seqlevel)]  
    Upobjout <- rbind(Upobjout, Upobj) 
  }
  
  return (Upobjout)
  
}