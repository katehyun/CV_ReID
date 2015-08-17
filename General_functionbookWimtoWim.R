### function



# FUNCTION - normalization
f.normalization <- function (insig){
  maxsig <- max(insig)
  outsig <- insig/maxsig
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
  len <- length(insig)
  outtime <- seq(from = 0, to = 1, length=c(len) )
  
  outsig0 <- spline(outtime, insig, num)
  outsig1 = data.frame(matrix(unlist(outsig0[1])))
  
  outsig2 <- f.round (outsig1, no_round)
  outtime <- outsig2
  
  outmag = data.frame(matrix(unlist(outsig0[2])))
  outsig <- cbind(outtime, outmag)
  return (outsig)
}

f.interpolationbytime <- function (insig, num, no_round){ 
  
  outsig <- spline(time, insig, num)
  outsig <- f.round (outsig$y, no_round)
  return (outsig)
}


# FUNCTION - swift
f.swift_horizontal <- function ( insig, splineDown, swift_coeff, n , no_round){
  

  Up_swift <-matrix(,nrow = length(time), ncol = length(swift_coeff))
  swift_magdif <-matrix(,nrow = 1, ncol = length(swift_coeff))
  swift_tempmag <- rep(NA, n)

  swiftmagdif2 <- rep(NA,1)
  min_swiftmagdif <- rep(NA,1)
  minvalue <- rep(NA,1)
  time <- f.round(time, no_round)
  
  for (i in 1: length(swift_coeff)){
    
    # horizontal
    swift_temptime <- time + swift_coeff[i] 
    swift_temptime <- f.round(swift_temptime, no_round)
    swift_temptime <- f.interpolationbytime (swift_temptime, n, no_round)
    
    swift_tempmag_init <- insig
    tempmat <- cbind( swift_temptime, swift_tempmag_init )
    swift_tempmag <- approx( swift_temptime,   swift_tempmag_init, xout = time)$y
    

    swift_tempmag [is.na( swift_tempmag )] <- 0
  
    
    swift_magdif[,i] <- sum ( abs( splineDown - swift_tempmag ) )
 
    
    Up_swift[,i] <- t(swift_tempmag)
    
  }
  min_swiftmagdif <- which.min (swift_magdif )
  minvalue <- swift_magdif[min_swiftmagdif]
  
  return (list(matrix=Up_swift[,min_swiftmagdif], mv=minvalue))
}



# FUNCTION - stret
f.stret_horizontal <- function (insig, splineDown, stret_coeff,n, no_round ){
  
  Up_stret <-matrix(,nrow = length(time), ncol = length(stret_coeff))
  stret_tempmag  <- rep(NA, n)
  stret_magdif2 <- rep(NA, 1)
  stret_magdif <- matrix(,nrow = 1, ncol = length(stret_coeff))
  min_stretmagdif <- rep(NA,1) 
  minvalue <- rep(NA,1) 
  time <- f.round(time, no_round)
  
  for (i in 1: length(stret_coeff)){
    
    timer <- f.round(time, no_round)
    stret_temptime <-  timer * stret_coeff[i]

    
    
    stret_tempmag_init <- insig
    tempmat <- cbind( stret_temptime, stret_tempmag_init)
    
    stret_tempmag <- approx(stret_temptime,  stret_tempmag_init, xout = time)$y
    


    stret_tempmag [is.na (stret_tempmag)] <- 0

    stret_magdif[,i] <- sum( abs(splineDown - stret_tempmag ) )
    
    
  
    Up_stret[,i] <- stret_tempmag
    
    
  }
  
  min_stretmagdif <- which.min (stret_magdif)
  minvalue <- stret_magdif[min_stretmagdif]
  return (list(matrix=Up_stret[,min_stretmagdif], mv=minvalue) )
  
}


# 
# # FUNCTION - stret
# f.stret_vertical <- function (insig, splineDown, stret_coeff, num, no_round ){
#   
# #   insig <- matrix
#   Up_stret <- rep(NA, num)
#   stret_tempmag  <- rep(NA, num)
#   stret_magdif2 <- rep(NA, 1)
#   stret_magdif <- rep(NA,1)
#   min_stretmagdif <- rep(NA,1) 
#   minvalue <- rep(NA,1) 
#   
#   for (i in 1: length(stret_coeff)){
#     
#     
#     stret_tempmag <- insig * stret_coeff[i]
#     #     Up_stret_time <- f.round(Up_stret_time, no_round)
#     stret_tempmag <- f.signorm(stret_tempmag)
#     
#     
#     # LOOKUP!!!!! 
#     
#     #     
#     #     stret_tempmag <- insig [match (time, Up_stret_time )]
#     #     stret_tempmag <- na.omit(stret_tempmag)
#     #     tempstret  <-  approx ( stret_tempmag,  n=num)
#     #     
#     #     stret_tempmag <- tempstret [[2]] 
#     
#     
#     
#     stret_magdif2 <- sum( abs(splineDown - stret_tempmag ) )
#     #     stret_magdif2 = abs(splineDown2$y - stret_tempmag2$y ) 
#     #     stret_magdif3= sum(stret_magdif2)
#     
#     stret_magdif <- cbind(stret_magdif2, stret_magdif) 
#     
#     
#     
#     #     Up_stret <- cbind(strettime[,min_stretmagdif], Up_stret_mag)
#     Up_stret <- cbind(stret_tempmag,  Up_stret )
#     
#     
#   }
#   
#   min_stretmagdif <- which.min (stret_magdif)
#   minvalue <- stret_magdif[min_stretmagdif]
#   return (list(matrix=Up_stret[,min_stretmagdif], mv=minvalue) )
#   
# }





