#utils:::menuInstallPkgs() 
#library(gtools)
#library(plyr)
library(zoo)
rm(list=ls())

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/08082014Jan0910.RData") # 1000 features
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/09252014Jan0910.RData")# 50 features (dont use)

#setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
#setwd("C:/Users/Kyung Hyun/Dropbox/Kate/ReID/TruckReid") 
#### loading functionbook2
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/DownsigID.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/UpsigID.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData") # 1000 features
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData") # 1000 features

# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout_50f.RData") #(dont use)
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout_50f.RData") #(dont use)


### Input ready 
num=50
nummat = 1:num
element = 1
no_round = 100



time <- seq(from= 0, to= 1, by = 1/(num-1))
time <- f.round(time, no_round) # added



swift_coeff = seq (-0.10, 0.10, by=0.01)
stret_coeff = seq ( 0.80, 1.20, by=0.1)




base_magdif <- c()
magdif <- c()
magdif2 <- c()
a_magdif <- list()
ss<-list()

swift <- list()
stret <- list()

candi_magdif <- list()
a_basemagdif <- list()

candidate <- list()




# swift_coeff = seq (-0.20, 0.20, by=0.01)
# stret_coeff = seq ( 0.80, 1.20, by=0.1)

Upheader_ID <- Upheader_new$sigid
Downheader_ID <- Downheader_new$sigid
aa <- list()

#Down

for (w in 1:length(Downheader_ID)){
#   for (w in 1:5){
  
  DownObj <- match (Downheader_ID,  DownsigID) # check!
  UpObj <- match (Upheader_ID,  UpsigID) # check!
  
  splineDown <- Downobjout [DownObj[w], ] 
#   splineDown <- spline(nummat, splineDown, length(time)) $y
 
  
  
  if (length(Upsiglist[[ DownObj[w] ]]) < 1 ) { 
    
    a_magdif[w] <- list(c(99999))
    a_basemagdif[w] <- list(c(99999))  
    candidate[w] <- list(aa)
    sigfeature[w] <- list(aa)
  
    
    
  }
  
  
  else {
    
    
    #Up 
    
    magdif2 <- c()
    base_magdif <- c()
    
    for (q in 1: length(Upsiglist[[  DownObj[w]  ]])  ){
      
      
      min_stretmagdif <- c()
      splineUpidx <- match (Upsiglist[[ DownObj[w]  ]][q] , Upheader_ID)
      splineUp <- Upobjout[splineUpidx,]
      splineUp <- spline(nummat, splineUp, length(time)) $y
      
      
      base_magdif[q] = sum( abs (splineDown - splineUp ))
      
      
      # first iteration
      swift_h <- f.swift_horizontal  ( splineUp , splineDown, swift_coeff, length(time) , no_round )
      #             swift_v <- f.swift_vertical  ( swift_h$matrix , splineDown, swift_coeff, num , no_round )
      stret_h <- f.stret_horizontal (swift_h$matrix, splineDown , stret_coeff, length(time) , no_round)
      #             stret_v <- f.stret_vertical  ( stret_h$matrix, splineDown , stret_coeff, num , no_round)
      
      # start iteration
      
      
      min_swiftmagdif = swift_h$mv
      min_stretmagdif = stret_h$mv
      
      if (abs ( min_swiftmagdif - min_stretmagdif ) < 2 ){
        
        stret <- stret_h$matrix
        #               Up_stret <- f.signorm(Up_stret)
      }
      
      else {
        
        while (abs(min_swiftmagdif - min_stretmagdif)  >= 2) {
          
          
          swift_h <- f.swift_horizontal (stret_h$matrix, splineDown, swift_coeff, length(time) , no_round)
          #                 swift_v <- f.swift_vertical  ( swift_h$matrix , splineDown, swift_coeff, num , no_round )
          stret_h <- f.stret_horizontal (swift_h$matrix, splineDown , stret_coeff, length(time) , no_round)
          #                 stret_v <- f.stret_vertical  ( stret_h$matrix, splineDown , stret_coeff, num , no_round)
          
          swift <- swift_h $matrix
          min_swiftmagdif <- swift_h $mv
          stret <- stret_h$matrix
          min_stretmagdif <- stret_h$mv
          
          #                 Up_stret <- f.signorm(Up_stret)
          
        } 
      }
      
      magdif2[q] <- (c(min_stretmagdif))  
      
      ss[q] <- list(stret)
      sigtemp[q] <- list( unlist(splineDown ) - unlist(ss[q]))
      
      
      
    }
    
    a_magdif[w] <- list(magdif2)
    a_basemagdif[w] <- list(base_magdif)
    candidate[w] <- list(ss)
    sigfeature[w] <- list(sigtemp)
    sigtemp <- list()
    ss <- list()
  }
  
}


save(candidate, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_04272015.RData")
save(a_magdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif_04272015.RData")
save(a_basemagdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif_04272015.RData")
save(sigfeature, file=" C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/sigfeature_04272015.RData")
#write.table(candi_1[[1]], "./ProcessedData/TestCode/candi1.txt", sep="\t")

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/shiftandstretch_Jan0910_04272015.RData")

##############################################################end
par(new=TRUE)
