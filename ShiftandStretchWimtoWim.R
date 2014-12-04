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

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData") # 1000 features
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData") # 1000 features

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout_50f.RData") #(dont use)
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout_50f.RData") #(dont use)


### Input ready 
num=1000
no_round = 1000



time <- seq(from= 0, to= 1, by = 1/(num-1))
time <- f.round(time, no_round) # added


base_magdif <- c()
magdif <- c()
magdif2 <- c()
a_magdif <- list()
ss<-list()

Up_stret <-list()
Up_shift <-list()


candi_magdif <- list()
a_basemagdif <- list()

candidate <- list()


swift_coeff = seq (-0.10, 0.10, by=0.001)
stret_coeff = seq ( 0.80, 1.20, by=0.001)

# swift_coeff = seq (-0.20, 0.20, by=0.01)
# stret_coeff = seq ( 0.80, 1.20, by=0.1)

Upheader_ID <- Upheader_new$sigid
Downheader_ID <- Downheader_new$sigid


#Down

 for (w in 1:length(Downheader_ID)){

  splineDown <- Downobjout [w,]


    if (length(Upsiglist[[w]]) < 1 ) { 
      
      a_magdif[w] <- list(c(99999))
      a_basemagdif[w] <- list(c(99999))    

    }


    else {
  
        
        #Up 
        
        magdif2 <- c()
        base_magdif <- c()
        
         for (q in 1: length(Upsiglist[[w]])){
  
            
            min_stretmagdif <- c()
            splineUpidx <- match (Upsiglist[[w]][q] , Upheader_ID)
            splineUp <- Upobjout[splineUpidx,]
            


            base_magdif[q] = sum( abs (splineDown - splineUp ))
      
            
            # first iteration
            swift <- f.swift ( splineUp , splineDown, swift_coeff, num , no_round )
            stret <- f.stret (swift$matrix, splineDown , stret_coeff, num , no_round)
            
            # start iteration
            
            min_stretmagdif = stret$mv
            min_swiftmagdif = swift$mv
            
            if ((min_stretmagdif - min_swiftmagdif ) < 1 ){
              
              Up_stret <- stret$matrix
            }
            
            else {
              
              while (abs (min_swiftmagdif - min_stretmagdif) > 1) {
                
               
                swift <- f.swift (stret$matrix, splineDown, swift_coeff, num , no_round)
                stret <- f.stret (swift$matrix, splineDown, stret_coeff, num , no_round)
                
                Up_swift <- swift$matrix
                min_swiftmagdif <- swift$mv
                Up_stret <- stret$matrix
                min_stretmagdif <- stret$mv
                
              } 
            }
            
            magdif2[q] <- (c(min_stretmagdif))  
            
            ss[q] <- list(Up_stret)
            
       
          
        }

        a_magdif[w] <- list(magdif2)
        a_basemagdif[w] <- list(base_magdif)
        candidate[w] <- list(ss)
        ss <- list()
      }

}


save(candidate, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate.RData")
save(a_magdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_magdif.RData")
save(a_basemagdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/a_basemagdif.RData")

#write.table(candi_1[[1]], "./ProcessedData/TestCode/candi1.txt", sep="\t")

#save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/shiftandstretch_Jan0910.RData")

##############################################################end


