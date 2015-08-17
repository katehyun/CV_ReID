
rm(list=ls())
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/07132015.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/07162015_set1_v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/07162015_set2_v2.RData")
#### loading functionbook
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/DownsigID.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/UpsigID.RData")

load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Upobjout.RData") 
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/Downobjout.RData") 



### Input ready 
num=50
nummat = 1:num
element = 1
no_round = 100



time <- seq(from= 0, to= 1, by = 1/(num-1))
time <- f.round(time, no_round) # added



# swift_coeff = seq (-0.10, 0.10, by=0.01)
swift_coeff = seq (-0.10, 0.10, by=0.1)
stret_coeff = seq ( 0.90, 1.10, by=0.1)




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
sigtemp <- list()
sigfeature <- list()
aa <- list()



Upheader_ID <- Upheader_new$sig_id
Downheader_ID <- Downheader_new$sig_id


#Down

for (w in 1:length(Downheader_ID)){
#     for (w in 1:100){
  

  UpObj <- match (Upheader_ID,  UpsigID) # check!
  
  splineDown <- Downobjout [w, ] 

  
  
  
  if (length(Upsiglist[[ w  ]]) < 1 ) { 
    
    a_magdif[w] <- list(c(99999))
    a_basemagdif[w] <- list(c(99999))  
    candidate[w] <- list(aa)
    sigfeature[w] <- list(aa)
    
    
    
  }
  
  
  else {
    
    
    #Up 
    
    magdif2 <- c()
    base_magdif <- c()
    
    for (q in 1: length(Upsiglist[[  w  ]])  ){
      
      
      min_stretmagdif <- c()
      splineUpidx <- match (Upsiglist[[ w  ]][q] , Upheader_ID)
      splineUp <- Upobjout[splineUpidx,]
      splineUp <- spline(nummat, splineUp, length(time)) $y
      
      
      base_magdif[q] = sum( abs (splineDown - splineUp ))
      
      
      # first iteration
      swift_h <- f.swift_horizontal  ( splineUp , splineDown, swift_coeff, length(time) , no_round )
 
      stret_h <- f.stret_horizontal (swift_h$matrix, splineDown , stret_coeff, length(time) , no_round)
   
      
      # start iteration
      
      
      min_swiftmagdif = swift_h$mv
      min_stretmagdif = stret_h$mv
      
      if (abs ( min_swiftmagdif - min_stretmagdif ) < 20 ){
        
        stret <- stret_h$matrix
    
      }
      
      else {
        
        while (abs(min_swiftmagdif - min_stretmagdif)  >= 20) {
          
          
          swift_h <- f.swift_horizontal (stret_h$matrix, splineDown, swift_coeff, length(time) , no_round)
  
          stret_h <- f.stret_horizontal (swift_h$matrix, splineDown , stret_coeff, length(time) , no_round)

          
          swift <- swift_h $matrix
          min_swiftmagdif <- swift_h $mv
          stret <- stret_h$matrix
          min_stretmagdif <- stret_h$mv
          

          
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


save(candidate, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/candidate_set2v2.RData")
save(a_magdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_magdif_set2v2.RData")
save(a_basemagdif, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/a_basemagdif_set2v2.RData")
save(sigfeature, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/sigfeature_set2v2.RData")


save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/shiftandstretch_07162015_set2v2.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/shiftandstretch_07162015_set2v2.RData")
##############################################################end
par(new=TRUE)
