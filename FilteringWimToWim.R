#install.packages("stringr")
rm(list=ls())
rm(Upheader_new, Downheader_new)
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 

load("./ProcessedData/Jan0910/SOLCLoadinJan0910")
# load functionbook2

### input file - Jan 0910

Upheader = SO.Jan0910Header
a <- length(Upheader[1,]) 


for (j in 1: 31){
Upheader[,a+j] <- SOJan_v1[,5+j][match( Upheader$sigid, SOJan_v1[,3])] # numaxle
}

#   
# Upheader[,a]   <- SOJan_v1[,b  ][match( Upheader$sigid, SOJan_v1[,3])] # FHWA class
# Upheader[,a+1] <- SOJan_v1[,b+1][match( Upheader$sigid, SOJan_v1[,3])] # numaxle
# Upheader[,a+2] <- SOJan_v1[,b+2][match( Upheader$sigid, SOJan_v1[,3])] # utc
# Upheader[,a+3] <- SOJan_v1[,b+3][match( Upheader$sigid, SOJan_v1[,3])] # length
# Upheader[,a+4] <- SOJan_v1[,b+4][match( Upheader$sigid, SOJan_v1[,3])] # gvw
# 
# Upheader[,a+5] <- SOJan_v1[,b+5][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 1-2
# Upheader[,a+6] <- SOJan_v1[,b+6][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 2-3
# Upheader[,a+7] <- SOJan_v1[,b+7][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 3-4
# Upheader[,a+8] <- SOJan_v1[,b+8][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 4-5
# Upheader[,a+9] <- SOJan_v1[,b+9][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 5-6
# Upheader[,a+10] <- SOJan_v1[,b+10][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 6-7
# Upheader[,a+11] <- SOJan_v1[,b+11][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 7-8
# Upheader[,a+12] <- SOJan_v1[,b+12][match( Upheader$sigid, SOJan_v1[,3])] # axle spacing 8-9
# 
# Upheader[,a+13] <- SOJan_v1[,14][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 1 left
# Upheader[,a+14] <- SOJan_v1[,15][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 1 right
# Upheader[,a+15] <- SOJan_v1[,16][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 2 left
# Upheader[,a+16] <- SOJan_v1[,17][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 2 right
# Upheader[,a+17] <- SOJan_v1[,18][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 3 left
# Upheader[,a+18] <- SOJan_v1[,19][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 3 right
# Upheader[,a+19] <- SOJan_v1[,20][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 4 left
# Upheader[,a+20] <- SOJan_v1[,21][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 4 right
# Upheader[,a+21] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 5 left
# Upheader[,a+22] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 5 right
# Upheader[,a+23] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 6 left
# Upheader[,a+24] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 6 right
# Upheader[,a+25] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 7 left
# Upheader[,a+26] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 7 right
# Upheader[,a+27] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 8 left
# Upheader[,a+28] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 8 right
# Upheader[,a+29] <- SOJan_v1[,22][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 9 left
# Upheader[,a+30] <- SOJan_v1[,23][match( Upheader$sigid, SOJan_v1[,3])] # axle weight 9 right

# up cleaning
idx <- vector()
Upheader_new <-subset(Upheader, Upheader[,8] > 100) # samplecount
Upheader_new <-subset(Upheader_new, Upheader_new[,14] > 3)
Upheader_new <-subset(Upheader_new, Upheader_new[,14] < 15)



for (i in 1: length(Upheader_new[,1])  ){
  if (Upheader_new[i,15] == 2 ) {
      idx[i] <- rowSums(Upheader_new [i, c(20:26) ] )
      idx[i] <- idx[i] + rowSums(Upheader_new [i, c(31:44) ] ) 
  }  
  else if (Upheader_new[i,15] == 3 ) {
    idx[i] <- rowSums(Upheader_new [i, c(21:26) ] )
    idx[i] <- idx[i] + rowSums(Upheader_new [i, c(33:44) ] )   
  }
  else if (Upheader_new[i,15] == 4 ) {
    idx[i] <- rowSums(Upheader_new [i, c(22:26) ] )
    idx[i] <- idx[i] + rowSums(Upheader_new [i, c(35:44) ] )       
  }
 else if (Upheader_new[i,15] == 5 ) {
    idx[i] <- rowSums(Upheader_new [i, c(23:26) ] )
    idx[i] <- idx[i] + rowSums(Upheader_new [i, c(37:44) ] )      
  }
 else if (Upheader_new[i,15] == 6 ) {
   idx[i] <- rowSums(Upheader_new [i, c(24:26) ] )
   idx[i] <- idx[i] + rowSums(Upheader_new [i, c(39:44) ] )      
 }
 else if (Upheader_new[i,15] == 7 ) {
   idx[i] <- rowSums(Upheader_new [i, c(25:26) ] )
   idx[i] <- idx[i] + rowSums(Upheader_new [i, c(41:44) ] )      
 }
 else if (Upheader_new[i,15] == 8 ) {
   idx[i] <- rowSums(Upheader_new [i, c(26) ] )
   idx[i] <- idx[i] + rowSums(Upheader_new [i, c(43:44) ] )      
 }
}

idxUp <- idx
whichidxUp <- which(idxUp > 0)
if ( length(whichidxUp ) > 0 ) { 
  Upheader_new <- Upheder_new[-whichidxUp,]
}

# non-normalized
Upheader_new_nonN <-  Upheader_new
save(Upheader_new_nonN, file="./ProcessedData/Jan0910/Upheader_new_nonN.RData")
# normalized
for (i in 1:length(Upheader_new[,1])){
  sp <- Upheader_new[i,20] 
  wt <- Upheader_new[i,27] 
  
  if (Upheader_new[i,14] == 9) {
      for ( j in 1:8){
        
        Upheader_new[i,j+18] <- Upheader_new[i,j+18] / sp
      }
      
      for ( k in 1:18){
        Upheader_new[i,k+26] <- Upheader_new[i,k+26] / wt
      }
      Upheader_new[i, 17] <- Upheader_new[i,17] / sp
      Upheader_new[i, 18] <- Upheader_new[i,18] / wt
  }
  
  
}





# Down
rm(Downheader)
Downheader = LC.Jan0910Header

a <- length(Downheader[1,])

for (j in 1: 31){
  Downheader[,a+j] <- LCJan_v1[,5+j][match( Downheader$sigid, LCJan_v1[,3])] # numaxle
}

# down clean
idx <- vector()
Downheader_new <-subset(Downheader, Downheader[,8] > 100) # samplecount
Downheader_new <-subset(Downheader, Downheader[,14] > 3)
Downheader_new <-subset(Downheader_new, Downheader_new[,14] < 15)



for (i in 1: length(Downheader_new[,1])  ){
  if (Downheader_new[i,15] == 2 ) {
    idx[i] <- rowSums(Downheader_new [i, c(20:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(31:44) ] ) 
  }  
  else if (Downheader_new[i,15] == 3 ) {
    idx[i] <- rowSums(Downheader_new [i, c(21:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(33:44) ] )   
  }
  else if (Downheader_new[i,15] == 4 ) {
    idx[i] <- rowSums(Downheader_new [i, c(22:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(35:44) ] )       
  }
  else if (Downheader_new[i,15] == 5 ) {
    idx[i] <- rowSums(Downheader_new [i, c(23:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(37:44) ] )      
  }
  else if (Downheader_new[i,15] == 6 ) {
    idx[i] <- rowSums(Downheader_new [i, c(24:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(39:44) ] )      
  }
  else if (Downheader_new[i,15] == 7 ) {
    idx[i] <- rowSums(Downheader_new [i, c(25:26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(41:44) ] )      
  }
  else if (Downheader_new[i,15] == 8 ) {
    idx[i] <- rowSums(Downheader_new [i, c(26) ] )
    idx[i] <- idx[i] + rowSums(Downheader_new [i, c(43:44) ] )      
  }
}
idxDown <- idx
whichidxDown <- which(idxDown > 0)
if ( length(whichidxDown) > 0 ) { 
  Downheader_new <- Downheder_new[-whichidxDown,]
}

# non-normalized
Downheader_new_nonN <- Downheader_new
save(Downheader_new_nonN, file="./ProcessedData/Jan0910/Downheader_new_nonN.RData")

# normalized
for (i in 1:length(Downheader_new[,1])){
  sp <- Downheader_new[i,20] 
  wt <- Downheader_new[i,27] 
  if (Downheader_new[i,14] == 9) {
      
      for ( j in 1:8){
        
        Downheader_new[i,j+18] <- Downheader_new[i,j+18] / sp
      }
      
      for ( k in 1:18){
        Downheader_new[i,k+26] <- Downheader_new[i,k+26] / wt
      }
      Downheader_new[i, 17] <- Downheader_new[i,17] / sp
      Downheader_new[i, 18] <-  Downheader_new[i,18] / wt
  }
  
}



# Downheader[,14] <- LCJan_v1[,6][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,15] <- LCJan_v1[,8][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,16] <- LCJan_v1[,9][match( Downheader$sigid, LCJan_v1[,3])]
# 
# Downheader[,17] <- LCJan_v1[,10][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,18] <- LCJan_v1[,11][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,19] <- LCJan_v1[,12][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,20] <- LCJan_v1[,13][match( Downheader$sigid, LCJan_v1[,3])]
# 
# Downheader[,21] <- LCJan_v1[,14][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,22] <- LCJan_v1[,15][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,23] <- LCJan_v1[,16][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,24] <- LCJan_v1[,17][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,25] <- LCJan_v1[,18][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,26] <- LCJan_v1[,19][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,27] <- LCJan_v1[,20][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,28] <- LCJan_v1[,21][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,29] <- LCJan_v1[,22][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,30] <- LCJan_v1[,23][match( Downheader$sigid, LCJan_v1[,3])]
# Downheader[,31] <- LCJan_v1[,24][match( Downheader$sigid, LCJan_v1[,3])]





# look traffic condition
buffertimewindow=60; # min (WIM-WIM case)
bufferduration = 0.4; # 0.2 min
buffernumpnt = 800
bufferlen = 12
bufferaspacing12 = 8
bufferaspacing23 = 5
bufferaspacing34 = 5
bufferaspacing45 = 5
bufferaspacing56 = 5
bufferaspacing67 = 5
bufferaspacing78 = 5
bufferaspacing89 = 5
buffergvw = 40

bufferaweightl1 = 4
bufferaweightr1 = 4
bufferaweightl2 = 4
bufferaweightr2 = 4
bufferaweightl3 = 4
bufferaweightr3 = 4
bufferaweightl4 = 4
bufferaweightr4 = 4
bufferaweightl5 = 4
bufferaweightr5 = 4
bufferaweightl6 = 4
bufferaweightr6 = 4
bufferaweightl7 = 4
bufferaweightr7 = 4
bufferaweightl8 = 4
bufferaweightr8 = 4
bufferaweightl9 = 4
bufferaweightr9 = 4

# set buffer
Downheader_ID=(Downheader_new$sigid)

settime <- matrix(nrow=length(Downheader_ID), ncol=1)
setduration<- matrix(nrow=length(Downheader_ID), ncol=1)
setnumpnt<- matrix(nrow=length(Downheader_ID), ncol=1)
setlen<- matrix(nrow=length(Downheader_ID), ncol=1)
setgvw<- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing12 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing23 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing34 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing45 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing56 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing67 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing78 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaspacing89 <- matrix(nrow=length(Downheader_ID), ncol=1)

setaweightl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr5 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl6 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr6 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl7 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr7 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl8 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr8 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightl9 <- matrix(nrow=length(Downheader_ID), ncol=1)
setaweightr9 <- matrix(nrow=length(Downheader_ID), ncol=1)


lb <- matrix(nrow=length(Downheader_ID), ncol=1)
ld <- matrix(nrow=length(Downheader_ID), ncol=1)
ud <- matrix(nrow=length(Downheader_ID), ncol=1)
lp <- matrix(nrow=length(Downheader_ID), ncol=1)
up <- matrix(nrow=length(Downheader_ID), ncol=1)
ul <- matrix(nrow=length(Downheader_ID), ncol=1)
ll <- matrix(nrow=length(Downheader_ID), ncol=1)
ug <- matrix(nrow=length(Downheader_ID), ncol=1)
lg <- matrix(nrow=length(Downheader_ID), ncol=1)

la12 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua12 <- matrix(nrow=length(Downheader_ID), ncol=1)
la23 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua23 <- matrix(nrow=length(Downheader_ID), ncol=1)
la34 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua34 <- matrix(nrow=length(Downheader_ID), ncol=1)
la45 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua45 <- matrix(nrow=length(Downheader_ID), ncol=1)
la56 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua56 <- matrix(nrow=length(Downheader_ID), ncol=1)
la67 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua67 <- matrix(nrow=length(Downheader_ID), ncol=1)
la78 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua78 <- matrix(nrow=length(Downheader_ID), ncol=1)
la89 <- matrix(nrow=length(Downheader_ID), ncol=1)
ua89 <- matrix(nrow=length(Downheader_ID), ncol=1)

lwl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl1 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr1 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl2 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr2 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl3 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr3 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl4 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr4 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl5 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr5 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr5 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl6 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl6 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr6 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr6 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl7 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl7 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr7 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr7 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl8 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl8 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr8 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr8 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwl9 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwl9 <- matrix(nrow=length(Downheader_ID), ncol=1)
lwr9 <- matrix(nrow=length(Downheader_ID), ncol=1)
uwr9 <- matrix(nrow=length(Downheader_ID), ncol=1)



for (j in 1: length(Downheader_ID)){
  settime[j] <- as.numeric(Downheader_new[j,12])
  lb[j] <- settime[j] - buffertimewindow * 60000  
}

for (j in 1: length(Downheader_ID)){
  setduration[j] <- as.numeric(Downheader_new[j,7])
  ld[j] <- setduration[j] - bufferduration  
  ud[j] <- setduration[j] + bufferduration  
}

for (j in 1: length(Downheader_ID)){
  setnumpnt[j] <- as.numeric(Downheader_new[j,8])
  lp[j] <- setnumpnt[j] - buffernumpnt  
  up[j] <- setnumpnt[j] + buffernumpnt  
}

for (j in 1: length(Downheader_ID)){
  setlen[j] <- as.numeric(Downheader_new[j,17])
  ll[j] <- setlen[j] - bufferlen  
  ul[j] <- setlen[j] + bufferlen  
}

for (j in 1: length(Downheader_ID)){
  setgvw[j] <- as.numeric(Downheader_new[j,18])
  lg[j] <- setgvw[j] - buffergvw  
  ug[j] <- setgvw[j] + buffergvw 
}

for (j in 1: length(Downheader_ID)){
  setaspacing12[j] <- as.numeric(Downheader_new[j,19])
  setaspacing23[j] <- as.numeric(Downheader_new[j,20])
  setaspacing34[j] <- as.numeric(Downheader_new[j,21])
  setaspacing45[j] <- as.numeric(Downheader_new[j,22])
  setaspacing56[j] <- as.numeric(Downheader_new[j,23])
  setaspacing67[j] <- as.numeric(Downheader_new[j,24])
  setaspacing78[j] <- as.numeric(Downheader_new[j,25])
  setaspacing89[j] <- as.numeric(Downheader_new[j,26])
  la12[j] <- setaspacing12[j] - bufferaspacing12  
  ua12[j] <- setaspacing12[j] + bufferaspacing12
  la23[j] <- setaspacing23[j] - bufferaspacing23  
  ua23[j] <- setaspacing23[j] + bufferaspacing23 
  la34[j] <- setaspacing34[j] - bufferaspacing34 
  ua34[j] <- setaspacing34[j] + bufferaspacing34  
  la45[j] <- setaspacing45[j] - bufferaspacing45  
  ua45[j] <- setaspacing45[j] + bufferaspacing45  
  la56[j] <- setaspacing56[j] - bufferaspacing56  
  ua56[j] <- setaspacing56[j] + bufferaspacing56 
  la67[j] <- setaspacing67[j] - bufferaspacing67  
  ua67[j] <- setaspacing67[j] + bufferaspacing67 
  la78[j] <- setaspacing78[j] - bufferaspacing78  
  ua78[j] <- setaspacing78[j] + bufferaspacing78 
  la89[j] <- setaspacing89[j] - bufferaspacing89  
  ua89[j] <- setaspacing89[j] + bufferaspacing89 
}

for (j in 1: length(Downheader_ID)){
  setaweightl1[j] <- as.numeric(Downheader_new[j,27])
  setaweightr1[j] <- as.numeric(Downheader_new[j,28])
  setaweightl2[j] <- as.numeric(Downheader_new[j,29])
  setaweightr2[j] <- as.numeric(Downheader_new[j,30])
  setaweightl3[j] <- as.numeric(Downheader_new[j,31])
  setaweightr3[j] <- as.numeric(Downheader_new[j,32])
  setaweightl4[j] <- as.numeric(Downheader_new[j,33])
  setaweightr4[j] <- as.numeric(Downheader_new[j,34])
  setaweightl5[j] <- as.numeric(Downheader_new[j,35])
  setaweightr5[j] <- as.numeric(Downheader_new[j,36])
  setaweightl6[j] <- as.numeric(Downheader_new[j,37])
  setaweightr6[j] <- as.numeric(Downheader_new[j,38])
  setaweightl7[j] <- as.numeric(Downheader_new[j,39])
  setaweightr7[j] <- as.numeric(Downheader_new[j,40])
  setaweightl8[j] <- as.numeric(Downheader_new[j,41])
  setaweightr8[j] <- as.numeric(Downheader_new[j,42])
  setaweightl9[j] <- as.numeric(Downheader_new[j,43])
  setaweightr9[j] <- as.numeric(Downheader_new[j,44])
  
  lwl1[j] <- setaweightl1[j] - bufferaweightl1 
  uwl1[j] <- setaweightl1[j] + bufferaweightl1
  lwl2[j] <- setaweightl2[j] - bufferaweightl2 
  uwl2[j] <- setaweightl2[j] + bufferaweightl2
  lwl3[j] <- setaweightl3[j] - bufferaweightl3 
  uwl3[j] <- setaweightl3[j] + bufferaweightl3
  lwl4[j] <- setaweightl4[j] - bufferaweightl4 
  uwl4[j] <- setaweightl4[j] + bufferaweightl4
  lwl5[j] <- setaweightl5[j] - bufferaweightl5 
  uwl5[j] <- setaweightl5[j] + bufferaweightl5
  lwl6[j] <- setaweightl6[j] - bufferaweightl6 
  uwl6[j] <- setaweightl6[j] + bufferaweightl6
  lwl7[j] <- setaweightl7[j] - bufferaweightl7 
  uwl7[j] <- setaweightl7[j] + bufferaweightl7
  lwl8[j] <- setaweightl8[j] - bufferaweightl8 
  uwl8[j] <- setaweightl8[j] + bufferaweightl8
  lwl9[j] <- setaweightl9[j] - bufferaweightl9 
  uwl9[j] <- setaweightl9[j] + bufferaweightl9
  
  lwr1[j] <- setaweightr1[j] - bufferaweightr1 
  uwr1[j] <- setaweightr1[j] + bufferaweightr1
  lwr2[j] <- setaweightr2[j] - bufferaweightr2 
  uwr2[j] <- setaweightr2[j] + bufferaweightr2
  lwr3[j] <- setaweightr3[j] - bufferaweightr3 
  uwr3[j] <- setaweightr3[j] + bufferaweightr3
  lwr4[j] <- setaweightr4[j] - bufferaweightr4 
  uwr4[j] <- setaweightr4[j] + bufferaweightr4
  lwr5[j] <- setaweightr5[j] - bufferaweightr5 
  uwr5[j] <- setaweightr5[j] + bufferaweightr5
  lwr6[j] <- setaweightr6[j] - bufferaweightr6 
  uwr6[j] <- setaweightr6[j] + bufferaweightr6
  lwr7[j] <- setaweightr7[j] - bufferaweightr7 
  uwr7[j] <- setaweightr7[j] + bufferaweightr7
  lwr8[j] <- setaweightr8[j] - bufferaweightr8 
  uwr8[j] <- setaweightr8[j] + bufferaweightr8
  lwr9[j] <- setaweightr9[j] - bufferaweightr9 
  uwr9[j] <- setaweightr9[j] + bufferaweightr9
 
}


### time window - TIME & DURATION 
Upsiglist <- list()

for (j in 1: length(Downheader_ID)){ 
  
  Upsiglist[j] <- list(subset(Upheader_new$sigid,  Upheader_new$utc > lb[j] &  Upheader_new$utc <= settime[j]
                              & Upheader_new[,7] > ld[j] & Upheader_new[,7] < ud[j]
                              & Upheader_new[,8] > lp[j] & Upheader_new[,8] < up[j]
                              & Upheader_new[,14] == Downheader_new[j,14] 
                              
                              & Upheader_new[,17] > ll[j] & Upheader_new[,17] < ul[j]
                              & Upheader_new[,18] > lg[j] & Upheader_new[,18] < ug[j]
                              
                              & Upheader_new[,19] > la12[j] & Upheader_new[,19] < ua12[j]
                              & Upheader_new[,20] > la23[j] & Upheader_new[,20] < ua23[j]
                              & Upheader_new[,21] > la34[j] & Upheader_new[,21] < ua34[j]
                              & Upheader_new[,22] > la45[j] & Upheader_new[,22] < ua45[j]
                              & Upheader_new[,23] > la45[j] & Upheader_new[,23] < ua56[j]
                              & Upheader_new[,24] > la45[j] & Upheader_new[,24] < ua67[j]
                              & Upheader_new[,25] > la45[j] & Upheader_new[,25] < ua78[j]
                              & Upheader_new[,26] > la45[j] & Upheader_new[,26] < ua89[j]
                              
                              & Upheader_new[,27] > lwl1[j] & Upheader_new[,27] < uwl1[j]
                              & Upheader_new[,28] > lwr1[j] & Upheader_new[,28] < uwr1[j]
                              & Upheader_new[,29] > lwl2[j] & Upheader_new[,29] < uwl2[j]
                              & Upheader_new[,30] > lwr2[j] & Upheader_new[,30] < uwr2[j]
                              & Upheader_new[,31] > lwl3[j] & Upheader_new[,31] < uwl3[j]
                              & Upheader_new[,32] > lwr3[j] & Upheader_new[,32] < uwr3[j]
                              & Upheader_new[,33] > lwl4[j] & Upheader_new[,33] < uwl4[j]
                              & Upheader_new[,34] > lwr4[j] & Upheader_new[,34] < uwr4[j]
                              & Upheader_new[,35] > lwl5[j] & Upheader_new[,35] < uwl5[j]
                              & Upheader_new[,36] > lwr5[j] & Upheader_new[,36] < uwr5[j]
                              & Upheader_new[,37] > lwl6[j] & Upheader_new[,37] < uwl6[j]
                              & Upheader_new[,38] > lwr6[j] & Upheader_new[,38] < uwr6[j]
                              & Upheader_new[,39] > lwl7[j] & Upheader_new[,39] < uwl7[j]
                              & Upheader_new[,40] > lwr7[j] & Upheader_new[,40] < uwr7[j]
                              & Upheader_new[,41] > lwl8[j] & Upheader_new[,41] < uwl8[j]
                              & Upheader_new[,42] > lwr8[j] & Upheader_new[,42] < uwr8[j]
                              & Upheader_new[,43] > lwl9[j] & Upheader_new[,43] < uwl9[j]
                              & Upheader_new[,44] > lwr9[j] & Upheader_new[,44] < uwr9[j]
  
                              ))
}

### input files - DOWN

num <- 1000
no_round <- 1000

# find index for potential matching
Downsig = LC.Jan0910sig
Downsig_IM=subset(Downsig, select=c(id,mag,sigid)) 

Downidx <- match ( (Downheader_ID),Downsig_IM[,3] )
Downidx <- Downidx[!is.na(Downidx)]

Downindex <- c()
Downobjout <- c()
DownsigID <- vector()
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData") # 1000 features
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/DownsigID.RData") # 1000 features

w <- length(DownsigID)

for (i in 1: length(Downidx)){
# for (i in 1:4){
 
   Downindex <- Downidx[i]
        
    if (!Downsig_IM[Downindex,3] %in% DownsigID) {                                    
      
        w <- w+1
      
        DownsigID[w] <- Downsig_IM[Downindex,3]
        inDownsig <- Downsig_IM[Downindex+1,]
        Downindex <- Downindex+1
        
        while (Downsig_IM[Downindex+1,1] < 100){
          inDownsig <- rbind(inDownsig,Downsig_IM[Downindex+1,])
          Downindex <- Downindex+1
        }
        
        inDownsig <- f.normalization(inDownsig)
        splineDown <- f.interpolation(inDownsig,num,no_round)
        colnames(splineDown) <- c("outDowntime", "outDownmag")
        #write.table(inDownsig, "./ProcessedData/TestCode/inDownsig.txt", sep="\t")
        #write.table(splineDown, "./ProcessedData/TestCode/splineDown.txt", sep="\t")
        
        Downobj <- c(splineDown[,2])
        Downobj <- t(Downobj)
        Downobjout <-rbind(Downobjout,  Downobj)
  }
}

### input files - UP


Upsig = SO.Jan0910sig # Mar 20
Upsig_IM=subset(Upsig, select=c(id,mag,sigid))

Upidx <- match ( Upheader_new$sigid, Upsig_IM$sigid ) 

UpheaderID <-Upsig_IM[Upidx,3]
Upindex <- c()
Upobjout <- c()
UpsigID <- vector()
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData") # 1000 features
# load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/UpsigID.RData") # 1000 features


 w <- length(UpsigID)

for (i in 1: length(Upidx)){
# for (w in 1: 20){
  
  Upindex <- Upidx[i]

  
  if (!Upsig_IM[Upindex,3] %in% UpsigID) { 
    
    w <- w+1
    
    UpsigID[w] <- Upsig_IM[Upindex,3]
    inUpsig <- Upsig_IM[Upindex+1,]
    Upindex <- Upindex+1
      
      while (Upsig_IM[Upindex+1,1] < 100){
        inUpsig <- rbind(inUpsig,Upsig_IM[Upindex+1,])
        Upindex <- Upindex+1
      }
      
      inUpsig <- f.normalization(inUpsig)
      splineUp <- f.interpolation(inUpsig,num,no_round)
      colnames(splineUp) <- c("outUptime", "outUpmag")
      
      
      Upobj <- c(splineUp[,2])
      Upobj <- t(Upobj)
      Upobjout <-rbind(Upobjout,  Upobj)
  }
}

save(Upheader_new, file="./ProcessedData/Jan0910/Upheader_new.RData")
save(Downheader_new, file="./ProcessedData/Jan0910/Downheader_new.RData")
save(Upsiglist, file="./ProcessedData/Jan0910/Upsiglist.RData")
save(UpsigID, file="./ProcessedData/Jan0910/UpsigID.RData")
save(DownsigID, file="./ProcessedData/Jan0910/DownsigID.RData")

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/12152014Jan0910.RData")  # for Jan 0910
#0925 : 50 features
#0808 : 1000 features

save(Upobjout, file="./ProcessedData/Jan0910/Upobjout.RData")
save(Downobjout, file="./ProcessedData/Jan0910/Downobjout.RData")
save(matching, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/matching.RData")
#####################################################################end
# remove files

rm(la12, la23, la34, la45, lb, ld, lg, ll, lp, lwl1, lwl2, lwl3, lwl4, lwl5, lwr1, lwr2, lwr3, lwr4, lwr5,
   ua12, ua23, ua34, ua45, ud, ug, ul, up, uw1l, uw1r, uw2l, uw2r, uw3l, uw3r, uw4l, uw4r, uw5l, uw5r, 
   uwl1, uwl2, uwl3, uwl4, uwl5, uwr1, uwr2, uwr3, uwr4, uwr5)
rm (a, b, la56, la67, la78, la89, lwl6, lwl7, lwl8, lwl9, lwr6, lwr7, lwr8, lwr9,
    ua56, ua67, ua78, ua89, uwl5, uwl6, uwl7, uwl8, uwl9, uwr6, uwr7, uwr8, uwr9, 
    setaspacing56, setaspacing67, setaspacing78, setaspacing89,
    setaweightl6,  setaweightl7,  setaweightl8,  setaweightl9,
    setaweightr6,  setaweightr7,  setaweightr8,  setaweightr9)

rm(setaspacing12, setaspacing23, setaspacing34, setaspacing45, setaweightl1, setaweightl2, setaweightl3,
   setaweightl4, setaweightl5, setaweightr1, setaweightr2, setaweightr3, setaweightr4, setaweightr5,
   setduration, setgvw, setnumpnt, settime, setlen, uctJan09, uctJan10)

rm(LC.Jan09ML3Header1, LC.Jan09ML3Header2, LC.Jan09ML3sig1, LC.Jan09ML3sig2, 
   LC.Jan09ML4Header1, LC.Jan09ML4Header2, LC.Jan09ML4sig1, LC.Jan09ML4sig2, 
   LC.Jan10ML3Header1, LC.Jan10ML3Header2, LC.Jan10ML3Header3, LC.Jan10ML3Header4,
   LC.Jan10ML3sig1, LC.Jan10ML3sig2, LC.Jan10ML3sig3, LC.Jan10ML3sig4,
   LC.Jan10ML4Header1, LC.Jan10ML4Header2, LC.Jan10ML4Header3, LC.Jan10ML4Header4,
   LC.Jan10ML4sig1, LC.Jan10ML4sig2, LC.Jan10ML4sig3, LC.Jan10ML4sig4)

rm(SO.Jan09ML3Header1, SO.Jan09ML3sig1, SO.Jan09ML4Header1, SO.Jan09ML4sig1,
   SO.Jan10ML3Header1, SO.Jan10ML3sig1, SO.Jan10ML3Header2, SO.Jan10ML3sig2, 
   SO.Jan10ML4Header1, SO.Jan10ML4sig1, SO.Jan10ML4Header2, SO.Jan10ML4sig2)

rm(bufferaspacing12, bufferaspacing23, bufferaspacing34, bufferaspacing45,
   bufferaweightl1,bufferaweightl2,bufferaweightl3,bufferaweightl4,bufferaweightl5,
   bufferaweightr1,bufferaweightr2,bufferaweightr3,bufferaweightr4,bufferaweightr5,
   bufferduration, buffergvw, bufferlen, buffernumpnt, buffertimewindow)

rm(bufferaspacing56,bufferaspacing67, bufferaspacing78, bufferaspacing89)
rm( bufferaweightl6, bufferaweightl7, bufferaweightl8, bufferaweightl9,
   bufferaweightr6, bufferaweightr7, bufferaweightr8, bufferaweightr9)

rm(x,y,i,j, len, splineUp, inUpsig, inDownsig)

rm(SO.Jan0910Header, SO.Jan0910sig, SO.Jan09Header, SO.Jan09sig, SO.Jan10Header, SO.Jan10sig,
   LC.Jan0910Header, LC.Jan0910sig, LC.Jan09Header, LC.Jan09sig, LC.Jan10Header, LC.Jan10sig)
