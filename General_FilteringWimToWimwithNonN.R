
Upheader <- data.frame()
Upheader = SIG_Up[match (SIG_Up$id, WIM_SIG_pair$sig_id) , ]

Upheader = SO.Jan0910Header
a <- length(Upheader[1,]) 


for (j in 1: 31){
  Upheader[,a+j] <- SOJan_v1[,5+j][match( Upheader$sigid, SOJan_v1[,3])] # numaxle
}



# up cleaning
idx <- vector()
Upheader_new <-subset(Upheader, Upheader[,8] > 100) # samplecount
Upheader_new <-subset(Upheader_new, Upheader_new[,14] > 3)
Upheader_new <-subset(Upheader_new, Upheader_new[,14] < 15)

# wim data > 0 

Upheader_new <-subset(Upheader_new, Upheader_new[,7] >= 0.0 ) # new line
# Upheader_new <-subset(Upheader_new, Upheader_new[,27:44] >= 0.0 ) # new line



for (i in 1: nrow(Upheader_new)  ){
  if (Upheader_new[i,15] == 2 ) {
    idx[i] <- any(Upheader_new[i,19]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:30]<=0)
  }  
  else if (Upheader_new[i,15] == 3 ) {
    idx[i] <- any(Upheader_new[i,19:20]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:32]<=0)
  }
  else if (Upheader_new[i,15] == 4 ) {
    idx[i] <- any(Upheader_new[i,19:21]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:34]<=0)
  }
  else if (Upheader_new[i,15] == 5 ) {
    idx[i] <- any(Upheader_new[i,19:22]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:36]<=0)
  }
  else if (Upheader_new[i,15] == 6 ) {
    idx[i] <- any(Upheader_new[i,19:23]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:38]<=0)
  }
  else if (Upheader_new[i,15] == 7 ) {
    idx[i] <- any(Upheader_new[i,19:24]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:40]<=0)
  }
  else if (Upheader_new[i,15] == 8 ) {
    idx[i] <- any(Upheader_new[i,19:25]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:42]<=0)
  }
  else if (Upheader_new[i,15] == 9) {
    idx[i] <- any(Upheader_new[i,19:26]<=0)
    idx[i] <- idx[i] + any(Upheader_new[i,27:44]<=0)
  }
}

idxUp <- idx
whichidxUp <- which(idxUp > 0)
if ( length(whichidxUp ) > 0 ) { 
  Upheader_new <- Upheader_new[-whichidxUp,]
}