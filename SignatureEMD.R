
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData") # 1000 features
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData") # 1000 features

install.packages('EMD')
library(EMD)

# Downobjout_emd 
# Upobjout_emd

num=50
time <- seq(from= 0, to= 1, by = 1/(num-1))
mad <- c()
sigma <- c()
gamma <- c()

Downobjout_emd <- data.frame()
energy <- vector()

# for (i in 1 : length(Downobjout[,1])){
for ( i in 1:5){
  
  try  <- emd(Downobjout[i,], time , boundary = "symmetric")
  
  for(j in 1:try$nimf) {    
#     mad <- median( abs( try$imf[,j] - median (try$imf[,j]) ) )
    energy[j] <- log (  (sum(try$imf[,j] * try$imf[,j])) / num   )
#     sigma <- mad/0.6745
    sigma <- energy/0.6745
#     gamma <- sigma * sqrt (2 * log ( try$nimf, base=exp(1)))   
#     gamma <- sigma * sqrt (energy * 2 * log ( try$nimf, base=exp(1)))  
#     try$imf[,j][abs( try$imf[,j] ) < gamma ] <- 0     
    
   
    
  }
  
  temp <- rowSums ( try$imf[,] )
  temp2 <- t ( rowSums ( try$imf[,1:4] ) +  (try$residue[]) )
  Downobjout_emd <- rbind(Downobjout_emd, temp2)
  
}

plot( time, Downobjout[i,] )
plot( time, Downobjout_emd[1,] )
plot( time, temp2)
i=4

temp3 <-    try$imf[,2]  +  try$imf[,3]   +  try$imf[,4]  +  try$imf[,5] + try$residue
plot( time, temp3)



i=20
energy <- vector()
try  <- emd(Downobjout[i,], time , boundary = "symmetric")
par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
rangeimf <- range(try$imf)
imfsig <- rep(0, 50)

if (try$nimf >= 2){
 
  
  for(i in 1:try$nimf) {
      plot(time, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
           main=paste(i, "-th IMF", sep="")); abline(h=0)
    }
  plot(time, try$residue, xlab="", ylab="", main="residue", type="l", axes=FALSE); box()

  for(j in 1:try$nimf) {    
      energy[j] <- log (  (sum(try$imf[,j] * try$imf[,j])) / num   )
    }
  View(energy)
  

        for(k in 1:try$nimf - 1){
          imfsig <- imfsig + try$imf[,k+1]
        }
        imfsig <- imfsig + try$residue

  
  else
    imfsig <- Downobjout[i,]
}


if (try$nimf <= 1){
  imfsig <- Downobjout[i,]
}


Before <- data.frame(time, Downobjout[i,])
After <- data.frame(time, imfsig)
                   

ggplot() + 
#   stat_smooth(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ,method = "loess",  se=FALSE, size=1) + 
  geom_line(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ) +
  geom_line(data=After , aes ( x=After [,1] , y=After [,2]  ) , color='red') 
 


ggplot(After,aes(x=After[,1] , y=After[,2])) + geom_point(size=3) + theme_minimal()


# plot - Before
time <- seq(from= 0, to= 1, by = 1/(num-1))
insig_time <- time

# sigidDown=531357857704444
sigidDown=ResultMisMatching_train[i,3] 
Down <- data.frame(time, Downobjout[match(sigidDown, Downheader_new$sigid),] )
sigplot <- plot(Down[,1], Down[,2], main=paste("Candidate (Downstream)", sigidDown[1]))

# sigidUp_est=941357854484367
sigidUp_est= ResultMisMatching_train[i,4]
Up_est <- data.frame(time, Upobjout[match(sigidUp_est, Upheader_new_nonN$sigid),] )
sigplot <- plot(Up_est[,1], Up_est[,2], main=paste("Target (Upstream)", sigidUp_est[1]))

# sigidUp_act=931357856156511
sigidUp_est= ResultMisMatching_train[i,2]
Up_act<- data.frame(time, Upobjout[match(sigidUp_act, Upheader_new_nonN$sigid),] )
sigplot <- plot(Up_act[,1], Up_act[,2], main=paste("Target (Upstream)", sigidUp_act[1]))

ggplot()+
  geom_line(data=Down, aes ( x=Down[,1] , y=Down[,2]  ) , color='green' ) +
  geom_line(data=Up_est , aes ( x=Up_est[,1] , y=Up_est [,2]  ) , color='red') +
  geom_line(data=Up_act , aes ( x=Up_act[,1] , y=Up_act [,2]  ) , color='blue') 


# try imf
energy <- vector()
try_down  <- emd(Down[,2], time , boundary = "symmetric")
par(mfrow=c(try_down$nimf+1, 1), mar=c(2,1,2,1))
rangeimf <- range(try_down$imf)

imfsig_down <- rep(0, 50)


if (try_down$nimf >= 2){
  
  
  for(i in 1:try_down$nimf) {
    plot(time, try_down$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
         main=paste(i, "-th IMF", sep="")); abline(h=0)
  }
  plot(time, try_down$residue, xlab="", ylab="", main="residue", type="l", axes=FALSE); box()
  
  for(j in 1:try_down$nimf) {    
    energy[j] <- log (  (sum(try_down$imf[,j] * try_down$imf[,j])) / num   )
  }
  View(energy)
  
  
 if (try_down$nimf > 2){
   imfsig_down <- rowSums( try_down$imf[,2:try_down$nimf] ) + (try_down$residue[]) 
 }
 if (try_down$nimf == 2){
   imfsig_down <- try_down$imf[,2] + (try_down$residue[]) 
 }

}



if (try_down$nimf <= 1){
  imfsig_down <-  Down[,2]
}


Before_down <- data.frame(time, Down[,2])
After_down <- data.frame(time, imfsig_down)


ggplot() + 
  #   stat_smooth(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ,method = "loess",  se=FALSE, size=1) + 
  geom_line(data=Before_down, aes ( x=Before_down[,1] , y=Before_down[,2]  ) , color='green' ) +
  geom_line(data=After_down , aes ( x=After_down [,1] , y=After_down [,2]  ) , color='red') 


ggplot() + 
  #   stat_smooth(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ,method = "loess",  se=FALSE, size=1) + 
  geom_line(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ) 

imfsig_upest <- rep(0, 50)
imfsig_upact <- rep(0, 50)
try_upest  <- emd(Up_est[,2], time , boundary = "symmetric")
try_upact  <- emd(Up_act[,2], time , boundary = "symmetric")


if (try_upest$nimf > 2){
  for(k in 1:try_upest$nimf - 1){
    imfsig_upest <- rowSums( try_upest$imf[,2:try_upest$nimf] ) + (try_upest$residue[]) 
  }
}


if (try_upest$nimf == 2){
  for(k in 1:try_upest$nimf - 1){
    imfsig_upest <- ( try_upest$imf[,2:try_upest$nimf] ) + (try_upest$residue[]) 
  }
}

if (try_upest$nimf <= 1){
  imfsig_upest <-  Up_est[,2]
}



if (try_upact$nimf > 2){
  for(k in 1:try_upact$nimf - 1){
    imfsig_upact <- rowSums( try_upact$imf[,2:try_upact$nimf] ) + (try_upact$residue[]) 
  }
}

if (try_upact$nimf == 2){
  for(k in 1:try_upact$nimf - 1){
    imfsig_upact <- ( try_upact$imf[,2:try_upact$nimf] ) + (try_upact$residue[]) 
  }
}

if (try_upact$nimf <= 1){
  imfsig_upact <-  Up_act[,2]
}


Before_upest <- data.frame(time, Up_est[,2])
After_upest <- data.frame(time, imfsig_upest)
ggplot() + 
  #   stat_smooth(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ,method = "loess",  se=FALSE, size=1) + 
  geom_line(data=Before_upest, aes ( x=Before_upest[,1] , y=Before_upest[,2]  ) , color='green' ) +
  geom_line(data=After_upest , aes ( x=After_upest [,1] , y=After_upest [,2]  ) , color='red') 

Before_upact <- data.frame(time, Up_act[,2])
After_upact <- data.frame(time, imfsig_upact)
ggplot() + 
  #   stat_smooth(data=Before, aes ( x=Before[,1] , y=Before[,2]  ) , color='green' ,method = "loess",  se=FALSE, size=1) + 
  geom_line(data=Before_upact, aes ( x=Before_upact[,1] , y=Before_upact[,2]  ) , color='green' ) +
  geom_line(data=After_upact , aes ( x=After_upact [,1] , y=After_upact [,2]  ) , color='red') 


ggplot() +
  geom_line(data=Down, aes ( x=Down[,1] , y=Down[,2]  ) , color='green' ) +
  geom_line(data=Up_est , aes ( x=Up_est[,1] , y=Up_est [,2]  ) , color='red') +
  geom_line(data=Up_act , aes ( x=Up_act[,1] , y=Up_act [,2]  ) , color='blue') +

  geom_line( data=After_down , aes ( x=After_down [,1] , y=After_down [,2]  ) , color='green', linetype="dotted", size=1) +
  geom_line(data=After_upest , aes ( x=After_upest [,1] , y=After_upest [,2]  ) , color='red', linetype="dotted", size=1) +
  geom_line(data=After_upact , aes ( x=After_upact [,1] , y=After_upact [,2]  ) , color='blue', linetype="dotted", size=1) 

  

sum ( abs(After_down[,2] - After_upest[,2]))
sum ( abs(After_down[,2] - After_upact[,2]))

sum ( abs(Down[,2] - Up_est[,2]))
sum ( abs(Down[,2]  - Up_act[,2]))

