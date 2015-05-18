### Plot
setwd( "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Plot/Non_Matches") 
num <- 1000

no <-28
Downplot <- f.drawDownsignature (no)


n1=28
n2=24
Upplot <- f.drawUpsignature (n1,n2)

n1=6
n2=3
Upplot <- f.drawUpsignature (n1,n2)


q=16
Downsigid <- 531357852462433
Downsigid <- matchingonly[q,4]
Downplot <- f.Downdraw (Downsigid)

# Upsigid <- 941357772169808
Upsigid <- matchingonly[q,5]
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( as.numeric(Upsigid), as.numeric(Downsigid) ) 

Upsigid <-931357850800950
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 


jpeg( paste("Error", Downsigid[1] , Upsigid[1], ".jpeg"));
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ); 
dev.off()


n1=6
n2=2
UpplotAfterSS <- f.UpdrawAfterSS(n1,n2)

num <- 1000
Upsigid <- 931357850800950
Downsigid <- 531357852462433
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 

library(pracma)
trapz(Errorplot,0)
require(pracma)


require(MESS)
auc(Errorplot, 0)

plot( kernel_mat[[9]]$x, kernel_mat[[9]]$y)
plot( kernel_nonmat[[9]]$x, kernel_nonmat[[9]]$y)

