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



Downsigid <- 531357775430643
Downplot <- f.Downdraw (Downsigid)

Upsigid <- 941357773870388
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 

Upsigid <- 941357772466203
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 


jpeg( paste("Error", Downsigid[1] , Upsigid[1], ".jpeg"));
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ); 
dev.off()


n1=6
n2=2
UpplotAfterSS <- f.UpdrawAfterSS(n1,n2)


Upsigid <- 941357775815254
Downsigid <- 531357777730247
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 

library(pracma)
trapz(Errorplot,0)
require(pracma)


require(MESS)
auc(Errorplot, 0)

plot( kernel_mat[[9]]$x, kernel_mat[[9]]$y)
plot( kernel_nonmat[[9]]$x, kernel_nonmat[[9]]$y)

