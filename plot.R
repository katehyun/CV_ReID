### Plot
library(ggplot2)


#normalized plot
sigid <- 531357852282706
time <- seq(from= 0, to= 1, by = 1/(50-1))

insig <- match(sigid, Downheader_new$sigid)
insig_mag <- Downobjout[insig,]

insig_mag <- spline(time, insig_mag, 1000)
insig_time <- seq(from= 0, to= 1, by = 1/(1000-1))

sigplot <- plot(insig_mag$x , insig_mag$y)

sigset <- data.frame()
sigset <- data.frame(cbind(insig_mag$x , insig_mag$y))

ggplot()+
  geom_line(data=sigset ,aes(x=sigset[,1] , y=sigset[,2]),  color='blue' ) +
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=18),
        axis.text.y = element_text(colour="grey20",size=18),
        axis.title.x = element_text(colour="grey20",size=18),
        axis.title.y = element_text(colour="grey20",size=18))



# raw plot

Downsig = LC.Jan0910sig
Downsig_IM=subset(Downsig, select=c(id,mag,sigid)) 

# sigid <- 531357852282706 # 9 - ev
# sigid <- 531357852191467 # 15 - dump
# sigid <- 531357852193243 # 9 - pt
sigid <- 531357863048707 # 9 - auto
# sigid <- 531357863732217 # 9 - 40ft

# sigid <- 531357852208380 # 5 - ev
# sigid <- 531357852204304 # 5 - ev
# sigid <- 531357853185902 # 5 - ev
# sigid <- 531357853477372 # 5 - pt
# sigid <- 531357854067145# 5 - pt

# sigid <- 531357856010369 # 4 - bus
# sigid <- 531357858593159 # 4 - bus

Downidx <- match ( sigid ,Downsig_IM[,3] )
Downidx <- Downidx[!is.na(Downidx)]

DownsigID <- Downsig_IM[Downidx,3]
inDownsig <- Downsig_IM[Downidx+1,]
Downidx <- Downidx+1

while (Downsig_IM[Downidx+1,1] < 100){
  inDownsig <- rbind(inDownsig,Downsig_IM[Downidx+1,])
  Downidx <- Downidx+1
}

time <- inDownsig[,1]
mag <- inDownsig[,2]


sigset <- data.frame()
sigset <- data.frame(cbind(time, mag))


pt <- ggplot()+
  geom_line(data=sigset ,aes(x=sigset[,1] , y=sigset[,2]),  color='blue' ) +
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=15),
        axis.text.y = element_text(colour="grey20",size=15),
        axis.title.x = element_text(colour="grey20",size=15),
        axis.title.y = element_text(colour="grey20",size=15))


setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/Figure") 

ggsave(filename = paste("sig-" , sigid, ".jpeg" , sep="") , plot = pt)


# ss
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/DownsigID.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/UpsigID.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upsiglist.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downheader_new.RData")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/candidate_06232015.RData")

Upheader_ID <- Upheader_new$sigid
Downheader_ID <- Downheader_new$sigid

# pair 1 - 268
downsig <-531357852300982
upsig <- 941357850577474

# pair 2 - 276
downsig <-531357852440705
upsig <- 941357850704589

# pair 3 - 281
downsig <- 531357852472132
upsig <- 941357849890825

# pair 4 - 286
downsig <- 531357852536971
upsig <- 941357850876515

# pair 5 - 190
downsig <- 531357852615814
upsig <- 941357850764342

# pair 6 - 349
downsig <- 531357853370851 
upsig <- 941357851472498

# pair 7 - 351
downsig <- 531357853427668  
upsig <- 941357851501102

# pair 8 - 407
downsig <- 531357854438662  
upsig <- 941357852270347

# pair 9 - 243
downsig <- 531357854674917 
upsig <- 931357850753536

# pair 10 - 453
downsig <- 531357855079802  
upsig <- 941357852933790

# pair 11 - 509
downsig <- 531357856070451  
upsig <- 941357854246633

# pair 12- 520
downsig <- 531357856293427  
upsig <- 941357854484367

# pair 13 - 558
downsig <- 531357856974574  
upsig <- 941357855104032

# pair 14 - 567
downsig <- 531357857163271 
upsig <- 941357855660767

# pair 15 - 669
downsig <- 531357858789441  
upsig <- 941357856915294

# pair 16 - 768 - best
downsig <- 531357860157060 
# upsig <- 941357858150320
upsig <- as.numeric(Upsiglist[[768]][1])

# pair 17 - 782
downsig <- 531357860289745 
upsig <- 941357858479736

# pair 18 - 864
downsig <- 531357862192981 
upsig <- 941357860574865

# pair 19 - 933 - second best
downsig <- 531357863745084  
upsig <- 931357862151087

# pair 20 - 952
downsig <- 531357864281197  
upsig <- 941357862191267

# pair 21 - 966
downsig <- 531357864672974  
upsig <- 941357862783142

# pair 16 - 768 - best
downsig <- 531357860157060 
# upsig <- 941357858150320
for (i in 1: length( Upsiglist[[768]])){
  
upsig <- as.numeric(Upsiglist[[768]][i])

DownObj <- match (downsig ,  DownsigID) # check!
nu <- which(Upsiglist[[DownObj]] == upsig)
UpObj <- match (upsig,  UpsigID) # check!

splineDown <- Downobjout [DownObj , ]
time <- seq(from= 0, to= 1, by = 1/(length(splineDown)-1))
splineDown <- spline(time, splineDown, 1000)

splineUpidx <- match (Upsiglist[[ DownObj  ]][nu] , Upheader_ID)
splineUp <- Upobjout[splineUpidx,]
splineUp <- spline(time, splineUp, 1000)

splineUp_after <- data.frame()
splineUp_after <- candidate[[DownObj ]][[nu]]
splineUp_after <- spline(time, splineUp_after, 1000)



down <- data.frame()
down <- data.frame(cbind(splineDown$x, splineDown$y))
befores <- data.frame()
befores<- data.frame( cbind(splineUp$x, splineUp$y))
colnames(befores) <- c("time", "magnitude")
afters <- data.frame()
afters<- data.frame( cbind( splineUp_after$x, splineUp_after$y))
colnames(afters) <- c("time", "magnitude")



setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/ShiftandStretch/Comparison")
fontsize <- 9

pt_before <- ggplot()+
  geom_line(data = down, aes(x = down[,1] ,y = down[,2], colour = "Target"),  lwd=0.5) +
  geom_line(data = befores, aes(x = befores[,1] ,y = befores[,2], colour = "Candidate") , lwd=0.5) +
  scale_colour_manual("", 
                      values = c("Target"="red", "Candidate"="green") ) +
  theme(legend.text = element_text(colour = 'black',  size = fontsize, face = 'bold')) +
  xlab("\ntime") +
  ylab("magnitude\n") +
  theme(axis.text.x = element_text(colour="grey20",size=fontsize),
        axis.text.y = element_text(colour="grey20",size=fontsize),
        axis.title.x = element_text(colour="grey20",size=fontsize),
        axis.title.y = element_text(colour="grey20",size=fontsize),
        legend.position='top')


ggsave(filename = paste("shift and stretch_before", i, "-" ,downsig ,  ".jpeg") , plot = pt_before)


pt_after <-ggplot()+
  geom_line(data = down, aes(x = down[,1] ,y = down[,2], colour = "Target") , lwd=0.5) +
  geom_line(data = afters, aes(x = afters[,1] ,y = afters[,2], colour = "Candidate") , lwd=0.5) +
  scale_colour_manual("", 
                      values = c("Target"="red", "Candidate"="green")) +
  theme(legend.text = element_text(colour = 'black',  size = fontsize, face = 'bold')) +
  xlab("\ntime") +
  ylab("magnitude\n") +
  theme(axis.text.x = element_text(colour="grey20",size=fontsize),
        axis.text.y = element_text(colour="grey20",size=fontsize),
        axis.title.x = element_text(colour="grey20",size=fontsize),
        axis.title.y = element_text(colour="grey20",size=fontsize),
        legend.position='top')

ggsave(filename = paste("shift and stretch_after", i, "-" , downsig, ".jpeg") , plot = pt_after)

}

pt_before <- ggplot()+
  geom_line(data = down, aes(x = down[,1] ,y = down[,2]), colour = "DarkBlue" , lwd=2) +
 
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=fontsize),
        axis.text.y = element_text(colour="grey20",size=fontsize),
        axis.title.x = element_text(colour="grey20",size=fontsize),
        axis.title.y = element_text(colour="grey20",size=fontsize))


ggsave(filename = paste("Signature", downsig ,".jpeg") , plot = pt_before)

# 
# ggplot() +
#   geom_line(data=down , aes(x=down [,1] , y=down [,2]) , color='green') + 
#   geom_line(data=befores, aes(x=befores[,1] , y=befores[,2]) , color='red') + 
#   xlab("time") +
#   ylab("magnitude") +
#   theme(axis.text.x = element_text(colour="grey20",size=9),
#         axis.text.y = element_text(colour="grey20",size=9),
#         axis.title.x = element_text(colour="grey20",size=9),
#         axis.title.y = element_text(colour="grey20",size=9))

#   
# ggplot() +
#   geom_line(data=down , aes(x=down [,1] , y=down [,2])  , color='green') +
#   geom_line(data=afters, aes(x=afters[,1] , y=afters[,2]) , color='red') + 
#   scale_colour_manual("", 
#                       breaks = c("Target", "Candidate"),
#                       values = c("red", "green")) +
#   xlab("time") +
#   ylab("magnitude") +
#   theme(axis.text.x = element_text(colour="grey20",size=9),
#         axis.text.y = element_text(colour="grey20",size=9),
#         axis.title.x = element_text(colour="grey20",size=9),
#         axis.title.y = element_text(colour="grey20",size=9))
#  


## kernal plot

#plot b = parametric , Gaussian fitting data
i=20
b_mat_tt <- data.frame(diffseq_mat_c_tt[[i]], 
                       normal_mat_c_tt[[i]]  * multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]  )
b_nonmat_tt <- data.frame( diffseq_nonmat_c_tt[[i]], 
                           normal_nonmat_c_tt[[i]]  * multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red') +





# plot histogram
i=1
k <- i
plot( hist_nonmat_c_tt[[k]], col = "red", 
      xlim = c(min ( round_any (min(Diff_mat_train_c_tt[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_nonmat_train_c_tt[[k]]) , interval[k], f= floor) ) ,
               max ( round_any (max(Diff_mat_train_c_tt[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_nonmat_train_c_tt[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_tt[[k]]$count )),
      freq=TRUE,
      xlab = 'Feature Vector ', ylab = 'Density', main = 'Histogram')

plot( hist_mat_c_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

# histogram with density plot
# wim
interval  <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,  NA, NA, NA, NA, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5,0.5, 0.5,0.1, 0.1,
               NA, NA, NA, NA, NA, NA, NA, NA, 0.1, 0.1 )

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/Kernels")

for (k in 1: 31){
  if (k %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 

png(filename= paste("wim feature" , k, ".png", sep=""))

# jpeg("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/Paper Draft/Kernels/wim 3.jpeg")


ylimvalue <-  max( density(Diff_mat_train_c_tt[[k]])$y,
                        density(Diff_mat_train_c_tt[[k]])$y ) 
# library(plyr)
min_tt <- round_any (min(Diff_nonmat_train_c_tt[[k]] , Diff_mat_train_c_tt[[k]]) , interval[k], f= floor)
max_tt <- round_any (max(Diff_nonmat_train_c_tt[[k]] , Diff_mat_train_c_tt[[k]]) , interval[k], f= ceiling)

pt <- hist( Diff_nonmat_train_c_tt[[k]], col = rgb(1,0,0,0.5), 
      breaks=seq(min_tt ,max_tt, by=interval[k]), prob=TRUE, 
      ylim= c(0,ylimvalue), 
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      xlab = 'Feature Vector ', ylab = 'Density', 
      main =  paste("wim feature" , k,  sep=""))

hist( Diff_mat_train_c_tt[[k]], col = rgb(0,0,1,0.5),
     breaks=seq(min_tt ,max_tt, by=interval[k]), prob=TRUE, add=T)

curve(dnorm(x, mean=mean(Diff_nonmat_train_c_tt[[k]]), 
            sd=sqrt(var(Diff_nonmat_train_c_tt[[k]]))), 
      col="red", lwd=2, add=TRUE, yaxt="n")

curve(dnorm(x, mean=mean(Diff_mat_train_c_tt[[k]]), 
            sd=sqrt(var(Diff_mat_train_c_tt[[k]]))), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n" )

legend("topright", c("Matching", "Non-matching"), col=c("blue", "red"), lwd=5,
       cex=1.5)

dev.off()
}
}
# dev.copy(png,
#          "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/Paper Draft/Kernels/wim 3.png",width=8,height=6,units="in",res=100)






# sig

setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/Kernels")

for (k in 1: 50){

# dev.off()
# dev.new
png(filename= paste("sig feature" , k, ".png", sep=""))

ylimvalue <-  max( density(Diff_sig_mat_train_tt[[k]])$y,
                   density(Diff_sig_mat_train_tt[[k]])$y ) 

min_tt <- round_any( min(Diff_sig_nonmat_train_tt[[k]] , 
                         Diff_sig_mat_train_tt[[k]]) ,  intervalsig [k], f= floor)
max_tt <- round_any( max(Diff_sig_nonmat_train_tt[[k]] ,
                          Diff_sig_mat_train_tt[[k]]) ,  intervalsig [k], f= ceiling)

intervalsig  <- rep(0.01, 50)

pt <- hist( Diff_sig_nonmat_train_tt[[k]], col = rgb(1,0,0,0.5), 
      breaks=seq(min_tt, max_tt, by=intervalsig[k]), prob=TRUE, 
      ylim= c(0,ylimvalue),
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
      xlab = 'Feature Vector ', ylab = 'Density',
      main =  paste("sig feature" , k,  sep=""))


hist( Diff_sig_mat_train_tt[[k]], col = rgb(0,0,1,0.5),
      breaks=seq(min_tt ,max_tt, by=intervalsig [k]), prob=TRUE, add=T)

curve(dnorm(x, mean=mean(Diff_sig_nonmat_train_tt[[k]]), 
            sd=sqrt(var(Diff_sig_nonmat_train_tt[[k]]))), 
      col="red", lwd=2, add=TRUE, yaxt="n")

curve(dnorm(x, mean=mean(Diff_sig_mat_train_tt[[k]]), 
            sd=sqrt(var(Diff_sig_mat_train_tt[[k]]))), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

legend("topleft", c("Matching", "Non-matching"), col=c("blue", "red"), lwd=5,
       cex=1.5)

dev.off()
}

dev.off()
dev.new()
png(filename= paste("legend" ,  ".png", sep=""))
hist( Diff_sig_nonmat_train_tt[[k]], col = rgb(1,0,0,0.5), 
      breaks=seq(min_tt, max_tt, by=intervalsig[k]), prob=TRUE, 
      ylim= c(0,ylimvalue),
      xlab = 'Feature Vector ', ylab = 'Density',
      main =  paste("sig feature" , k,  sep="", plot=F))
legend("topright", c("Matching", "Non-matching"), col=c("blue", "red"), lwd=2)

#plot b = parametric , Gaussian fitting data
i=3
b_mat_tt <- data.frame(diffseq_mat_c_sig_tt[[i]], 
                       normal_mat_c_sig_tt[[i]]  * multiplier_hist_mat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_sig_tt[[i]] ) ) ]  )
b_nonmat_tt <- data.frame( diffseq_nonmat_c_sig_tt[[i]], 
                           normal_nonmat_c_sig_tt[[i]]  * multiplier_hist_nonmat_c_sig_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_sig_tt[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')


# plot histogram 
k <- i
plot( hist_nonmat_c_sig_tt[[k]], col = "red", 
      xlim = c(min (  round_any (min(Diff_sig_mat_train_tt[[k]]) , interval[k], f= floor)  , 
                     round_any (min(Diff_sig_nonmat_train_tt[[k]]) , interval[k], f= floor) ) ,
               max ( 0, round_any (max(Diff_sig_mat_train_tt[[k]]) , interval[k], f= ceiling) ,
                     round_any (max(Diff_sig_nonmat_train_tt[[k]]) , interval[k], f= ceiling) )),
      ylim= c(0,max(hist_mat_c_sig_tt[[k]]$count )),
      freq=TRUE,
      xlab = 'Feature Vector ', ylab = 'Density')

plot( hist_mat_c_sig_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

curve(dnorm(x, mean=mean(Diff_sig_mat_train_tt[[k]]), sd=sd(Diff_sig_mat_train_tt[[k]])), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")




#entropy filter
# plot - wim 
setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/EntropyFilter")
fontsize <- 18
fontsize2 <- 22

for (i in 1: 31){
  if (i %in% c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20,21,30,31) ){ 
    
    b_mat_tt <- data.frame(diffseq_mat_c_tt[[i]], 
                           normal_mat_c_tt[[i]]  * multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]  )
    b_nonmat_tt <- data.frame( diffseq_nonmat_c_tt[[i]], 
                               normal_nonmat_c_tt[[i]]  * multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]  )
    pt <- ggplot() + 
      geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2] , 
                colour="Matching") , lwd=2) +       
      geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ,
                colour="Non-Matching") , lwd=2 )   +
      scale_colour_manual("", 
                          values = c("Matching"="green" ,"Non-Matching"="red")) +
      theme(legend.text = element_text(colour = 'black',  size = fontsize, face = 'bold')) +
      xlab("\nFeature Vector") +
      ylab("Density\n") +
      theme(axis.text.x = element_text(colour="grey20",size=fontsize2),
            axis.text.y = element_text(colour="grey20",size=fontsize2),
            axis.title.x = element_text(colour="grey20",size=fontsize2),
            axis.title.y = element_text(colour="grey20",size=fontsize2),
            legend.position = c(0.8,0.9))
    
    #   pt <-   pt + xlim(-0.4, 0.4)
    
    ggsave(filename = paste("wim feature - gaussian tt" , i, ".jpeg" , sep="") , plot = pt)
  }
}


       


## end

setwd( "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Plot/Non_Matches") 
num <- 50

no <-28
Downplot <- f.drawDownsignature (no)


n1=28
n2=24
Upplot <- f.drawUpsignature (n1,n2)

n1=6
n2=3
Upplot <- f.drawUpsignature (n1,n2)


q=16
Downsigid <- 531357852282706
# Downsigid <- matchingonly[q,4]
Downplot <- f.Downdraw (Downsigid)

# Upsigid <- 941357772169808
Upsigid <- matchingonly[q,5]
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( as.numeric(Upsigid), as.numeric(Downsigid) ) 

Upsigid <- 941357772586625
Upplot <- f.Updraw (Upsigid)
Errorplot <- f.ErrorDraw ( Upsigid, Downsigid ) 



xfit_nonmat <- sort(Diff_nonmat_train_c_tt[[k]])
yfit_nonmat <- dnorm(xfit_nonmat,mean=mean(xfit_nonmat),sd=sd(xfit_nonmat)) 
yfit_nonmat <- spline( xfit_nonmat , yfit_nonmat ,1000)
lines(yfit_nonmat$x, yfit_nonmat$y, col="red", lwd=2)

xfit_mat <- sort(Diff_mat_train_c_tt[[k]])
yfit_mat <- dnorm(xfit_mat,mean=mean(xfit_mat),sd=sd(xfit_mat)) 
yfit_mat <- spline( xfit_mat , yfit_mat ,1000)
lines(yfit_mat$x, yfit_mat$y, col="green", lwd=2)

hist(Diff_mat_train_c_tt[[k]], density = T, plot=T)



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


  