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
# sigid <- 531357863048707 # 9 - auto
# sigid <- 531357863732217 # 9 - 40ft

# sigid <- 531357852208380 # 5 - ev
# sigid <- 531357852204304 # 5 - ev
# sigid <- 531357853185902 # 5 - ev
# sigid <- 531357853477372 # 5 - pt
# sigid <- 531357854067145# 5 - pt

# sigid <- 531357856010369 # 4 - bus
sigid <- 531357858593159 # 4 - bus

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
  theme(axis.text.x = element_text(colour="grey20",size=9),
        axis.text.y = element_text(colour="grey20",size=9),
        axis.title.x = element_text(colour="grey20",size=9),
        axis.title.y = element_text(colour="grey20",size=9))


setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/Paper Draft/Figure") 

ggsave(filename = paste("sig-" , sigid, ".jpeg" , sep="") , plot = pt)


# ss
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Downobjout.RData ")
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/Jan0910/Upobjout.RData ")

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


ggplot() +
  geom_line(data=down , aes(x=down [,1] , y=down [,2]) , color='green') + 
  geom_line(data=befores, aes(x=befores[,1] , y=befores[,2]) , color='red') + 
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=9),
        axis.text.y = element_text(colour="grey20",size=9),
        axis.title.x = element_text(colour="grey20",size=9),
        axis.title.y = element_text(colour="grey20",size=9))



ggplot()+
  geom_line(data = down, aes(x = down[,1] ,y = down[,2], colour = "Target")) +
  geom_line(data = befores, aes(x = befores[,1] ,y = befores[,2], colour = "Candidate")) +
  scale_colour_manual("", 
                      values = c("Target"="red", "Candidate"="green") ) +
#   theme(legend.background = element_rect(colour = 'black', size = 1)) +
  theme(legend.text = element_text(colour = 'black',  size = 9, face = 'bold')) +
  theme(legend.position =c(0.9, 0.9)) +
#   theme(legend.position = 'top')+
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=9),
        axis.text.y = element_text(colour="grey20",size=9),
        axis.title.x = element_text(colour="grey20",size=9),
        axis.title.y = element_text(colour="grey20",size=9))

  
  
ggplot() +
  geom_line(data=down , aes(x=down [,1] , y=down [,2])  , color='green') +
  geom_line(data=afters, aes(x=afters[,1] , y=afters[,2]) , color='red') + 
  scale_colour_manual("", 
                      breaks = c("Target", "Candidate"),
                      values = c("red", "green")) +
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=9),
        axis.text.y = element_text(colour="grey20",size=9),
        axis.title.x = element_text(colour="grey20",size=9),
        axis.title.y = element_text(colour="grey20",size=9))
 


ggplot()+
  geom_line(data = down, aes(x = down[,1] ,y = down[,2], colour = "Target")) +
  geom_line(data = afters, aes(x = afters[,1] ,y = afters[,2], colour = "Candidate")) +
  scale_colour_manual("", 
                      values = c("Target"="red", "Candidate"="green")) +
  xlab("time") +
  ylab("magnitude") +
  theme(axis.text.x = element_text(colour="grey20",size=9),
        axis.text.y = element_text(colour="grey20",size=9),
        axis.title.x = element_text(colour="grey20",size=9),
        axis.title.y = element_text(colour="grey20",size=9))

## kernal plot

#plot b = parametric , Gaussian fitting data
i=1
b_mat_tt <- data.frame(diffseq_mat_c_tt[[i]], 
                       normal_mat_c_tt[[i]]  * multiplier_hist_mat_c_tt[[i]][ which.min(is.na( multiplier_hist_mat_c_tt[[i]] ) ) ]  )
b_nonmat_tt <- data.frame( diffseq_nonmat_c_tt[[i]], 
                           normal_nonmat_c_tt[[i]]  * multiplier_hist_nonmat_c_tt[[i]][ which.min(is.na( multiplier_hist_nonmat_c_tt[[i]] ) ) ]  )
ggplot() + 
  geom_line(data=b_mat_tt, aes ( x=b_mat_tt[,1] , y=b_mat_tt[,2]  ) , color='green') + 
  geom_line(data=b_nonmat_tt, aes ( x=b_nonmat_tt[,1] , y=b_nonmat_tt[,2]  ) , color='red')


# plot histogram
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

# sig

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
      xlab = 'Feature Vector ', ylab = 'Density', main = 'Histogram')

plot( hist_mat_c_sig_tt[[k]], col = rgb(0,1,0,0.5), freq=TRUE, add=T)

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


  