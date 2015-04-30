install.packages('EMD')
library(EMD)

ndata <- 3000
tt2 <- seq(0, 9, length=ndata)
xt2 <- sin(pi * tt2) + sin(5* pi * tt2) + sin(10 * pi * tt2) + 0.5 * tt2
try <- emd(xt2, tt2, boundary="wave")
par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
rangeimf <- range(try$imf)
for(i in 1:try$nimf) {
  plot(tt2, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
       main=paste(i, "-th IMF", sep="")); abline(h=0)
}

plot(tt2,xt2)
plot(tt2, try$residue, xlab="", ylab="", main="residue", type="l", axes=FALSE); box()

utils::View(try$imf[,])

log (  (sum(try$imf[,1] * try$imf[,1])) / 3000   )
log (  (sum(try$imf[,2] * try$imf[,2])) / 3000   )
log (  (sum(try$imf[,3] * try$imf[,3])) / 3000   )

new <- data.frame()
for (i in 1:3000){
   new[1,i] <- sum(try$imf[i,2] +  try$imf[i,3] + try$residue[i])
}

plot(tt2, new)
time <- seq(from= 0, to= 1, by = 1/(1000))


DownObj <- match (Downheader_ID,  DownsigID) # check!
UpObj <- match (Upheader_ID,  UpsigID) # check!


Downtest_7 <- spline ( 1:1000 , Downobjout [DownObj[7], ] , length(time) ) $y 
Downtest_8 <- spline ( 1:1000 , Downobjout [DownObj[8], ] , length(time) ) $y 
Downtest_9 <- spline ( 1:1000 , Downobjout [DownObj[9], ] , length(time) ) $y 


Uptest_7_1 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[7]  ]][11] , Upheader_ID), ] , length(time) )$y
Uptest_7_2 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[7]  ]][12] , Upheader_ID), ] , length(time) )$y

Uptest_8_1 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[8]  ]][12] , Upheader_ID), ] , length(time) )$y
Uptest_8_2 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[8]  ]][13] , Upheader_ID), ] , length(time) )$y
Uptest_8_3 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[8]  ]][14] , Upheader_ID), ] , length(time) )$y

Uptest_9_1 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[9]  ]][1] , Upheader_ID), ] , length(time) )$y
Uptest_9_2 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[9]  ]][2] , Upheader_ID), ] , length(time) )$y
Uptest_9_3 <- spline ( 1:1000, Upobjout[ match (Upsiglist[[ DownObj[9]  ]][3] , Upheader_ID), ] , length(time) )$y

ndata <- 1000
try_down_7 <- emddenoise(Downtest_7, time,  boundary="wave")
try_down_8 <- emd(Downtest_8, time,  boundary="wave")
try_down_9 <- emd(Downtest_9, time, boundary="wave")

try_up_7_1 <- emd( Uptest_7_1, time)
try_up_7_2 <- emd( Uptest_7_2, time)

try_up_9_2 <- emd ( Uptest_9_2, time )
try_up_9_3 <- emd ( Uptest_9_3, time )

try <- try_up_9_2
try <- try_down_9

par(mfrow=c(try$nimf+1, 1), mar=c(2,1,2,1))
rangeimf <- range(try$imf)
for(i in 1:try$nimf) {
  plot(time, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
       main=paste(i, "-th IMF", sep="")); abline(h=0)
}

# plot(time,Uptest_7_1)
plot(time, try$residue, xlab="", ylab="", main="residue", type="l", axes=FALSE); box()

log2 (  (sum(try$imf[,1] * try$imf[,1])) / 1000   )
log2 (  (sum(try$imf[,2] * try$imf[,2])) / 1000  )
log2 (  (sum(try$imf[,3] * try$imf[,3])) / 4   )
log2 (  (sum(try$imf[,4] * try$imf[,4])) / 4   )
test <- data.frame()
for (i in 1: 1001){
 test[i,1] <-  try$imf[i,2] * try$imf[i,2]
}
log2 (sum(test) / 1001)

new <- data.frame()
for (i in 1:1001){
#   new[1,i] <- sum(try$imf[i,1] +  try$imf[i,3] + try$residue[i])
  new[1,i] <- sum( try$imf[i,2]  + try$residue[i])
}

new_up_9_2 <- new
new_down_9 <- new

plot (time, new_up_9_2)
plot (time, Uptest_9_2)

plot (time, new_up_9_3)
plot (time, Uptest_9_3)


plot (time, new_down_9)
plot (time, Downtest_9)


par(mfrow=c(4, 1), mar=c(2,1,2,1))
plot (time, Downtest_7)
plot (time, new_down_7)
plot (time, new_up_7_1)
plot (time, new_up_7_2)

plot (time, Downtest_7)
plot (time, new_down_7)
plot (time, Uptest_7_1)
plot (time, Uptest_7_2)


sum( abs ( new_down_9 - Uptest_9_2) )
a_magdif[[9]]
