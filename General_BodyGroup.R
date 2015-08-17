rm(list=ls())
bodygroup = read.table( "C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/bodygroup_nogroupname.txt", header=T)
load("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/ResultMisMatching_a_set1v2.RData")

options(scipen=999) 

ResultMisMatching_b <- data.frame()
ResultMisMatching_b <- ResultMisMatching_a[,1:4]

col <- 16
ResultMisMatching_b <- cbind(ResultMisMatching_b , ResultMisMatching_a[,col])

downbody <-  bodygroup [ match(ResultMisMatching_b[,4] , bodygroup[,1]) , 2]
downbody <-  cbind( downbody , bodygroup [ match(ResultMisMatching_b[,4] , bodygroup[,1]) , 3] )

ResultMisMatching_b <- cbind(ResultMisMatching_b , downbody)

colnames(ResultMisMatching_b) <- c( "ts", "FHWAclass" , "a_magdif" , "DownID" ,  "UpID" , 
                                    "bodyid", "bodygroup")


ResultMisMatching_b_class9 <- subset(ResultMisMatching_b , ResultMisMatching_b[,2] == 9 )
ResultMisMatching_b_tt <- subset(ResultMisMatching_b , ResultMisMatching_b[,2] >= 8 )


interest <- c( 90, 91, 92, 903, 904 , 94 , 906, 908, 95 ) # pt, ev, tk, logging, livestock, 40ft, agri, 53 , opentop/dump
# bodygroup <- list()
# bodygroup[[1]] <- c(80, 91,  93, 97,99, 103, 111) # ev
# bodygroup[[2]] <- c(81, 82, 90, 100, 114, 102,114, 142 ) # pt
# bodygroup[[3]] <- c(92, 112, 144) # tk
# bodygroup[[4]] <- c(903) # log
# bodygroup[[5]] <- c(84, 141 , 904) #ls
# bodygroup[[6]] <- c(94, 101, 900, 901) #40ft
# bodygroup[[7]] <- c(908) #53ft
# bodygroup[[8]] <- c(118, 906) #agri
# bodygroup[[9]] <- c(95, 110, 140) #opentopdumn

# TOD analysis
len = 3 * 24 #3 days * 24 hrs
ts <-1434524400000   # milisecond
mintoms <- 60000 #min to milisecond (1min = 60000ms)
tsseq  <- vector()
for ( i in 0 : len ){
  timeinterval <- ts + i * 60 * mintoms
  tsseq <- cbind(tsseq , timeinterval)
}


Total_len_all <- data.frame()
Matched_len_all <- data.frame()
Matched_rate_all <- data.frame()
Total_len_body <- data.frame()
Matched_len_body <- data.frame()
Matched_rate_body <- data.frame()

ResultMisMatching_temp <- data.frame()

tsidx <- vector()

for ( i in 1: (length(tsseq)-1)){
  
  tsidx <- which( ResultMisMatching_b_class9[,1] > tsseq[i] & ResultMisMatching_b_class9[,1] <= tsseq[i+1]   )
#   tsidx <- which( ResultMisMatching_b_tt[,1] > tsseq[i] & ResultMisMatching_b_tt[,1] <= tsseq[i+1]   )
  
  ResultMisMatching_temp <- ResultMisMatching_b_class9[tsidx,]
#   ResultMisMatching_temp <- ResultMisMatching_b_tt[tsidx,]
  
  Total_len_all <- nrow(ResultMisMatching_temp) 
  Matched_len_all <- sum(ResultMisMatching_temp[,5]!=999) 
  Matched_rate_all <-rbind( Matched_rate_all,  
                            cbind(tsseq[i] , Total_len_all , Matched_len_all, Matched_len_all  / Total_len_all ) )
 
#   for (j in 1: length(bodygroup)){
for (j in 1: length(bodygroup)){
    
    
    ResultMisMatching_body_temp <- subset( ResultMisMatching_temp ,  ResultMisMatching_temp[,7] %in% bodygroup[[j]])
    
    Total_len_body <- length(ResultMisMatching_body_temp[,1]) 
    Matched_len_body <- sum(ResultMisMatching_body_temp[,5]!=999) 
    Matched_rate_body <-rbind(  Matched_rate_body,  
                              cbind(tsseq[i] ,  j, Total_len_body , Matched_len_body, Matched_len_body  / Total_len_body  ) )
  }
  
  
}

save(Matched_rate_all , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Matched_rate_all_class9.RData")
save(Matched_rate_body, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/Matched_rate_body_class9.RData")

write.table(Matched_rate_all , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/ Matched_rate_all_class9.txt")
write.table(Matched_rate_body , file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_2015/ Matched_rate_body_class9.txt")
