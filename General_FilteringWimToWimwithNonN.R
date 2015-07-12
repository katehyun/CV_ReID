# Upstream Set

Upheader <- data.frame()

Upheader <- SIG_Up[ match (  WIM_SIG_pair$sig_id  , SIG_Up$id ,  , nomatch = 0 ) , ]
Upheader <- cbind( WIM_SIG_pair$wim_id[ match ( Upheader$id , WIM_SIG_pair$sig_id  )], Upheader)

colnames(Upheader)[1:2] = c( "wim_id" , "sig_id")

Upheader <- cbind(Upheader ,  WIM_Up[ match ( Upheader$wim_id ,  WIM_Up$id  ) , ] )
Upheader <- na.omit( Upheader )
Upheader <- subset(Upheader , Upheader$numaxles > 0)


# axle spacing  temp list
axlespcmtemp <- list()
axlewtrtkgtemp <- list()
axlewtltkgtemp <- list()
for ( i in 1 : length(Upheader[,1])){
  axlespcmtemp[[i]] <-  unlist( strsplit( substr(Upheader$axlespccm[i], 2, nchar(Upheader$axlespccm[i])-1) , ",") )
  axlewtrtkgtemp[[i]] <-  unlist(strsplit( substr(Upheader$axlewtrtkg[i], 2, nchar(Upheader$axlewtrtkg[i])-1) , ",") )
  axlewtltkgtemp[[i]] <-  unlist(strsplit( substr(Upheader$axlewtltkg[i], 2, nchar(Upheader$axlewtltkg[i])-1) , ",") )
}

Upheader <- cbind(Upheader , matrix (rep(NA, 27), ncol = 27))
colnames(Upheader)[16:42] = c( "axsp12" , "axsp23" , "axsp34" , "axsp45",  "axsp56",  "axsp67",  "axsp78",  "axsp89", 
                                 "axwt1r" , "axwt2r" , "axwt3r" ,"axwt4r" , "axwt5r", "axwt6r" , "axwt7r" , "axwt8r", "axwt9r",
                                 "axwt1l" , "axwt2l" , "axwt3l" ,"axwt4l" , "axwt5l", "axwt6l" , "axwt7l", "axwt8l" , "axwt9l",
                                 "gvw")



for ( i in 1 : length(Upheader[,1])){
  
  j <- length ( axlespcmtemp[[i]] )
  
  if (j==1){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- 0
    Upheader$axsp34[i] <- 0
    Upheader$axsp45[i] <- 0
    Upheader$axsp56[i] <- 0
    Upheader$axsp67[i] <- 0
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- 0
    Upheader$axwt3l[i] <- 0
    Upheader$axwt4r[i] <- 0
    Upheader$axwt4l[i] <- 0
    Upheader$axwt5r[i] <- 0
    Upheader$axwt5l[i] <- 0
    Upheader$axwt6r[i] <- 0
    Upheader$axwt6l[i] <- 0
    Upheader$axwt7r[i] <- 0
    Upheader$axwt7l[i] <- 0
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    
    Upheader$gvw[i] <-   as.numeric(Upheader$axwt1r[i]) +  as.numeric(Upheader$axwt1l[i]) + 
                          as.numeric(Upheader$axwt2r[i]) +  as.numeric(Upheader$axwt2l[i])

  }
  
  if (j==2){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1])  * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2])  * 0.0328084
    Upheader$axsp34[i] <- 0
    Upheader$axsp45[i] <- 0
    Upheader$axsp56[i] <- 0
    Upheader$axsp67[i] <- 0
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- 0
    Upheader$axwt4l[i] <- 0
    Upheader$axwt5r[i] <- 0
    Upheader$axwt5l[i] <- 0
    Upheader$axwt6r[i] <- 0
    Upheader$axwt6l[i] <- 0
    Upheader$axwt7r[i] <- 0
    Upheader$axwt7l[i] <- 0
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    
    Upheader$gvw[i] <-   as.numeric(Upheader$axwt1r[i]) +  as.numeric(Upheader$axwt1l[i]) +  
                          as.numeric(Upheader$axwt2r[i]) +  as.numeric(Upheader$axwt2l[i]) + 
                           as.numeric(Upheader$axwt3r[i]) +   as.numeric(Upheader$axwt3l[i])
  }
  
  if (j==3){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- 0
    Upheader$axsp56[i] <- 0
    Upheader$axsp67[i] <- 0
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- 0
    Upheader$axwt5l[i] <- 0
    Upheader$axwt6r[i] <- 0
    Upheader$axwt6l[i] <- 0
    Upheader$axwt7r[i] <- 0
    Upheader$axwt7l[i] <- 0
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
                        as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
                        as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) +
                        as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i])
  }
  
  if (j==4){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Upheader$axsp56[i] <- 0
    Upheader$axsp67[i] <- 0
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt6r[i] <- 0
    Upheader$axwt6l[i] <- 0
    Upheader$axwt7r[i] <- 0
    Upheader$axwt7l[i] <- 0
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
      as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
      as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) + 
      as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i]) +  
      as.numeric(Upheader$axwt5r[i]) + as.numeric(Upheader$axwt5l[i])
  }
 
  if (j==5){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Upheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Upheader$axsp67[i] <- 0
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt7r[i] <- 0
    Upheader$axwt7l[i] <- 0
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
      as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
      as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) + 
      as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i]) +  
      as.numeric(Upheader$axwt5r[i]) + as.numeric(Upheader$axwt5l[i]) +
      as.numeric(Upheader$axwt6r[i]) + as.numeric(Upheader$axwt6l[i])
  }
  
  if (j==6){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Upheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Upheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Upheader$axsp78[i] <- 0
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt8r[i] <- 0
    Upheader$axwt8l[i] <- 0
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
      as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
      as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) + 
      as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i]) +  
      as.numeric(Upheader$axwt5r[i]) + as.numeric(Upheader$axwt5l[i]) +
      as.numeric(Upheader$axwt6r[i]) + as.numeric(Upheader$axwt6l[i]) +
      as.numeric(Upheader$axwt7r[i]) + as.numeric(Upheader$axwt7l[i])
  }
  
  if (j==7){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Upheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Upheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Upheader$axsp78[i] <- as.numeric(axlespcmtemp[[i]][7]) * 0.0328084
    Upheader$axsp89[i] <- 0
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt8r[i] <- as.numeric(axlewtrtkgtemp[[i]][8]) * 2.20462 / 1000
    Upheader$axwt8l[i] <- as.numeric(axlewtltkgtemp[[i]][8]) * 2.20462 / 1000
    Upheader$axwt9r[i] <- 0
    Upheader$axwt9l[i] <- 0
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
      as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
      as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) + 
      as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i]) +  
      as.numeric(Upheader$axwt5r[i]) + as.numeric(Upheader$axwt5l[i]) +
      as.numeric(Upheader$axwt6r[i]) + as.numeric(Upheader$axwt6l[i]) +
      as.numeric(Upheader$axwt7r[i]) + as.numeric(Upheader$axwt7l[i]) +
      as.numeric(Upheader$axwt8r[i]) + as.numeric(Upheader$axwt8l[i])
  }
  
  if (j==8){
    Upheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Upheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Upheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Upheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Upheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Upheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Upheader$axsp78[i] <- as.numeric(axlespcmtemp[[i]][7]) * 0.0328084
    Upheader$axsp89[i] <- as.numeric(axlespcmtemp[[i]][8]) * 0.0328084
    
    Upheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Upheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Upheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Upheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Upheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Upheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Upheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Upheader$axwt8r[i] <- as.numeric(axlewtrtkgtemp[[i]][8]) * 2.20462 / 1000
    Upheader$axwt8l[i] <- as.numeric(axlewtltkgtemp[[i]][8]) * 2.20462 / 1000
    Upheader$axwt9r[i] <- as.numeric(axlewtrtkgtemp[[i]][9]) * 2.20462 / 1000
    Upheader$axwt9l[i] <- as.numeric(axlewtltkgtemp[[i]][9]) * 2.20462 / 1000
    Upheader$gvw[i] <-  as.numeric(Upheader$axwt1r[i]) + as.numeric(Upheader$axwt1l[i]) +
      as.numeric(Upheader$axwt2r[i]) + as.numeric(Upheader$axwt2l[i]) + 
      as.numeric(Upheader$axwt3r[i]) + as.numeric(Upheader$axwt3l[i]) + 
      as.numeric(Upheader$axwt4r[i]) + as.numeric(Upheader$axwt4l[i]) +  
      as.numeric(Upheader$axwt5r[i]) + as.numeric(Upheader$axwt5l[i]) +
      as.numeric(Upheader$axwt6r[i]) + as.numeric(Upheader$axwt6l[i]) +
      as.numeric(Upheader$axwt7r[i]) + as.numeric(Upheader$axwt7l[i]) +
      as.numeric(Upheader$axwt8r[i]) + as.numeric(Upheader$axwt8l[i]) +
      as.numeric(Upheader$axwt9r[i]) + as.numeric(Upheader$axwt9l[i]) 
  }
}

# clearing - remove NA



naidx  <- vector()
for (i in 1: length(Upheader[,1])){ 
  if( any(is.na(Upheader[i,]))  ){
    naidx <- cbind(naidx, i)
  }
}

if ( length(naidx) > 0){
  Upheader <- Upheader[-naidx,]
}


# FHWA Class 

Upheader <- cbind(Upheader, NA)
colnames(Upheader)[length(Upheader[1,])] <- c("FHWAclass")

for (i in 1: length( Upheader[,1])){
  if ( Upheader$numaxles[i] == 2){
      if ( Upheader$axsp12[i] >= 1.00 & Upheader$axsp12[i] < 5.99 ){
        Upheader$FHWAclass[i] <- 1
      }
      if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 10.11 & 
             Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 7.99  ){
        Upheader$FHWAclass[i] <- 2
      } 
      if ( Upheader$axsp12[i] >= 10.11 & Upheader$axsp12[i] < 23.09 & 
             Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 7.99  ){
        Upheader$FHWAclass[i] <- 3
      } 
      if ( Upheader$axsp12[i] >= 23.10 & Upheader$axsp12[i] < 40.00 & 
             Upheader$gvw[i] >= 12.00   ){
        Upheader$FHWAclass[i] <- 4
      }
      if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & 
             Upheader$gvw[i] >= 8.00   ){
        Upheader$FHWAclass[i] <- 5
      }  
  }
  
  if ( Upheader$numaxles[i] == 3){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 10.10 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 25.00 &
           Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 11.99){
      Upheader$FHWAclass[i] <- 2
    }
    if ( Upheader$axsp12[i] >= 10.11 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 25.00 &
           Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 11.99){
      Upheader$FHWAclass[i] <- 3
    }
    if ( Upheader$axsp12[i] >= 23.10 & Upheader$axsp12[i] < 40.00 & Upheader$axsp23[i] >= 3.00 & Upheader$axsp23[i] < 7.00 &
           Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 4
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 30.00 &
           Upheader$gvw[i] >= 12.00 & Upheader$gvw[i] < 19.99){
      Upheader$FHWAclass[i] <- 5
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$gvw[i] >= 12.00 ){
      Upheader$FHWAclass[i] <- 6
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 11.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 8
    }
  }
  
  if ( Upheader$numaxles[i] == 4){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 10.10 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 30.00 &
           Upheader$axsp34[i] >= 1.00 & Upheader$axsp34[i] < 11.99 &
           Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 11.99){
      Upheader$FHWAclass[i] <- 2
    }
    if ( Upheader$axsp12[i] >= 10.11 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 30.00 &
           Upheader$axsp34[i] >= 1.00 & Upheader$axsp34[i] < 11.99 &
         Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 11.99){
      Upheader$FHWAclass[i] <- 3
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 26.10 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 40.00 &
           Upheader$axsp34[i] >= 1.00 & Upheader$axsp34[i] < 20.20 &
           Upheader$gvw[i] >= 12.00 & Upheader$gvw[i] < 19.99){
      Upheader$FHWAclass[i] <- 5
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$axsp34[i] >= 2.50 & Upheader$axsp34[i] < 12.99 &
         Upheader$gvw[i] >= 12.00){
      Upheader$FHWAclass[i] <- 7
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 26.00 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$axsp34[i] >= 13.00 & Upheader$axsp34[i] < 50.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 8
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 26.00 & Upheader$axsp23[i] >= 8.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$axsp34[i] >= 2.50 & Upheader$axsp34[i] < 20.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 8
    }    
  }
  
  
  if ( Upheader$numaxles[i] == 5){
    if ( Upheader$axsp12[i] >= 10.11 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 25.00 &
           Upheader$axsp34[i] >= 1.00 & Upheader$axsp34[i] < 11.99 & Upheader$axsp45[i] >= 1.00 & Upheader$axsp45[i] < 11.99 &
         Upheader$gvw[i] >= 1.00 & Upheader$gvw[i] < 11.99){
      Upheader$FHWAclass[i] <- 3
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 6.00 & Upheader$axsp23[i] < 35.00 &
           Upheader$axsp34[i] >= 1.00 & Upheader$axsp34[i] < 25.00 & Upheader$axsp45[i] >= 1.00 & Upheader$axsp45[i] < 11.99 &
         Upheader$gvw[i] >= 12.00 & Upheader$gvw[i] < 19.99){
      Upheader$FHWAclass[i] <- 5
    }  
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 23.09 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$axsp34[i] >= 2.50 & Upheader$axsp34[i] < 6.29 & Upheader$axsp45[i] >= 2.50 & Upheader$axsp45[i] < 6.30 &
         Upheader$gvw[i] >= 12.00 ){
      Upheader$FHWAclass[i] <- 7
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 30.00 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$axsp34[i] >= 6.30 & Upheader$axsp34[i] < 65.00 & Upheader$axsp45[i] >= 2.50 & Upheader$axsp45[i] < 11.99 &
         Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 9
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 30.00 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.29 &
           Upheader$axsp34[i] >= 6.30 & Upheader$axsp34[i] < 50.00 & Upheader$axsp45[i] >= 12.00 & Upheader$axsp45[i] < 27.00 &
         Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 14
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 30.00 & Upheader$axsp23[i] >= 16.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$axsp34[i] >= 2.50 & Upheader$axsp34[i] < 6.30 & Upheader$axsp45[i] >= 2.50 & Upheader$axsp45[i] < 11.99 &
         Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 9
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 30.00 & Upheader$axsp23[i] >= 11.00 & Upheader$axsp23[i] < 26.00 &
           Upheader$axsp34[i] >= 6.00 & Upheader$axsp34[i] < 20.00 & Upheader$axsp45[i] >= 11.00 & Upheader$axsp45[i] < 26.00 &
         Upheader$gvw[i] >= 20.00 ){
      Upheader$FHWAclass[i] <- 11
    }
  }
  
  if ( Upheader$numaxles[i] == 6){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 26.00 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.30 &
           Upheader$axsp34[i] >= 6.10 & Upheader$axsp34[i] < 50.00 & Upheader$axsp45[i] >= 2.50 & Upheader$axsp45[i] < 11.99 &
           Upheader$axsp56[i] >= 2.50 & Upheader$axsp56[i] < 10.99 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 10
    }
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 26.00 & Upheader$axsp23[i] >= 2.50 & Upheader$axsp23[i] < 6.30 &
           Upheader$axsp34[i] >= 11.00 & Upheader$axsp34[i] < 26.00 & Upheader$axsp45[i] >= 6.00 & Upheader$axsp45[i] < 24.00 &
           Upheader$axsp56[i] >= 11.00 & Upheader$axsp56[i] < 26.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 12
    }
  }
  
  if ( Upheader$numaxles[i] == 7){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 45.00 & Upheader$axsp23[i] >= 3.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$axsp34[i] >= 3.00 & Upheader$axsp34[i] < 45.00 & Upheader$axsp45[i] >= 3.00 & Upheader$axsp45[i] < 45.00 &
           Upheader$axsp56[i] >= 3.00 & Upheader$axsp56[i] < 45.00 & Upheader$axsp67[i] >= 3.00 & Upheader$axsp67[i] < 45.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 13
    }
  }
  
  if ( Upheader$numaxles[i] == 8){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 45.00 & Upheader$axsp23[i] >= 3.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$axsp34[i] >= 3.00 & Upheader$axsp34[i] < 45.00 & Upheader$axsp45[i] >= 3.00 & Upheader$axsp45[i] < 45.00 &
           Upheader$axsp56[i] >= 3.00 & Upheader$axsp56[i] < 45.00 & Upheader$axsp67[i] >= 3.00 & Upheader$axsp67[i] < 45.00 &
           Upheader$axsp78[i] >= 3.00 & Upheader$axsp78[i] < 45.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 13
    }
  }
  
  if ( Upheader$numaxles[i] == 9){
    if ( Upheader$axsp12[i] >= 6.00 & Upheader$axsp12[i] < 45.00 & Upheader$axsp23[i] >= 3.00 & Upheader$axsp23[i] < 45.00 &
           Upheader$axsp34[i] >= 3.00 & Upheader$axsp34[i] < 45.00 & Upheader$axsp45[i] >= 3.00 & Upheader$axsp45[i] < 45.00 &
           Upheader$axsp56[i] >= 3.00 & Upheader$axsp56[i] < 45.00 & Upheader$axsp67[i] >= 3.00 & Upheader$axsp67[i] < 45.00 &
           Upheader$axsp78[i] >= 3.00 & Upheader$axsp78[i] < 45.00 & Upheader$axsp89[i] >= 3.00 & Upheader$axsp89[i] < 45.00 &
         Upheader$gvw[i] >= 20.00){
      Upheader$FHWAclass[i] <- 13
    }
  }
}

Upheader$FHWAclass[is.na( Upheader$FHWAclass)] <-15

# up cleaning 
Upheader_new <- data.frame()
Upheader_new <-subset(Upheader, Upheader$FHWAclass > 3)
Upheader_new <-subset(Upheader_new, Upheader_new$FHWAclass < 15)



idx <- vector()

for (i in 1: nrow(Upheader_new)  ){
  if (Upheader_new$numaxles[i] == 2 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
  }  
  else if (Upheader_new$numaxles[i] == 3 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
  }
  else if (Upheader_new$numaxles[i] == 4 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
  }
  else if (Upheader_new$numaxles[i] == 5 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- Upheader_new$axsp45<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
    idx[i] <- idx[i] + Upheader_new$axwt5l<=0
    idx[i] <- idx[i] + Upheader_new$axwt5r<=0
  }
  else if (Upheader_new$numaxles[i] == 6 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- Upheader_new$axsp45<=0
    idx[i] <- Upheader_new$axsp56<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
    idx[i] <- idx[i] + Upheader_new$axwt5l<=0
    idx[i] <- idx[i] + Upheader_new$axwt5r<=0
    idx[i] <- idx[i] + Upheader_new$axwt6l<=0
    idx[i] <- idx[i] + Upheader_new$axwt6r<=0
  }
  else if (Upheader_new$numaxles[i] == 7 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- Upheader_new$axsp45<=0
    idx[i] <- Upheader_new$axsp56<=0
    idx[i] <- Upheader_new$axsp67<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
    idx[i] <- idx[i] + Upheader_new$axwt5l<=0
    idx[i] <- idx[i] + Upheader_new$axwt5r<=0
    idx[i] <- idx[i] + Upheader_new$axwt6l<=0
    idx[i] <- idx[i] + Upheader_new$axwt6r<=0
    idx[i] <- idx[i] + Upheader_new$axwt7l<=0
    idx[i] <- idx[i] + Upheader_new$axwt7r<=0
  }
  else if (Upheader_new$numaxles[i] == 8 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- Upheader_new$axsp45<=0
    idx[i] <- Upheader_new$axsp56<=0
    idx[i] <- Upheader_new$axsp67<=0
    idx[i] <- Upheader_new$axsp78<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
    idx[i] <- idx[i] + Upheader_new$axwt5l<=0
    idx[i] <- idx[i] + Upheader_new$axwt5r<=0
    idx[i] <- idx[i] + Upheader_new$axwt6l<=0
    idx[i] <- idx[i] + Upheader_new$axwt6r<=0
    idx[i] <- idx[i] + Upheader_new$axwt7l<=0
    idx[i] <- idx[i] + Upheader_new$axwt7r<=0
    idx[i] <- idx[i] + Upheader_new$axwt8l<=0
    idx[i] <- idx[i] + Upheader_new$axwt8r<=0
  }
  else if (Upheader_new$numaxles[i] == 9 ) {
    idx[i] <- Upheader_new$axsp12<=0
    idx[i] <- Upheader_new$axsp23<=0
    idx[i] <- Upheader_new$axsp34<=0
    idx[i] <- Upheader_new$axsp45<=0
    idx[i] <- Upheader_new$axsp56<=0
    idx[i] <- Upheader_new$axsp67<=0
    idx[i] <- Upheader_new$axsp78<=0
    idx[i] <- Upheader_new$axsp89<=0
    idx[i] <- idx[i] + Upheader_new$axwt1l<=0
    idx[i] <- idx[i] + Upheader_new$axwt1r<=0
    idx[i] <- idx[i] + Upheader_new$axwt2l<=0
    idx[i] <- idx[i] + Upheader_new$axwt2r<=0
    idx[i] <- idx[i] + Upheader_new$axwt3l<=0
    idx[i] <- idx[i] + Upheader_new$axwt3r<=0
    idx[i] <- idx[i] + Upheader_new$axwt4l<=0
    idx[i] <- idx[i] + Upheader_new$axwt4r<=0
    idx[i] <- idx[i] + Upheader_new$axwt5l<=0
    idx[i] <- idx[i] + Upheader_new$axwt5r<=0
    idx[i] <- idx[i] + Upheader_new$axwt6l<=0
    idx[i] <- idx[i] + Upheader_new$axwt6r<=0
    idx[i] <- idx[i] + Upheader_new$axwt7l<=0
    idx[i] <- idx[i] + Upheader_new$axwt7r<=0
    idx[i] <- idx[i] + Upheader_new$axwt8l<=0
    idx[i] <- idx[i] + Upheader_new$axwt8r<=0
    idx[i] <- idx[i] + Upheader_new$axwt9l<=0
    idx[i] <- idx[i] + Upheader_new$axwt9r<=0
  }
}

whichidxUp <- which(idx > 0)
if ( length(whichidxUp ) > 0 ) { 
  Upheader_new <- Upheader_new[-whichidxUp,]
}

#Down header

Downheader <- data.frame()
Downheader <- SIG_Down[ match ( WIM_SIG_pair$sig_id ,  SIG_Down$id , nomatch = 0 ) , ]
Downheader <- cbind( WIM_SIG_pair$wim_id[ match (Downheader$id, WIM_SIG_pair$sig_id  )], Downheader)

colnames(Downheader)[1:2] = c( "wim_id" , "sig_id")

Downheader <- cbind(Downheader ,  WIM_Down[ match ( Downheader$wim_id ,  WIM_Down$id  ) , ] )
Downheader <- na.omit( Downheader )
Downheader <- subset(Downheader , Downheader$numaxles > 0)


# axle spacing  temp list
axlespcmtemp <- list()
axlewtrtkgtemp <- list()
axlewtltkgtemp <- list()
for ( i in 1 : length(Downheader[,1])){
  axlespcmtemp[[i]] <-  unlist( strsplit( substr(Downheader$axlespccm[i], 2, nchar(Downheader$axlespccm[i])-1) , ",") )
  axlewtrtkgtemp[[i]] <-  unlist(strsplit( substr(Downheader$axlewtrtkg[i], 2, nchar(Downheader$axlewtrtkg[i])-1) , ",") )
  axlewtltkgtemp[[i]] <-  unlist(strsplit( substr(Downheader$axlewtltkg[i], 2, nchar(Downheader$axlewtltkg[i])-1) , ",") )
}

Downheader <- cbind(Downheader , matrix (rep(NA, 27), ncol = 27))
colnames(Downheader)[16:42] = c( "axsp12" , "axsp23" , "axsp34" , "axsp45",  "axsp56",  "axsp67",  "axsp78",  "axsp89", 
                               "axwt1r" , "axwt2r" , "axwt3r" ,"axwt4r" , "axwt5r", "axwt6r" , "axwt7r" , "axwt8r", "axwt9r",
                               "axwt1l" , "axwt2l" , "axwt3l" ,"axwt4l" , "axwt5l", "axwt6l" , "axwt7l", "axwt8l" , "axwt9l",
                               "gvw")

for ( i in 1 : length(Downheader[,1])){
  
  j <- length ( axlespcmtemp[[i]] )
  
  if (j==1){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- 0
    Downheader$axsp34[i] <- 0
    Downheader$axsp45[i] <- 0
    Downheader$axsp56[i] <- 0
    Downheader$axsp67[i] <- 0
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- 0
    Downheader$axwt3l[i] <- 0
    Downheader$axwt4r[i] <- 0
    Downheader$axwt4l[i] <- 0
    Downheader$axwt5r[i] <- 0
    Downheader$axwt5l[i] <- 0
    Downheader$axwt6r[i] <- 0
    Downheader$axwt6l[i] <- 0
    Downheader$axwt7r[i] <- 0
    Downheader$axwt7l[i] <- 0
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    
    Downheader$gvw[i] <-   as.numeric(Downheader$axwt1r[i]) +  as.numeric(Downheader$axwt1l[i]) + 
      as.numeric(Downheader$axwt2r[i]) +  as.numeric(Downheader$axwt2l[i])
    
  }
  
  if (j==2){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1])  * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2])  * 0.0328084
    Downheader$axsp34[i] <- 0
    Downheader$axsp45[i] <- 0
    Downheader$axsp56[i] <- 0
    Downheader$axsp67[i] <- 0
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- 0
    Downheader$axwt4l[i] <- 0
    Downheader$axwt5r[i] <- 0
    Downheader$axwt5l[i] <- 0
    Downheader$axwt6r[i] <- 0
    Downheader$axwt6l[i] <- 0
    Downheader$axwt7r[i] <- 0
    Downheader$axwt7l[i] <- 0
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    
    Downheader$gvw[i] <-   as.numeric(Downheader$axwt1r[i]) +  as.numeric(Downheader$axwt1l[i]) +  
      as.numeric(Downheader$axwt2r[i]) +  as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) +   as.numeric(Downheader$axwt3l[i])
  }
  
  if (j==3){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- 0
    Downheader$axsp56[i] <- 0
    Downheader$axsp67[i] <- 0
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- 0
    Downheader$axwt5l[i] <- 0
    Downheader$axwt6r[i] <- 0
    Downheader$axwt6l[i] <- 0
    Downheader$axwt7r[i] <- 0
    Downheader$axwt7l[i] <- 0
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) +
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i])
  }
  
  if (j==4){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Downheader$axsp56[i] <- 0
    Downheader$axsp67[i] <- 0
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt6r[i] <- 0
    Downheader$axwt6l[i] <- 0
    Downheader$axwt7r[i] <- 0
    Downheader$axwt7l[i] <- 0
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) + 
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i]) +  
      as.numeric(Downheader$axwt5r[i]) + as.numeric(Downheader$axwt5l[i])
  }
  
  if (j==5){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Downheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Downheader$axsp67[i] <- 0
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt7r[i] <- 0
    Downheader$axwt7l[i] <- 0
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) + 
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i]) +  
      as.numeric(Downheader$axwt5r[i]) + as.numeric(Downheader$axwt5l[i]) +
      as.numeric(Downheader$axwt6r[i]) + as.numeric(Downheader$axwt6l[i])
  }
  
  if (j==6){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Downheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Downheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Downheader$axsp78[i] <- 0
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt8r[i] <- 0
    Downheader$axwt8l[i] <- 0
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) + 
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i]) +  
      as.numeric(Downheader$axwt5r[i]) + as.numeric(Downheader$axwt5l[i]) +
      as.numeric(Downheader$axwt6r[i]) + as.numeric(Downheader$axwt6l[i]) +
      as.numeric(Downheader$axwt7r[i]) + as.numeric(Downheader$axwt7l[i])
  }
  
  if (j==7){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Downheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Downheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Downheader$axsp78[i] <- as.numeric(axlespcmtemp[[i]][7]) * 0.0328084
    Downheader$axsp89[i] <- 0
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt8r[i] <- as.numeric(axlewtrtkgtemp[[i]][8]) * 2.20462 / 1000
    Downheader$axwt8l[i] <- as.numeric(axlewtltkgtemp[[i]][8]) * 2.20462 / 1000
    Downheader$axwt9r[i] <- 0
    Downheader$axwt9l[i] <- 0
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) + 
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i]) +  
      as.numeric(Downheader$axwt5r[i]) + as.numeric(Downheader$axwt5l[i]) +
      as.numeric(Downheader$axwt6r[i]) + as.numeric(Downheader$axwt6l[i]) +
      as.numeric(Downheader$axwt7r[i]) + as.numeric(Downheader$axwt7l[i]) +
      as.numeric(Downheader$axwt8r[i]) + as.numeric(Downheader$axwt8l[i])
  }
  
  if (j==8){
    Downheader$axsp12[i] <- as.numeric(axlespcmtemp[[i]][1]) * 0.0328084
    Downheader$axsp23[i] <- as.numeric(axlespcmtemp[[i]][2]) * 0.0328084
    Downheader$axsp34[i] <- as.numeric(axlespcmtemp[[i]][3]) * 0.0328084
    Downheader$axsp45[i] <- as.numeric(axlespcmtemp[[i]][4]) * 0.0328084
    Downheader$axsp56[i] <- as.numeric(axlespcmtemp[[i]][5]) * 0.0328084
    Downheader$axsp67[i] <- as.numeric(axlespcmtemp[[i]][6]) * 0.0328084
    Downheader$axsp78[i] <- as.numeric(axlespcmtemp[[i]][7]) * 0.0328084
    Downheader$axsp89[i] <- as.numeric(axlespcmtemp[[i]][8]) * 0.0328084
    
    Downheader$axwt1r[i] <- as.numeric(axlewtrtkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt1l[i] <- as.numeric(axlewtltkgtemp[[i]][1]) * 2.20462 / 1000
    Downheader$axwt2r[i] <- as.numeric(axlewtrtkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt2l[i] <- as.numeric(axlewtltkgtemp[[i]][2]) * 2.20462 / 1000
    Downheader$axwt3r[i] <- as.numeric(axlewtrtkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt3l[i] <- as.numeric(axlewtltkgtemp[[i]][3]) * 2.20462 / 1000
    Downheader$axwt4r[i] <- as.numeric(axlewtrtkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt4l[i] <- as.numeric(axlewtltkgtemp[[i]][4]) * 2.20462 / 1000
    Downheader$axwt5r[i] <- as.numeric(axlewtrtkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt5l[i] <- as.numeric(axlewtltkgtemp[[i]][5]) * 2.20462 / 1000
    Downheader$axwt6r[i] <- as.numeric(axlewtrtkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt6l[i] <- as.numeric(axlewtltkgtemp[[i]][6]) * 2.20462 / 1000
    Downheader$axwt7r[i] <- as.numeric(axlewtrtkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt7l[i] <- as.numeric(axlewtltkgtemp[[i]][7]) * 2.20462 / 1000
    Downheader$axwt8r[i] <- as.numeric(axlewtrtkgtemp[[i]][8]) * 2.20462 / 1000
    Downheader$axwt8l[i] <- as.numeric(axlewtltkgtemp[[i]][8]) * 2.20462 / 1000
    Downheader$axwt9r[i] <- as.numeric(axlewtrtkgtemp[[i]][9]) * 2.20462 / 1000
    Downheader$axwt9l[i] <- as.numeric(axlewtltkgtemp[[i]][9]) * 2.20462 / 1000
    Downheader$gvw[i] <-  as.numeric(Downheader$axwt1r[i]) + as.numeric(Downheader$axwt1l[i]) +
      as.numeric(Downheader$axwt2r[i]) + as.numeric(Downheader$axwt2l[i]) + 
      as.numeric(Downheader$axwt3r[i]) + as.numeric(Downheader$axwt3l[i]) + 
      as.numeric(Downheader$axwt4r[i]) + as.numeric(Downheader$axwt4l[i]) +  
      as.numeric(Downheader$axwt5r[i]) + as.numeric(Downheader$axwt5l[i]) +
      as.numeric(Downheader$axwt6r[i]) + as.numeric(Downheader$axwt6l[i]) +
      as.numeric(Downheader$axwt7r[i]) + as.numeric(Downheader$axwt7l[i]) +
      as.numeric(Downheader$axwt8r[i]) + as.numeric(Downheader$axwt8l[i]) +
      as.numeric(Downheader$axwt9r[i]) + as.numeric(Downheader$axwt9l[i]) 
  }
}
# clearing - remove NA



naidx  <- vector()
for (i in 1: length(Downheader[,1])){ 
  if( any(is.na(Downheader[i,]))  ){
    naidx <- cbind(naidx, i)
  }
}

if ( length(naidx) > 0){
  Downheader <- Downheader[-naidx,]
}


# FHWA Class 

Downheader <- cbind(Downheader, NA)
colnames(Downheader)[length(Downheader[1,])] <- c("FHWAclass")

for (i in 1: length( Downheader[,1])){
  if ( Downheader$numaxles[i] == 2){
    if ( Downheader$axsp12[i] >= 1.00 & Downheader$axsp12[i] < 5.99 ){
      Downheader$FHWAclass[i] <- 1
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 10.11 & 
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 7.99  ){
      Downheader$FHWAclass[i] <- 2
    } 
    if ( Downheader$axsp12[i] >= 10.11 & Downheader$axsp12[i] < 23.09 & 
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 7.99  ){
      Downheader$FHWAclass[i] <- 3
    } 
    if ( Downheader$axsp12[i] >= 23.10 & Downheader$axsp12[i] < 40.00 & 
           Downheader$gvw[i] >= 12.00   ){
      Downheader$FHWAclass[i] <- 4
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & 
           Downheader$gvw[i] >= 8.00   ){
      Downheader$FHWAclass[i] <- 5
    }  
  }
  
  if ( Downheader$numaxles[i] == 3){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 10.10 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 25.00 &
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 11.99){
      Downheader$FHWAclass[i] <- 2
    }
    if ( Downheader$axsp12[i] >= 10.11 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 25.00 &
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 11.99){
      Downheader$FHWAclass[i] <- 3
    }
    if ( Downheader$axsp12[i] >= 23.10 & Downheader$axsp12[i] < 40.00 & Downheader$axsp23[i] >= 3.00 & Downheader$axsp23[i] < 7.00 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 4
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 30.00 &
           Downheader$gvw[i] >= 12.00 & Downheader$gvw[i] < 19.99){
      Downheader$FHWAclass[i] <- 5
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$gvw[i] >= 12.00 ){
      Downheader$FHWAclass[i] <- 6
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 11.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 8
    }
  }
  
  if ( Downheader$numaxles[i] == 4){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 10.10 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 30.00 &
           Downheader$axsp34[i] >= 1.00 & Downheader$axsp34[i] < 11.99 &
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 11.99){
      Downheader$FHWAclass[i] <- 2
    }
    if ( Downheader$axsp12[i] >= 10.11 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 30.00 &
           Downheader$axsp34[i] >= 1.00 & Downheader$axsp34[i] < 11.99 &
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 11.99){
      Downheader$FHWAclass[i] <- 3
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 26.10 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 40.00 &
           Downheader$axsp34[i] >= 1.00 & Downheader$axsp34[i] < 20.20 &
           Downheader$gvw[i] >= 12.00 & Downheader$gvw[i] < 19.99){
      Downheader$FHWAclass[i] <- 5
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$axsp34[i] >= 2.50 & Downheader$axsp34[i] < 12.99 &
           Downheader$gvw[i] >= 12.00){
      Downheader$FHWAclass[i] <- 7
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 26.00 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$axsp34[i] >= 13.00 & Downheader$axsp34[i] < 50.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 8
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 26.00 & Downheader$axsp23[i] >= 8.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$axsp34[i] >= 2.50 & Downheader$axsp34[i] < 20.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 8
    }    
  }
  
  
  if ( Downheader$numaxles[i] == 5){
    if ( Downheader$axsp12[i] >= 10.11 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 25.00 &
           Downheader$axsp34[i] >= 1.00 & Downheader$axsp34[i] < 11.99 & Downheader$axsp45[i] >= 1.00 & Downheader$axsp45[i] < 11.99 &
           Downheader$gvw[i] >= 1.00 & Downheader$gvw[i] < 11.99){
      Downheader$FHWAclass[i] <- 3
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 6.00 & Downheader$axsp23[i] < 35.00 &
           Downheader$axsp34[i] >= 1.00 & Downheader$axsp34[i] < 25.00 & Downheader$axsp45[i] >= 1.00 & Downheader$axsp45[i] < 11.99 &
           Downheader$gvw[i] >= 12.00 & Downheader$gvw[i] < 19.99){
      Downheader$FHWAclass[i] <- 5
    }  
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 23.09 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$axsp34[i] >= 2.50 & Downheader$axsp34[i] < 6.29 & Downheader$axsp45[i] >= 2.50 & Downheader$axsp45[i] < 6.30 &
           Downheader$gvw[i] >= 12.00 ){
      Downheader$FHWAclass[i] <- 7
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 30.00 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$axsp34[i] >= 6.30 & Downheader$axsp34[i] < 65.00 & Downheader$axsp45[i] >= 2.50 & Downheader$axsp45[i] < 11.99 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 9
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 30.00 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.29 &
           Downheader$axsp34[i] >= 6.30 & Downheader$axsp34[i] < 50.00 & Downheader$axsp45[i] >= 12.00 & Downheader$axsp45[i] < 27.00 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 14
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 30.00 & Downheader$axsp23[i] >= 16.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$axsp34[i] >= 2.50 & Downheader$axsp34[i] < 6.30 & Downheader$axsp45[i] >= 2.50 & Downheader$axsp45[i] < 11.99 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 9
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 30.00 & Downheader$axsp23[i] >= 11.00 & Downheader$axsp23[i] < 26.00 &
           Downheader$axsp34[i] >= 6.00 & Downheader$axsp34[i] < 20.00 & Downheader$axsp45[i] >= 11.00 & Downheader$axsp45[i] < 26.00 &
           Downheader$gvw[i] >= 20.00 ){
      Downheader$FHWAclass[i] <- 11
    }
  }
  
  if ( Downheader$numaxles[i] == 6){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 26.00 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.30 &
           Downheader$axsp34[i] >= 6.10 & Downheader$axsp34[i] < 50.00 & Downheader$axsp45[i] >= 2.50 & Downheader$axsp45[i] < 11.99 &
           Downheader$axsp56[i] >= 2.50 & Downheader$axsp56[i] < 10.99 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 10
    }
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 26.00 & Downheader$axsp23[i] >= 2.50 & Downheader$axsp23[i] < 6.30 &
           Downheader$axsp34[i] >= 11.00 & Downheader$axsp34[i] < 26.00 & Downheader$axsp45[i] >= 6.00 & Downheader$axsp45[i] < 24.00 &
           Downheader$axsp56[i] >= 11.00 & Downheader$axsp56[i] < 26.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 12
    }
  }
  
  if ( Downheader$numaxles[i] == 7){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 45.00 & Downheader$axsp23[i] >= 3.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$axsp34[i] >= 3.00 & Downheader$axsp34[i] < 45.00 & Downheader$axsp45[i] >= 3.00 & Downheader$axsp45[i] < 45.00 &
           Downheader$axsp56[i] >= 3.00 & Downheader$axsp56[i] < 45.00 & Downheader$axsp67[i] >= 3.00 & Downheader$axsp67[i] < 45.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 13
    }
  }
  
  if ( Downheader$numaxles[i] == 8){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 45.00 & Downheader$axsp23[i] >= 3.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$axsp34[i] >= 3.00 & Downheader$axsp34[i] < 45.00 & Downheader$axsp45[i] >= 3.00 & Downheader$axsp45[i] < 45.00 &
           Downheader$axsp56[i] >= 3.00 & Downheader$axsp56[i] < 45.00 & Downheader$axsp67[i] >= 3.00 & Downheader$axsp67[i] < 45.00 &
           Downheader$axsp78[i] >= 3.00 & Downheader$axsp78[i] < 45.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 13
    }
  }
  
  if ( Downheader$numaxles[i] == 9){
    if ( Downheader$axsp12[i] >= 6.00 & Downheader$axsp12[i] < 45.00 & Downheader$axsp23[i] >= 3.00 & Downheader$axsp23[i] < 45.00 &
           Downheader$axsp34[i] >= 3.00 & Downheader$axsp34[i] < 45.00 & Downheader$axsp45[i] >= 3.00 & Downheader$axsp45[i] < 45.00 &
           Downheader$axsp56[i] >= 3.00 & Downheader$axsp56[i] < 45.00 & Downheader$axsp67[i] >= 3.00 & Downheader$axsp67[i] < 45.00 &
           Downheader$axsp78[i] >= 3.00 & Downheader$axsp78[i] < 45.00 & Downheader$axsp89[i] >= 3.00 & Downheader$axsp89[i] < 45.00 &
           Downheader$gvw[i] >= 20.00){
      Downheader$FHWAclass[i] <- 13
    }
  }
}

Downheader$FHWAclass[is.na( Downheader$FHWAclass)] <-15


# down cleaning 
Downheader_new <- data.frame()
Downheader_new <-subset(Downheader, Downheader$FHWAclass > 3)
Downheader_new <-subset(Downheader_new, Downheader_new$FHWAclass < 15)



idx <- vector()

for (i in 1: nrow(Downheader_new)  ){
  if (Downheader_new$numaxles[i] == 2 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
  }  
  else if (Downheader_new$numaxles[i] == 3 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
  }
  else if (Downheader_new$numaxles[i] == 4 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
  }
  else if (Downheader_new$numaxles[i] == 5 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- Downheader_new$axsp45<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
    idx[i] <- idx[i] + Downheader_new$axwt5l<=0
    idx[i] <- idx[i] + Downheader_new$axwt5r<=0
  }
  else if (Downheader_new$numaxles[i] == 6 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- Downheader_new$axsp45<=0
    idx[i] <- Downheader_new$axsp56<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
    idx[i] <- idx[i] + Downheader_new$axwt5l<=0
    idx[i] <- idx[i] + Downheader_new$axwt5r<=0
    idx[i] <- idx[i] + Downheader_new$axwt6l<=0
    idx[i] <- idx[i] + Downheader_new$axwt6r<=0
  }
  else if (Downheader_new$numaxles[i] == 7 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- Downheader_new$axsp45<=0
    idx[i] <- Downheader_new$axsp56<=0
    idx[i] <- Downheader_new$axsp67<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
    idx[i] <- idx[i] + Downheader_new$axwt5l<=0
    idx[i] <- idx[i] + Downheader_new$axwt5r<=0
    idx[i] <- idx[i] + Downheader_new$axwt6l<=0
    idx[i] <- idx[i] + Downheader_new$axwt6r<=0
    idx[i] <- idx[i] + Downheader_new$axwt7l<=0
    idx[i] <- idx[i] + Downheader_new$axwt7r<=0
  }
  else if (Downheader_new$numaxles[i] == 8 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- Downheader_new$axsp45<=0
    idx[i] <- Downheader_new$axsp56<=0
    idx[i] <- Downheader_new$axsp67<=0
    idx[i] <- Downheader_new$axsp78<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
    idx[i] <- idx[i] + Downheader_new$axwt5l<=0
    idx[i] <- idx[i] + Downheader_new$axwt5r<=0
    idx[i] <- idx[i] + Downheader_new$axwt6l<=0
    idx[i] <- idx[i] + Downheader_new$axwt6r<=0
    idx[i] <- idx[i] + Downheader_new$axwt7l<=0
    idx[i] <- idx[i] + Downheader_new$axwt7r<=0
    idx[i] <- idx[i] + Downheader_new$axwt8l<=0
    idx[i] <- idx[i] + Downheader_new$axwt8r<=0
  }
  else if (Downheader_new$numaxles[i] == 9 ) {
    idx[i] <- Downheader_new$axsp12<=0
    idx[i] <- Downheader_new$axsp23<=0
    idx[i] <- Downheader_new$axsp34<=0
    idx[i] <- Downheader_new$axsp45<=0
    idx[i] <- Downheader_new$axsp56<=0
    idx[i] <- Downheader_new$axsp67<=0
    idx[i] <- Downheader_new$axsp78<=0
    idx[i] <- Downheader_new$axsp89<=0
    idx[i] <- idx[i] + Downheader_new$axwt1l<=0
    idx[i] <- idx[i] + Downheader_new$axwt1r<=0
    idx[i] <- idx[i] + Downheader_new$axwt2l<=0
    idx[i] <- idx[i] + Downheader_new$axwt2r<=0
    idx[i] <- idx[i] + Downheader_new$axwt3l<=0
    idx[i] <- idx[i] + Downheader_new$axwt3r<=0
    idx[i] <- idx[i] + Downheader_new$axwt4l<=0
    idx[i] <- idx[i] + Downheader_new$axwt4r<=0
    idx[i] <- idx[i] + Downheader_new$axwt5l<=0
    idx[i] <- idx[i] + Downheader_new$axwt5r<=0
    idx[i] <- idx[i] + Downheader_new$axwt6l<=0
    idx[i] <- idx[i] + Downheader_new$axwt6r<=0
    idx[i] <- idx[i] + Downheader_new$axwt7l<=0
    idx[i] <- idx[i] + Downheader_new$axwt7r<=0
    idx[i] <- idx[i] + Downheader_new$axwt8l<=0
    idx[i] <- idx[i] + Downheader_new$axwt8r<=0
    idx[i] <- idx[i] + Downheader_new$axwt9l<=0
    idx[i] <- idx[i] + Downheader_new$axwt9r<=0
  }
}

whichidxUp <- which(idx > 0)
if ( length(whichidxUp ) > 0 ) { 
  Downheader_new <- Downheader_new[-whichidxUp,]
}

# matching
WIM_SIG_pair <- WIM_SIG_pair



# threshold


buffertimewindow=120
bufferduration = 0.8
bufferlen = 800
bufferaspacing12 = 8
bufferaspacing23 = 5
bufferaspacing34 = 5
bufferaspacing45 = 6
bufferaspacing56 = 5
bufferaspacing67 = 5
bufferaspacing78 = 5
bufferaspacing89 = 5
buffergvw = 40

bufferaweightl1 = 8
bufferaweightr1 = 8
bufferaweightl2 = 8
bufferaweightr2 = 8
bufferaweightl3 = 8
bufferaweightr3 = 8
bufferaweightl4 = 8
bufferaweightr4 = 8
bufferaweightl5 = 8
bufferaweightr5 = 8
bufferaweightl6 = 8
bufferaweightr6 = 8
bufferaweightl7 = 8
bufferaweightr7 = 8
bufferaweightl8 = 8
bufferaweightr8 = 8
bufferaweightl9 = 8
bufferaweightr9 = 8


# set buffer
Downheader_ID=(Downheader_new$sig_id)

settime <- matrix(nrow=length(Downheader_ID), ncol=1)
setduration<- matrix(nrow=length(Downheader_ID), ncol=1)
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


options(scipen=999)

for (j in 1: length(Downheader_ID)){
  settime[j] <- as.numeric(Downheader_new$ts_fieldunit[j])
  lb[j] <- settime[j] - buffertimewindow * 60000  
}

for (j in 1: length(Downheader_ID)){
  setduration[j] <- as.numeric(Downheader_new$duration[j])
  ld[j] <- setduration[j] - bufferduration  
  ud[j] <- setduration[j] + bufferduration  
}


for (j in 1: length(Downheader_ID)){
  setlen[j] <- as.numeric(Downheader_new$vehlencm[j])
  ll[j] <- setlen[j] - bufferlen  
  ul[j] <- setlen[j] + bufferlen  
}

for (j in 1: length(Downheader_ID)){
  setgvw[j] <- as.numeric(Downheader_new$gvw[j])
  lg[j] <- setgvw[j] - buffergvw  
  ug[j] <- setgvw[j] + buffergvw 
}

for (j in 1: length(Downheader_ID)){
  setaspacing12[j] <- as.numeric(Downheader_new$axsp12[j])
  setaspacing23[j] <- as.numeric(Downheader_new$axsp23[j])
  setaspacing34[j] <- as.numeric(Downheader_new$axsp34[j])
  setaspacing45[j] <- as.numeric(Downheader_new$axsp45[j])
  setaspacing56[j] <- as.numeric(Downheader_new$axsp56[j])
  setaspacing67[j] <- as.numeric(Downheader_new$axsp67[j])
  setaspacing78[j] <- as.numeric(Downheader_new$axsp78[j])
  setaspacing89[j] <- as.numeric(Downheader_new$axsp89[j])
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
  setaweightl1[j] <- as.numeric(Downheader_new$axwt1l[j])
  setaweightr1[j] <- as.numeric(Downheader_new$axwt1r[j])
  setaweightl2[j] <- as.numeric(Downheader_new$axwt2l[j])
  setaweightr2[j] <- as.numeric(Downheader_new$axwt2r[j])
  setaweightl3[j] <- as.numeric(Downheader_new$axwt3l[j])
  setaweightr3[j] <- as.numeric(Downheader_new$axwt3r[j])
  setaweightl4[j] <- as.numeric(Downheader_new$axwt4l[j])
  setaweightr4[j] <- as.numeric(Downheader_new$axwt4r[j])
  setaweightl5[j] <- as.numeric(Downheader_new$axwt5l[j])
  setaweightr5[j] <- as.numeric(Downheader_new$axwt5r[j])
  setaweightl6[j] <- as.numeric(Downheader_new$axwt6l[j])
  setaweightr6[j] <- as.numeric(Downheader_new$axwt6r[j])
  setaweightl7[j] <- as.numeric(Downheader_new$axwt7l[j])
  setaweightr7[j] <- as.numeric(Downheader_new$axwt7r[j])
  setaweightl8[j] <- as.numeric(Downheader_new$axwt8l[j])
  setaweightr8[j] <- as.numeric(Downheader_new$axwt8r[j])
  setaweightl9[j] <- as.numeric(Downheader_new$axwt9l[j])
  setaweightr9[j] <- as.numeric(Downheader_new$axwt9r[j])
  
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



### Upsiglist 
Upsiglist <- list()

for (j in 1: length(Downheader_ID)){ 
  
  Upsiglist[j] <- list(subset(Upheader_new$sig_id,  Upheader_new$ts_fieldunit > lb[j] &  Upheader_new$ts_fieldunit <= settime[j]
                              
                              & Upheader_new$duration > ld[j] & Upheader_new$duration < ud[j]         
                              & as.numeric(Upheader_new$numaxles) == as.numeric(Downheader_new$numaxles[j])                   
                              & Upheader_new$vehlencm > ll[j] & Upheader_new$vehlencm < ul[j]
                              & Upheader_new$gvw > lg[j] & Upheader_new$gvw  < ug[j]

                              & Upheader_new$axsp34 > la34[j] & Upheader_new$axsp34 < ua34[j]
                              & Upheader_new$axsp45 > la45[j] & Upheader_new$axsp45 < ua45[j]
                              & Upheader_new$axsp56 > la56[j] & Upheader_new$axsp56 < ua56[j]
                              & Upheader_new$axsp67 > la67[j] & Upheader_new$axsp67 < ua67[j]
                              & Upheader_new$axsp78 > la78[j] & Upheader_new$axsp78 < ua78[j]
                              & Upheader_new$axsp89 > la89[j] & Upheader_new$axsp89 < ua89[j]
                              
                              & Upheader_new$axwt1l > lwl1[j] & Upheader_new$axwt1l < uwl1[j]
                              & Upheader_new$axwt1r > lwr1[j] & Upheader_new$axwt1r < uwr1[j]
                              & Upheader_new$axwt2l > lwl2[j] & Upheader_new$axwt2l < uwl2[j]
                              & Upheader_new$axwt2r > lwr2[j] & Upheader_new$axwt2r < uwr2[j]
                              & Upheader_new$axwt3l > lwl3[j] & Upheader_new$axwt3l < uwl3[j]
                              & Upheader_new$axwt3r > lwr3[j] & Upheader_new$axwt3r < uwr3[j]
                              & Upheader_new$axwt4l > lwl4[j] & Upheader_new$axwt4l < uwl4[j]
                              & Upheader_new$axwt4r > lwr4[j] & Upheader_new$axwt4r < uwr4[j]
                              & Upheader_new$axwt5l > lwl5[j] & Upheader_new$axwt5l < uwl5[j]
                              & Upheader_new$axwt5r > lwr5[j] & Upheader_new$axwt5r < uwr5[j]
                              & Upheader_new$axwt6l > lwl6[j] & Upheader_new$axwt6l < uwl6[j]
                              & Upheader_new$axwt6r > lwr6[j] & Upheader_new$axwt6r < uwr6[j]
                              & Upheader_new$axwt7l > lwl7[j] & Upheader_new$axwt7l < uwl7[j]
                              & Upheader_new$axwt7r > lwr7[j] & Upheader_new$axwt7r < uwr7[j]
                              & Upheader_new$axwt8l > lwl8[j] & Upheader_new$axwt8l < uwl8[j]
                              & Upheader_new$axwt8r > lwr8[j] & Upheader_new$axwt8r < uwr8[j]
                              & Upheader_new$axwt9l > lwl9[j] & Upheader_new$axwt9l < uwl9[j]
                              & Upheader_new$axwt9r > lwr9[j] & Upheader_new$axwt9r < uwr9[j]                    
  ))
}


# signature
DownsigID <- Downheader_new$sig_id
UpsigID <- Upheader_new$sig_id

sigtemp_Up <- list()
sigtemp_Down <- list()

for ( i in 1 : length(Upheader_new[,1])){
  sigtemp_Up[[i]] <-  unlist( strsplit( substr(Upheader_new$rawsig[i], 2, nchar(Upheader_new$rawsig[i])-1) , ",") )
}
for ( i in 1 : length(Downheader_new[,1])){
  sigtemp_Down[[i]] <-  unlist( strsplit( substr(Downheader_new$rawsig[i], 2, nchar(Downheader_new$rawsig[i])-1) , ",") )
}

num <- 50
no_round <- 100

Downobjout <- data.frame()

for (i in 1: length(sigtemp_Down)){

 
  inDownsig <- as.numeric(sigtemp_Down[[i]])
  inDownsig <- f.normalization(inDownsig)
  splineDown <- f.interpolation(inDownsig,num,no_round)
  colnames(splineDown) <- c("outDowntime", "outDownmag")
  Downobj <- c(splineDown[,2])
  Downobj <- t(Downobj)
  Downobjout <-rbind(Downobjout,  Downobj)
  
 
}

Upobjout <- data.frame()

for (i in 1: length(sigtemp_Up)){
  
  
  inUpsig <- as.numeric(sigtemp_Up[[i]])
  inUpsig <- f.normalization(inUpsig)
  splineUp <- f.interpolation(inUpsig,num,no_round)
  colnames(splineUp) <- c("outUptime", "ouUpmag")
  Upobj <- c(splineUp[,2])
  Upobj <- t(Upobj)
  Upobjout <-rbind(Upobjout,  Upobj)
  
  
}



setwd("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid") 
save(Upheader_new, file="./ProcessedData/General_April2015/Upheader_new.RData")
save(Downheader_new, file="./ProcessedData/General_April2015/Downheader_new.RData")
save(Upsiglist, file="./ProcessedData/General_April2015/Upsiglist.RData")
save(UpsigID, file="./ProcessedData/General_April2015/UpsigID.RData")
save(DownsigID, file="./ProcessedData/General_April2015/DownsigID.RData")

save(Upobjout, file="./ProcessedData/General_April2015/Upobjout.RData")
save(Downobjout, file="./ProcessedData/General_April2015/Downobjout.RData")
save(WIM_SIG_pair, file="C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/WIM_SIG_pair.RData")

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


rm(bufferaspacing12, bufferaspacing23, bufferaspacing34, bufferaspacing45,
   bufferaweightl1,bufferaweightl2,bufferaweightl3,bufferaweightl4,bufferaweightl5,
   bufferaweightr1,bufferaweightr2,bufferaweightr3,bufferaweightr4,bufferaweightr5,
   bufferduration, buffergvw, bufferlen, buffernumpnt, buffertimewindow)

rm(bufferaspacing56,bufferaspacing67, bufferaspacing78, bufferaspacing89)
rm( bufferaweightl6, bufferaweightl7, bufferaweightl8, bufferaweightl9,
    bufferaweightr6, bufferaweightr7, bufferaweightr8, bufferaweightr9)

rm(x,y,i,j, len, splineUp, inUpsig, inDownsig)

save.image("C:/Users/Kate Hyun/Dropbox/Kate/ReID/TruckReid/ProcessedData/General_April2015/07032015.RData")
