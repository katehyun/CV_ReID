
install.packages("ggplot2")
library(ggplot2)
p <- rbeta( 10000, (3.4+15), (23+85) )

ggplot( data.frame(p), aes(x=p) ) +
  geom_histogram( aes(y=..density..), binwidth=.005, color="#000000", fill="#FFFFFF" ) +
  geom_density( alpha=.2, fill="#66FF66" ) +
  ylab("Density") +
  ggtitle("Approximate distribution for p")


# example
ggplot(data.frame(Diff.lengthdiff_m), aes(x=Diff.lengthdiff_m))  + 
  geom_histogram( aes(y=..density..), binwidth=1, color="#000000", fill="#FFFFFF" ) +
  geom_density( alpha=.2, fill="#66FF66" ) +
  ylab("Density") +
  ggtitle("Approximate distribution for length_matching") 




k <- list(c(1,2,3,4,5),c(1,2,3,4),c(1,3,6,8,14),c(1,3,7,8,10,37))
d <- data.frame(x = unlist(k), grp = rep(letters[1:length(k)], times = sapply(k,length)))
ggplot(d, aes(x = d)) + 
  geom_histogram( aes(y=..density..), binwidth=1, color="#000000", fill="#FFFFFF" ) +
  geom_density( alpha=.2, fill="#66FF66" ) +
  ylab("Density") +
  ggtitle("Approximate distribution for length_matching") 
ggplot(d,aes(x = grp, y = x)) + geom_boxplot()

vec1 <- data.frame(x=Diff.lengthdiff_m)
vec2 <- data.frame(x=Diff.lengthdiff_nm)
ggplot() + geom_density(aes(x=x), colour="red", data=vec1) + 
  geom_density(aes(x=x), colour="blue", data=vec2)

library(plyr)
d3 <- ldply(list(d1 = vec1, d2 = vec2))


ggplot(data.frame(d3), aes(x=d3))  + 
  geom_histogram( aes(y=..density..), binwidth=1, color="#000000", fill="#FFFFFF" ) +
  geom_density( alpha=.2, fill="#6666FF" ) +
  ylab("Density") +
  ggtitle("Approximate distribution for length_nonmatching")




p_1 <- p

a_2 <- 2
b_2 <- 21
p_2 <- rbeta( 10000, (a_2+10), (b_2+140) )

ggplot( data.frame(p_1, p_2) ) +
  geom_histogram( aes(x=p_1, y=..density..), binwidth=.005, color="#000000", fill="#FFFFFF" ) +
  geom_density( aes(x=p_1), alpha=.2, fill="#66FF66" ) +
  geom_histogram( aes(x=p_2, y=..density..), binwidth=.005, color="#000000", fill="#FFFFFF" ) +
  geom_density( aes(x=p_2), alpha=.2, fill="#6666FF" ) +
  xlab( expression( paste( p[1], ", ", p[2] ) ) ) +
  ylab( "Density" ) +
  scale_x_continuous( breaks=c(0,.025,.05, .075, .1, .125, .15, .175, .2, .225, .25, .275) ) +
  ggtitle( expression( paste( "Approximate distributions for ", p[1], ", ", p[2] ) ) )

quantile( p_2, probs=c(0.025, 0.5, 0.975) )
mean(p_2)
sqrt( var(p_2) )


s <- (0:1000)/300
z_1 <- dgamma(s,1,1)
z_2 <- dgamma(s,1,2)
z_3 <- dgamma(s,1,0.5)
z_4 <- dgamma(s,2,1)
z_5 <- dgamma(s,2,2)

ggplot( data.frame(s, z_1, z_2, z_3, z_4, z_5) ) +
  layer( mapping=aes(x=s, y=z_1), geom="line" ) +
  layer( mapping=aes(x=s, y=z_2), geom="line", geom_params=list(color="red") ) +
  layer( mapping=aes(x=s, y=z_3), geom="line", geom_params=list(color="blue") ) +
  layer( mapping=aes(x=s, y=z_4), geom="line", geom_params=list(color="darkgreen") ) +
  layer( mapping=aes(x=s, y=z_5), geom="line", geom_params=list(color="purple") ) +
  ylab("Density") +
  ggtitle("Approximate distribution for various Gammas")