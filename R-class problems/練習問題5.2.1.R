c = 1/(1-pnorm(1/2))
my.dexp <- function(x){
  	t <- c / sqrt(2*pi)* exp(-x^2/2)
  	return(t)
}
#n個の乱数を発生させるプログラム
my.rtnorm <- function(n){
	x<-NULL
	for(i in 1:n){	
		gx <- runif(1)
		fx <- (gx/c + pnorm(1/2))
		x <- append(x,qnorm(fx))
	}
	return(x)
}

#histogram
x <-my.rtnorm(10000)
h <- hist(x,freq=FALSE,breaks = 100)

#density function
x0 <- seq(min(h$breaks),max(h$breaks),length.out = 1000)
y0 <- sapply(x0,my.dexp)
par(new = T)
plot(x0,y0,xlim=range(h$breaks),ylim=c(0,max(h$density)),type="l",ann=F,axes=F,lwd=2,col="red") 
