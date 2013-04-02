
my.dexp <- function(x){
  	t <- 1/sqrt(2*pi) * exp(-x^2/2)
  	return(t)
}

my.rtnorm2 <- function(n){
	y1 <- NULL
	y2 <- NULL
	for(i in 1:n){
		x1 = runif(1)
		x2 = runif(1)
		y1 <- append(y,qnorm(x1))
		y2 <- append(y,qnorm(x2))
	}
	return(list(y1,y2))
}
#histogram
x <-my.rtnorm(10000)
h <- hist(x,freq=FALSE,breaks = 100)

#density function
x0 <- seq(min(h$breaks),max(h$breaks),length.out = 1000)
y0 <- sapply(x0,my.dexp)
par(new = T)
plot(x0,y0,xlim=range(h$breaks),ylim=c(0,max(h$density)),type="l",ann=F,axes=F,lwd=2,col="red") 

  