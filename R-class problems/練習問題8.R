#練習問題8.1
#8.1.2

inverseOfH<-function(y){
		if(y>=1/2)
			return(-log(2-2*y))
		else
			return(log(2*y))
}

genRandom<-function(n){
	x <- runif(n)
	y <- sapply(x,inverseOfH)
	return(y)
}
x<-genRandom(400)
hist(x,breaks=20)

#8.1.3
inverseOfH<-function(y){
		if(y>=1/2)
			return(-log(2-2*y))
		else
			return(log(2*y))
}

genRandom<-function(n){
	x <- runif(n)
	y <- sapply(x,inverseOfH)
	return(y)
}

g <- function(x){
	y <- 1/sqrt(2*pi) * exp(-x^2/2)
	return (y)
}

h <- function(x){
	y <- 1/2* exp(-abs(x))
	return(y)
}
n=10000

u1 <- 2* runif(n) # U1 ~ U(-1,1)
u <- abs(u1)  # U~ U(0,1)
s <- sign(u1)
v <- genRandom(n)  # 逆関数法を使ってv (V ~ h(v)) を生成
c = sqrt(2 * exp(1)/pi)
a = c*h(v)*u <= g(v)
rbind(u,v,g(v),a)[,1:5]
x = v[a]
x
length(x)

#histogram
h <- hist(x,freq=FALSE,breaks=100)

#density funftion
x0 <- seq(min(h$breaks),max(h$breaks),length.out =1000)
y0 <- sapply(x0,dnorm)
par(new = T)
plot(x0,y0,xlim=range(h$breaks),ylim=c(0,max(h$density)),type="l",ann=F,axes=F,lwd=2,col="red") 

#練習問題8.2
mu=c(0,0)
sigma = matrix(c(1,1/2,1/2,1),2,2)
nextX <-function(x){
	eps <- runif(2,-s,s)
	v<- x + eps
	g_v <- dmvnorm(v,mu,sigma)
	g_x <- dmvnorm(x,mu,sigma)
	a<- min(1,g_v/g_x)
	c <-runif(1)
	
	if(c<a)
		return(v)
	else
		return(x)
}
my.mvrnorm2<-function(N,s){
	x <- matrix(c(rep(1:N)),2,N)
	x[1,1] = -4
	x[2,1] = -4
	
	for(i in 1:(N-1)){
		tmp<- c(x[1,i],x[2,i])
		retVal<- nextX(tmp)
		x[1,i+1] <- retVal[1]
		x[2,i+1] <- retVal[2]
	}
	plot(x[1,],x[2,],pch=16,cex=0.3)
}
y<-my.mvrnorm2(1000,0.5)
?dmvnorm
