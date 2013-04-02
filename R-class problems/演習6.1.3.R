my.mvrnorm<-function(n){
	y1<-rnorm(n)
	y2<-rnorm(n)
	x1<-y1
	x2<-0.5 * y1 + sqrt(3)/2 * y2
	return(data.frame(x1,x2))
}

y<-my.mvrnorm(100000)
plot(y,pch=16,cex=0.3)

Sigma = matrix(c(1,0.5,0.5,1),2,2)
A= t(chol(Sigma))

A%*%t(A)