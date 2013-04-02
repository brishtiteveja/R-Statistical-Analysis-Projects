library(tmvtnorm)
my.mvtrnorm2<-function(N){
	x1<-numeric(N)
	x2<-numeric(N)
	v1<-numeric(N)
	v2<-numeric(N)
	eps1<-numeric(N)
	eps2<-numeric(N)
	a<-numeric(N)
	
	x1[1]<-0.5
	x2[1]<-0
	eps1[1]<-runif(1,-0.5,0.5)
	eps2[1]<-runif(1,-0.5,0.5)
	
	v1[1]<-x1[1]+eps1[1]
	v2[1]<-x2[1]+eps2[1]
	
	mu <-c(0,0)
	sigma <- matrix(c(1,0.5,0.5,1),2,2)
	lower<- c(-1.0,-0.5)
	upper<- c(Inf,Inf)
	
	v<-c(v1[1],v2[1])
	d1<-dtmvnorm(v,mu,sigma,lower,upper)
	x<-c(x1[1],x2[1])
	d2<-dtmvnorm(x,mu,sigma,lower,upper)
	a[1]<-min(1,d1/d2)
	
    	
	for(i in 2:N){
		k<-runif(1,0,1) 
		if(k<a[i-1]){　　#移動
			x1[i]<-v1[i-1]
			x2[i]<-v2[i-1]
		}else{
			x1[i]<-x1[i-1]
			x2[i]<-x2[i-1]
		}
		eps1[i]<-runif(1,-0.5,0.5)
		eps2[i]<-runif(1,-0.5,0.5)
		v1[i]<-x1[i]+eps1[i]
		v2[i]<-x2[i]+eps2[i]
		
		v<-c(v1[i],v2[i])
		d1=dtmvnorm(v,mu,sigma,lower,upper)
		x<-c(x1[i],x2[i])
		d2=dtmvnorm(x,mu,sigma,lower,upper)
		
		a[i] = min(1,d1/d2) 
	}
	return(data.frame(x1,x2))
}
y<-my.mvtrnorm2(100000)
plot(y,pch=16,cex=0.3)