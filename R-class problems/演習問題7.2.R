N = 100


x1<-numeric(N)
x2<-numeric(N)

x1[0] = 1
x2[0] = 2

myfunc<-function(x){
	mu <-c(0,0)
	sigma <- matrix(c(1,0.5,0.5,1),2,2)
	D<-det(sigma)
	E<-t(x-mu)%*%solve(sigma)%*%(x-mu)
	f <- 1/(sqrt(2*pi)*D) *exp(-0.5E)
	return(f)	
}

#X1|x2
h1 <- function(x2){
	mu=c(0,0)
	sigma=c(1,1)
	rho=0.5
	rnorm(1,mean = mu[1] + (sigma[1]/sigma[2])*rho*(x2-mu[2]), sd = sigma[1]^2*(1-rho^2))
}

h2 <- function(x2){
	mu=c(0,0)
	sigma=c(1,1)
	rho=0.5
	rnorm(1,mean = mu[2] + (sigma[2]/sigma[1])*rho*(x1-mu[1]), sd = sigma[2]^2*(1-rho^2))
}

for(i in 1:7){
	
}