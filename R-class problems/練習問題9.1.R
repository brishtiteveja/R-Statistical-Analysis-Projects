myprior<-function(N,k,p,q){
	mypriorprep <- function(theta){
		(theta^(p-1) * (1-theta)^(q-1)) / beta(p,q)
	}
	plot(mypriorprep,0,1)
}

myposterior<-function(N,k,p,q){
	myposteriorprep <- function(theta){
		(theta^(k+p-1) * (1-theta)^(N-k+q-1)) / beta(k+p,N-k+q)
	}
	plot(myposteriorprep,0,1)
}

myprior(20,10,1,1)
par(new="T")
myposterior(20,10,1,1)
par(new="T")
myprior(20,10,6,4)
par(new="T")
myposterior(20,10,6,4)
par(new="T")
myprior(20,10,60,40)
par(new="T")
myposterior(20,10,60,40)