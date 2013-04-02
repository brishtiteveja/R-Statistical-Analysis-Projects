#8.1.1


#8.1.2
hinv<-function(y){
 if(y>=1/2){
  return (log(1/(2-2*y)))
 }else{
  return (log(2*y))
 }
}

rhdis<-function(n){
 x<-runif(n)
 x<-sapply(x,hinv)
 return (x)
}

x<-rhdis(300)
hist(x,breaks=20)


#8.1.3
c<-sqrt(2*exp(1)/pi)

hinv<-function(y){
 if(y>=1/2){
  return (log(1/(2-2*y)))
 }else{
  return (log(2*y))
 }
}


h<-function(x) exp(-abs(x))/2
g<-function(x) exp(-x^2/2)/sqrt(2*pi)

rhone<-function(x){
 x<-sapply(x,hinv)
 return (x)
}

rnormd<-function(n){
 x<-runif(n)
 u<-runif(n)

 x<-sapply(x,hinv)

 for(i in 1:n){
  while(c*h(x[i])*u[i]>g(x[i])){
   x[i]<-rhone(runif(1))
   u[i]<-runif(1)
  }
 }
 return (x)
}

x<-rnormd(10000)
h<-hist(x,breaks=100,freq=FALSE)

z<-seq(-3,3,0.01)
gs<-sapply(z,g)
par(new=T)
plot(z,gs,xlim=range(h$breaks),ylim=c(0,max(h$density)),type="l")








#8.2
s<-0.5
S<-4*(pnorm(1,mean=0,sd=1)-pnorm(-0.5,mean=0,sd=1))/3

g<-function(x){
 if(x[1]<=-15){ return (0)
 }else if(x[2]<=-15){ return(0)
 }else{ return (exp(-2*(x[1]*x[1]+x[2]*x[2]-x[1]*x[2])/3)/(pi*sqrt(3)))
 }
}

nextX<-function(x){
 err<-2*s*runif(2)-s
 v<-x+err
 a<-min(1,(g(v)/g(x)) )
 choice<-runif(1)

 if(choice < a) return (v)
 else return (x)
}

my.mvrnorm2<-function(n){
 x<-matrix(c(rep(1:n),rep(1:n)),2,n)
 x[1,1]=-4
 x[2,1]=4
 for(i in 1:(n-1)){
  tmp<-c(x[1,i],x[2,i])
  ret<-nextX(tmp)
  x[1,i+1]<-ret[1]
  x[2,i+1]<-ret[2]
 }
 plot(x[1,],x[2,],pch=16,cex=0.3)

 return (0) #void
}

my.mvrnorm2(10000)




















#8.3

nextX<-function(x,rho){
xt<-c(0,0) #nextX
det<-runif(1)
 if(det>=0.5){ #x[1]:fixed
  z[1]<-x[1]
  z[2]<-rnorm(1,rho*x[1],sqrt(1-rho*rho))
 }else{ #x[2]:fixed
  z[1]<-rnorm(1,rho*x[2],sqrt(1-rho*rho))
  z[2]<-x[2]
 }
return (z)
}

my.mvtrnorm.gibbs2<-function(n,rho){
 x<-matrix(c(rep(1:n),rep(1:n)),2,n)
 x[1,1]=5
 x[2,1]=5
 for(i in 1:(n-1)){
  tmp<-c(x[1,i],x[2,i])
  ret<-nextX(tmp,rho)
  x[1,i+1]<-ret[1]
  x[2,i+1]<-ret[2]
 }
 plot(x[1,],x[2,],pch=16,cex=0.3)

 return (0) #void
}

my.mvtrnorm.gibbs2(10000,0.99)


