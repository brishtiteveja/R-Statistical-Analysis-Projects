#5.1

#5.1.1

f <- function(x){
  	t <- 1/(2*pi) * exp(-x^2/2)
  	return(t)
  }

g <-function(x){
  	if(abs(x) < 1){
  		t <- -1 *(1-exp(-1/2))*x^2 + 1
  	}else if(abs(x) >= 1 && abs(x) < 2){
  		t <- -1*(exp(-1/2) - exp(-2))*abs(x) + 2 * exp(-1/2) - exp(-2)
  	}else if(abs(x) <= 2 && abs(x) < 3){
  		t <- -1*(exp(-2) - exp(-9/2))*abs(x) + 3 * exp(-2) - 2*exp(-9/2)
  	}else if(abs(x) >= 3){
  		t <- exp(-3)*exp(-abs(x) /2)
  	}  	
  	return(t)
 }
  
x <- seq(-10,10,0.01)
fx <- sapply(x,f)
gx <- sapply(x,g)
plot(x,fx,type="l")
par(new = "T")
plot(x,gx)
