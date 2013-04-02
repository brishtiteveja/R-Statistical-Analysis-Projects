#練習問題12.1.2
Y.train = Y

#テスト用のデータの中心化
X.test2 = scale(X.test,center=T,scale=F)

#主成分計算
Y.test = X.test2 %*% V  
dim(Y.test)

p = 53
avg = c(1:p)

for(i in 1:p){
	data.training <- as.data.frame(cbind(Y.train[,1:i],spam.train))
	fit.glm <-glm(spam.train~.,data=data.training,family=binomial())
	
	#P(y=1)の予測値
	yhat<-predict(fit.glm,as.data.frame(cbind(Y.test[,1:i])),type="response")
	
	#スパーム判別 　　
	Spam = yhat > 0.5
	
	#誤判定率
	avg[i] = mean(Spam != spam.test) * 100
	
}
avg


#Optim function を用いて
#練習問題11.1.2


#Logistic Regression Function
mle.logreg = function(x,y)
{
    # Define the negative log likelihood function
    logl <- function(theta,x,y){
      y <- y
      x <- as.matrix(x)
      beta <- theta[1:ncol(x)]

      # Use the log-likelihood of the Bernouilli distribution
      g = function(x) 1/(1 + exp(-x))
     prb = g(x%*% beta)
     loglik <- sum(-y*log(prb) - (1-y) * log(1-prb))
      return(-loglik)
    }

    # Prepare the data
    x = as.matrix(x)
    y = as.matrix(y)

    # Define initial values for the parameters
    theta.start = rep(0,(dim(x)[2]+1))
  #  names(theta.start) = c("Intercept",colnames(x))

    # Allow Intercept Estimation
    X = cbind(1,x)
   # colnames(X)[1] = "Intercept"

    # Calculate the maximum likelihood
    mle = optim(theta.start,logl,x=X,y=y,hessian=F)

    # Obtain regression coefficients
    beta = mle$par


    # Return estimates
    out = list(beta=beta)
    return(out)
}

#Printing Estimated parameters
optimBeta<- mle.logreg(Y.train,spam.train)


optimBeta

