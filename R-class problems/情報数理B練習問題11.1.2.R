#練習問題11.1.2
load(url("http://www.keihirose.com/spam.Rdata"))
data.training <- as.data.frame(cbind(X.train,spam.train))

#Using general linear model
fit.glm <-glm(spam.train~.,data = data.training,family=binomial())

#Estimated parameters
summary(fit.glm)

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
    names(theta.start) = c("Intercept",colnames(x))

    # Allow Intercept Estimation
    X = cbind(1,x)
    colnames(X)[1] = "Intercept"

    # Calculate the maximum likelihood
    mle = optim(theta.start,logl,x=X,y=y,hessian=F)

    # Obtain regression coefficients
    beta = mle$par

    # Calculate the Information matrix
    # The variance of a Bernouilli distribution is given by p(1-p)
    p = 1/(1+exp(-X%*%beta))
    V = array(0,dim=c(dim(X)[1],dim(X)[1]))
    diag(V) = p*(1-p)
    IB = t(X)%*%V%*%X

    # Return estimates
    out = list(beta=beta,vcov=solve(IB),dev=2*mle$value)
    outf = cbind(out$beta,out$dev,lower=F)
    return(outf)
}

#Printing Estimated parameters
mle.logreg(X.train,spam.train)



