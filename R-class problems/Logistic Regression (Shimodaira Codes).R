load(url("http://www.keihirose.com/spam.Rdata"))

data.training <- as.data.frame(cbind(X.train,spam.train))

fit.glm <- glm(spam.train~.,data=data.training,family=binomial())

class(fit.glm)

summary(fit.glm)

#推定されたパラメータ
coef(fit.glm)

#P(y=1)の予測値
yhat<-predict(fit.glm,as.data.frame(X.test),type="response")

#ロジスティック関数
eta <- predict(fit.glm,as.data.frame(X.test))
plot(eta,yhat,xlim=c(-5,5))
x = seq(-6,6,length=101)
lines(x,1/(1+exp(-x)),col = "red",lwd=3)
points(eta,spam.test,pch=2,col="green")


:::::::Data Generation

x11()

#乱数で例題用データの生成
truebeta = c(-4,10)
n = 100
x = runif(n)
g = function(x) 1 / (1+exp(-x))
trueprob = g(truebeta[1] + truebeta[2] * x)
y = (runif(n) < trueprob ) + 0
y

data.train = as.data.frame(cbind(x,y))
class(data)
#glm使ってロジスティック回帰
kekka = glm(y~.,data=data.train,family=binomial())
#Maximum likelihood estimate (最尤推定)
kekka$coef #(glmで計算した最尤推定量)

#Data plot
plot(x,y)
points(x,trueprob,col="green")
beta = kekka$coef
points(x,g(beta[1]+beta[2]*x),col="red")  # p(y=1|x)を赤でプロット(パラメータ最尤推定量)


#maximum likelihood function -log L(beta | data)
mylik = function(beta){
	prb = g(beta[1] + beta[2] * x)
	-sum(y*log(prb) + (1-y) * log(1-prb))
}

#等高線プロット
x11()
x0 = seq(-10,10,length=50)
y0 = seq(0,20,length=50)
a = expand.grid(x0,y0) # grid of x,y

#グリッド上の-log L の値
z = matrix(apply(a,1,mylik),50,50) 
z
image(x0,y0,z,col = terrain.colors(100),xlab = "beta0" , ylab = "beta1")
contour(x0,y0,z,add=T,nlevels = 20)
points(0,0,lwd = 2,pch=1)
beta[1]
beta[2]
points(beta[1],beta[2],lwd = 10,pch=1)
points(truebeta[1],truebeta[2],lwd = 2,pch = 2)



#Using optim to maximize maximum likelihood log L
ans_optim = optim(c(0,0),mylik,method = "BFGS",hessian = TRUE,control = list(trace=1))

#result of optim function
newbeta = ans_optim$par
newbeta
sd = sqrt(diag(solve(ans_optim$hessian)))
cbind(beta,sd,beta/sd,2*pnorm(abs(newbeta/sd),lower = F))
points(newbeta[1],newbeta[2],lwd = 10,pch=1,col="red")