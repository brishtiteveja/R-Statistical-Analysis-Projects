#–â‘è11.1.1

load(url("http://www.keihirose.com/spam.Rdata"))

data.training <- as.data.frame(cbind(X.train,spam.train))

fit.glm <- glm(spam.train~.,data=data.training,family=binomial())

class(fit.glm)

summary(fit.glm)

yhat<-predict(fit.glm,as.data.frame(X.test),type="response")

round(rbind(spam.test[1:10],yhat[1:10]),3)

plot(yhat,spam.test)

spam = yhat > 0.5

table(spam,spam.test)

abline(v = 0.5,col ="red")

#–â‘è11.1.2

