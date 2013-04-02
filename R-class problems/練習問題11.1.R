load(url("http://www.keihirose.com/spam.Rdata"))

#問題11.1.1
data.training <- as.data.frame(cbind(X.train,spam.train))
class(spam.train~.)
fit.glm <- glm(spam.train~.,data = data.training, family = binomial())
class(fit.glm)
summary(fit.glm)
coef(fit.glm)

#問題11.1.2
?optim
?glm
optim(spam.train~.)
predict(fit.glm,as.data.frame(X.train),type = "response")


