#練習問題11.1.1

load(url("http://www.keihirose.com/spam.Rdata"))

data.training <- as.data.frame(cbind(X.train,spam.train))

fit.glm <- glm(spam.train~.,data=data.training,family=binomial())

class(fit.glm)

summary(fit.glm)

#推定されたパラメータ
coef(fit.glm)

#P(y=1)の予測値
yhat<-predict(fit.glm,as.data.frame(X.test),type="response")

＃答えと予測値　最小の２０個
round(rbind(spam.test[1:20],yhat[1:20]),3)

#P(y==1)の予測値とyの観測値
plot(yhat,spam.test)

#スパーム判別 　　
Spam = yhat > 0.5

table(Spam,spam.test)

abline(v = 0.5,col ="red")

#誤判定率
mean(Spam != spam.test)
