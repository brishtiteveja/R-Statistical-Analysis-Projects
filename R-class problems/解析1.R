
dat = read.table("/Users/zehadyzbdullahkhan/Documents/Class/学年３学期２/計算数理B/データ解析/gakurekishushou.txt")
kekka = lm(Shushou~Gakureki,dat)
summary(kekka)
dim(dat)
plot(dat)
abline(kekka,col = "red")
kekka

x = dat[,1]
y = dat[,2]

x
mean(x)
a = x - mean(x)
b = y - mean(y)
beta1 = sum(a*b) / sum(a*a)
beta0 = mean(y) - beta1*mean(x)

#ボストン市の住宅価格データ
dat2 = read.table("/Users/zehadyzbdullahkhan/Documents/Class/学年３学期２/計算数理B/データ解析/Boston.dat.txt")

a = dat2[1,]
names("a","b")
colnames(dat2) = c("Crim","Zn","Indus","Chas","Nox2","Rm2","Age","Dis","Rad","Tax","Ptratic","S","Lstat","LogCmedv")
colnames(dat2)
dim(dat2)
dat2 = dat2[2:507,]
dat2
kekka2 = lm(LogCmedv ~ .,data = dat2)
dat2[1,]
