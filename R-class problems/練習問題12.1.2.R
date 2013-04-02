load(url('http://www.keihirose.com/spam200.Rdata'))
X.test[1,]
nrow(X.test)
ncol(X.test)


#各変数を平均0と分散1にしておく
X = scale(X.train)
dim(X)
X[1:5,]
apply(X,2,mean)
var(X)[1:5,1:5]

X[]

#Variance Covariance matrix
Sigma = var(X)
Sigma[1:5,1:5]
#test
n = nrow(X)
n
1/(n-1) * (t(X)%*%X)[1:5,1:5]

#Eigen Value Lambda, Eigen Vector Matrix V
a = eigen(Sigma)

lambda =a$values
lambda
V = a$vectors
V
dim(V)

rownames(V) = colnames(X)
X[1:5,1:5]
V[1:5,1:5]

names(lambda) = paste("PC",seq(along=lambda),rep="")

colnames(V) = names(lambda)

?cumsum
cumsum(lambda)/sum(lambda)


V[,3:3]

lambda = diag(lambda)
lambda
lambda[1:5,1:5]

#confirmation of eigen values and eigen vectors

cbind(Sigma %*% V[,1],lambda[1]*V[,1])

round(t(V) %*% V, 10)
round(V %*% t(V),10)[1:5,1:5]

#Diagonalization and Spector decomposition
round(t(V) %*% Sigma %*% V, 10)

(V %*% lambda %*% t(V))[1:5,1:5]

Sigma[1:5,1:5]


#Calculating principle co-efficient
Y = X %*% V
dim(Y)
Y[1:5,]

#
variance=round(var(Y),10)


#Plotting eigen vector
plot(V[,c(1,2)],type="n",main="V") #1st and 2nd column of variance covariance matrix V
text(V[,c(1,2)],rownames(V),cex=0.5)
?plot
V[,c(1,2)]



biplot(Y[,c(1,2)],V[,c(1,2)],cex = 0.5,main ="(Y,V)")
biplot(Y[,c(1,3)],V[,c(1,3)],cex = 0.5,main="(Y,V)")

sum(Y[200,1:2] * V["word_freq_",1:2])
cumContR= round(cumsum(lambda)/sum(lambda),3)


#biplot for principle component ananlysis
f = princomp(X.train,cor=T)
?princomp  #princomp : principle component analysis
biplot(f,cex = 0.5)
biplot(f,choi = c(1,3),cex = 0.5)
?biplot


#Singular value decomposition for X
Z = Y %*% diag(lambda^(-1/2))
Z
Z2 = (1/sqrt(n-1)) * Z
dim(Z2)
lambda2 = (n-1)*lambda
X2 = round(Z2 %*% diag(lambda2^(1/2)) %*% t(V),3)
dim(X2)
X2[1:5,1:5]
X2

a = svd(X)
names(a)
a$d
a$u
a$v

summary(a)
a

?svd

X3 = round(a$u %*% diag(a$d) %*% a$v,3)
X3
dim(a$u)
dim(a$d)
dim(a$v)
