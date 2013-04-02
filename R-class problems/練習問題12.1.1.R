#練習問題12.1.1

load(url('http://www.keihirose.com/spam200.Rdata'))
X.train[1,]
dim(X.train)

#centralizatiion
X = scale(X.train,center=T,scale=F)
dim(X)

#variance covariance matrix
Sigma = var(X)

#Eigen value, vector

a = eigen(Sigma)

V = a$vectors
V

#主成分
Y = X %*% V
dim(Y)


#(t=1,2,..,3601)3601個のデータに対する第１主成分
Y[,1]

#(t=1,2,..,3601)3601個のデータに対する第２主成分
Y[,2]

#主成分の解釈
  _ : subscript  ^: superscript
　y_tk = x^(t) v_k = Sigma_{i=1}^{i = p} (x_ti . v_ik )

which.max(V[,1])
21
which.min(V[,1])
25
which.max(abs(V[,1]))
sort(V[,1])


which.max(V[,2])
25
which.min(V[,2])
51

