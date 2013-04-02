#練習問題2.3
A <- rbind(c(1.0,0.5,0.2),c(0.5,1.0,0.5),c(0.2,0.5,1.0))

# Aの対角化行列はYである
Y <- matrix(eigen(A)$vectors,3,3)  
#このYは直行行列である。なぜかというと、 Y * t(Y) = IdentityMatrix(3) であることがたしかめた

round(Y%*%solve(Y),2)
# 結果
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

Z <- diag(eigen(A)$values)
#対角に固有値を持つ行列

H1 <- rbind(c(1,0,0),c(0,1,0),c(0,0,1))
H2 <- rbind(c(-1,0,0),c(0,1,0),c(0,0,1))
H3 <- rbind(c(1,0,0),c(0,-1,0),c(0,0,1))
H4 <- rbind(c(-1,0,0),c(0,-1,0),c(0,0,1))
H5 <- rbind(c(1,0,0),c(0,1,0),c(0,0,-1))
H7 <- rbind(c(-1,0,0),c(0,1,0),c(0,0,-1))
H6 <- rbind(c(1,0,0),c(0,-1,0),c(0,0,-1))
H8 <- rbind(c(-1,0,0),c(0,-1,0),c(0,0,-1))

W <- Z^(1/4)

W1 <- H1 %*% W
W2 <- H2 %*% W
W3 <- H3 %*% W
W4 <- H4 %*% W
W5 <- H5 %*% W
W6 <- H6 %*% W
W7 <- H7 %*% W
W8 <- H8 %*% W


B1 <- round(Y %*% W1 %*% t(Y),5)
B2 <- round(Y %*% W2 %*% t(Y),5)
B3 <- round(Y %*% W3 %*% t(Y),5)
B4 <- round(Y %*% W4 %*% t(Y),5)
B5 <- round(Y %*% W5 %*% t(Y),5)
B6 <- round(Y %*% W6 %*% t(Y),5)
B7 <- round(Y %*% W7 %*% t(Y),5)
B8 <- round(Y %*% W8 %*% t(Y),5)

B1
B2
B3
B4
B5
B6
B7
B8

#確認
ans1 <- round(B1 %*% B1 %*% B1 %*% B1,4)
ans2 <- round(B2 %*% B2 %*% B2 %*% B2,4)
ans3 <- round(B3 %*% B3 %*% B3 %*% B3,4)
ans4 <- round(B4 %*% B4 %*% B4 %*% B4,4)
ans5 <- round(B5 %*% B5 %*% B5 %*% B5,4)
ans6 <- round(B6 %*% B6 %*% B6 %*% B6,4)
ans7 <- round(B7 %*% B7 %*% B7 %*% B7,4)
ans8 <- round(B8 %*% B8 %*% B8 %*% B8,4)

ans1
ans2
ans3
ans4
ans5
ans6
ans7
ans8
# B^4 = A になった