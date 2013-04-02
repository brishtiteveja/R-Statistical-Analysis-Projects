%練習問題2.2.3
A <- matrix(1:100,100,100)
B <- t(A)
C <- 1 / (A + B)
D <- C %*% C
E <- sum(D)
Answer <- 3/2 * E
Answer
% Answer is 341.9553



