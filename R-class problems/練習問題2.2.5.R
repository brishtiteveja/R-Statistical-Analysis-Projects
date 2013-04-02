A <- rbind(c(0.8,0.2),c(0.2,0.5))
eigen(A)
P <- matrix(eigen(A)$vectors,2,2)
D <- diag(eigen(A)$values)
invP <- solve(P)
Ans <- P %*% D^50 %*% invP
Ans