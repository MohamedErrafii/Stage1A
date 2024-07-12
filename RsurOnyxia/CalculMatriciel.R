#Produit Matriciel en R
X <- matrix(1:10, 5, byrow = False)
Y <- 2:6

dim(X)
t(X)
X %*% t(X)
Xp
X*X

solve(matrix(c(1,5,13,6),2))
det(W)

#résoudre syst lin 

2x + 3y = 1 
3x + 5y = -1

A <- matrix(c(2,3,3,5), nrow = 2, byrow = TRUE)
B <- matrix(c(1,-1), nrow = 2)

solve(A,B)

#On a résolu l'équation AX = B
qr(A)
qr(A)$rank

#tirer un échantillion aléatoire entre homme-femme 

x<- sample(c("H","F"), size = 100, replace = TRUE, prob =c(0.1, 0.9) )
mean(x == "F")
