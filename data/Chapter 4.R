##########################################################

#  Chapter 4: Some Linear Algebra

##########################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

#########################################################

#  Section  4.4

#########################################################


x <- seq(2, 8, 2)
x
y <- 4 : 1
y
x + y
crossprod(x, y)
crossprod(x)

x * y

x

x %o% x

(x %o% x)[, 3]

(x %o% x)[, 2]

diag(1, 4, 4)

matrix(1, 4, 4)

matrix(1 : 16, 4, 4)

t(matrix(1 : 16, 4, 4))

diag( t(matrix(1 : 16, 4, 4)))

A
B
A + B
A * B

A
t(B)
A %*% t(B)


(b <- c(9, 7 5, 3)
(A <- matrix(runif(16), c(4, 4)))
solve(A, b)
solve(A)


###############################################################

#  Section 4.5

###############################################################

#####################################  Section 4.5.1:  Determinants

(x <- matrix(rnorm(25), 5, 5))
det(x)

################################  Section 4.5.2:  Matrix inversion

x
solve(x)
x %*% solve(x)

b %*%  solve(A, b)

######################  Section 4.5.3: Eigenvalues and eigenvectors

evs <- eigen (A)

eigen(A, only.values = TRUE)$values

eigen(x)$values

prod(eigen(x)$values)
det(x)

##################  Section 4.5.4: diagonizable matrices

(a <- matrix(rnorm(15), 5, 3))  # 5x3 matrix with random entries

(apa <- t(a) %*% a)        # a'a matrix

(ax <- eigen(apa))   # eigenvalues and eigenvector as a matrix

ax$vectors %*% t(ax$vectors)

ax$vectors %*% diag(ax$values) %*% t(ax$vectors)

###############  4.5.5  Generalized inverses

library(MASS)
(z <- matrix(rnorm(15), 3, 5))
(q <- t(z) %*% z)

eigen(q)$values
ginv(q)

q %*% ginv(q) %*% q  - q

#################  4.5.6  Matrix Square Root

msqrt <- function(a)   # matrix square root of positive definite matrix
{
  a.eig <- eigen(a)
  if ( min(a.eig$values) < 0) # check for positive definite
    warning("Matrix not positive definite")
  
  a.eig$vectors %*% diag(sqrt(a.eig$values)) %*% t(a.eig$vectors)
}

#########################################################

(x <- matrix(rnorm(35), 7, 5)) # 7x5 matrix of normals

(z <-  t(x) %*% x)    # pos definite, symmetric 5x5 matrix

eigen(z)$values       # check positive definite

(sqrtz <- msqrt(z))   # find matrix square root

sqrtz %*% sqrtz - z   # check properties of matrix sqrt

################################################################
###############  End of this file ##############################
################################################################
