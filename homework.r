library(mvtnorm)

simcor <- function(ncor, nnor, rho){
    if (ncor < 1 | nnor < 2 | rho < -1 | rho > 1) return(NA)
    library(mvtnorm)
    vm <- matrix(c(1,rho,rho,1),2,2)
    result <- NULL
    for (i in c(1:ncor)){
        norv <- rmvnorm(nnor,mean = c(0,0), sigma= vm)
        sc <- cor(norv)[1,2]
        result <- c(result,sc)
    }
    return(result)
}

x=(-100:100)/10

l= dnorm(x)
plot(x,l)