#library(mvtnorm)

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

for (i in seq(-0.6,-0.7,by=-0.01)){
    k <- simcor(ncor=10000,nnor=15,rho=i)

    j= k[k > 0.5]
    g= k[k < -0.5]
    p=c(j,g)

    if((length(p)/10000)>0.8) {
         break 
    } 
    cat("power lower than 80%",i,"\n")
}

# k <- simcor(ncor=10000,nnor=15,rho=0.7)

# j= k[k > 0.5]
# g= k[k < -0.5]
# p=c(j,g)

# if((length(p)/10000)>0.8){
#     print("power greater than 80%")
# }else {
#    print("power lower than 80%")
# }


# n=seq(-1,1,by=(0.1))
# print(n)

