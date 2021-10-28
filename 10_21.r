
hu <- read.table("housing.txt", header = TRUE, row.names = 1)

std_apt <- (hu[,1]-mean(hu[,1]))/sd(hu[,1])
std_hou <- (hu[,2]-mean(hu[,2]))/sd(hu[,2])

dta <- cbind(hu, std_apt,std_hou)

print(sapply(dta,sd))


# test <- function(x){
#     q<-c()
#     for (i in c(1:x)){
#     a <- seq(1:i)
#     q <- append(q, a)
#     }
#     return(q)
# }


# a<-test(8)
# print(a)

# simcor <- function(ncor, nnor, rho){
#     if (ncor < 1 | nnor < 2 | rho < -1 | rho > 1) return(NA)
#     library(mvtnorm)
#     vm <- matrix(c(1,rho,rho,1),2,2)
#     result <- NULL
#     for (i in c(1:ncor)){
#         norv <- rmvnorm(nnor,mean = c(0,0), sigma= vm)
#         sc <- cor(norv)[1,2]
#         result <- c(result,sc)
#     }
#     return(result)
# }

# j <- simcor(ncor=1000,nnor=15,rho=0)
# print(quantile(j,c(0.025,0.975)))