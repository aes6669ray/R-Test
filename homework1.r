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

# for (i in seq(-0.6,-0.7,by=-0.01)){
#     k <- simcor(ncor=10000,nnor=15,rho=i)

#     j= k[k > 0.5]
#     g= k[k < -0.5]
#     p=c(j,g)

#     if((length(p)/10000)>0.8) {
#          break 
#     } 
#     cat("power is lower than 80%",i,"\n")
# }


# plot((1:150),type="l",ylim=c(1,-1))
# for (i in (2:150)){
#     j=simcor(ncor=1000,nnor=i,rho=0)
#     k=unname(quantile(j,c(0.025,0.975)))
#     lines(c(i,i),c(k[1],k[2]))
# }

# x = Inf
# y= 1+x
# cat("1+x=",y,"\n")
# z=c(1,2)
# cat("Add Inf in vector:",z+x,"\n")

x=c(1,NA,NaN,2)
print(x)
y=NULL
print(y)

j=c(1,NaN,NA)
k=c(1,NA,NaN)

cat("length of J",length(j),"\n")
cat("length of K",length(k),"\n")

cat("sum of j",sum(j),"\n")
cat("sum of k",sum(k),"\n")


# a=c(NaN,NA,Inf,0,1)
# b=NULL
# for (i in a){
#     for (j in a){
#       b=c(b,i*j)  
#     }
# }

# t=matrix(b,5,5)
# k=data.frame(t,row.names = c("NaN","NA","Inf",0,1))
# names(k) = a
# print(k)


# AA=NULL*NaN
# BB=NULL+NaN
# CC=NULL*NA
# DD=NULL+NA
# EE=NULL*Inf
# FF=NULL+Inf
# GG=NULL*0
# HH=NULL+0
# ALL=c(AA,BB,CC,DD,EE,FF,GG,HH)
# print(is.numeric(HH))

# x=c(Inf,1)
# print(length(x))