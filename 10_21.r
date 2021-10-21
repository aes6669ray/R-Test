hu <- read.table("housing.txt", header = TRUE, row.names = 1)

# std_apt <- (hu[,1]-mean(hu[,1]))/sd(hu[,1])
# std_hou <- (hu[,2]-mean(hu[,2]))/sd(hu[,2])

# dta <- cbind(hu, std_apt,std_hou)

# print(sapply(dta,sd))


test <- function(x){
    q<-c()
    for (i in c(1:x)){
    a <- seq(1:i)
    k <- append(q, a)
    q<-k
    }
    return(q)
}


a<-test(8)
print(a)


