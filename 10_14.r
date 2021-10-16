# a <- c(seq(0, 1, 0.1), seq(2, 10))

# t <- 500
# x <- pnorm(t)
# g=sum(.5<x^2 & x^2<1.2)/t
# print(g)

# par(mfrow=c(3,1))

# x= (30:-30)/ 10 
# y= (0:100)/ 100


# plot(y,qnorm(y),type="l")
# plot(x,dnorm(x),type="l") 
# plot(x,pnorm(x),type="l")

#x <- rf(100000, df1 = 3, df2 = 25)
#hist(x, breaks = 'Scott', freq = FALSE, xlim = c(0,10))

# a= seq(0,10,0.001)
# plot(a, df(a,df1=3,df2=25),type="l")

hu <- read.table("housing.txt", header = TRUE, row.names = 1)

a <- log(hu$Apartment[1:51])

lu <- cbind(hu, a)
print(lu)

#mydata<‐read.table(file="clipboard")