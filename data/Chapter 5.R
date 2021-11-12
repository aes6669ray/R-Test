#############################################################

# R Programs and Figures in Chapter 5: Univariate Normal

##############################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

##############################################################

# Fig 5.1:  Normal density with different means

### pdf(file="normal1.pdf", height=4)  #  reduced height

x <- (0 : 400) * 3 / 100 - 6
plot (x, dnorm(x, mean = -2), ylim = c(0, .46),
      ylab = "Density", type = "l", cex.lab = 1.25, col = "blue")
lines(x, dnorm(x, mean = 0), type = "l", col = "blue")
lines(x, dnorm(x, mean = 2), type = "l", col = "blue")
text(c(-2, 0, 2), rep(.44, 3), cex = 1.25,
    c(expression(mu == -2), expression(mu == 0), 
      expression(mu == 2)))

### dev.off()

#################################################################

# Fig 5.2:  Normal densities with different standard deviations

### pdf(file = "normal2.pdf")

x <- (0 : 800) * 2 / 100 - 8
plot(x, dnorm(x, sd = .5), ylim = c(0, .82), col = "blue",
     ylab = "Density", type = "l", cex.lab = 1.5)
lines(x, dnorm(x, sd = 1), type = "l", col = "blue")
lines(x, dnorm(x, sd = 2), type = "l", col = "blue")
lines(x, dnorm(x, sd = 4), type = "l", col = "blue")
text(c(1.15, 1.65, 3, 5), c(.75, .25, .13, .1), 
     c(expression(sigma == .5), expression(sigma == 1),
       expression(sigma == 2),  expression(sigma == 4)),
     cex = 1.2)

### dev.off()


####################################################################

#  Fig 5.3: Cumulative normal distributions

### pdf(file = "normal3.pdf")

x <- (-100 : 100) * 4 / 100
plot(x, pnorm(x), type = "l", ylab = "Cumulative normal function(x)",
     cex.lab = 1.5, col = "blue")
lines(x, pnorm(x, mean = 1), type = "l", col = "blue")
lines(x, pnorm(x, mean = 0, sd = .5),type = "l", col = "blue")
text(c(-1.5, -1.5, .75,.75, 0, -.1), 
     c(.2, .15, .2, .15, .9, .85), cex = 1.5,
     c(expression(mu == 0), expression(sigma == 1), 
       expression(mu == 1), expression(sigma == 1), 
       expression(mu == 0), expression(sigma == .5) 
      ))

### dev.off()

################################################################


qnorm(.05 / 2)

qnorm(1 - .05 / 2)


################################################################

#   Figure 5.4: Normal inverse function

###  pdf(file = "qnorm.pdf")

p <- 1 : 99 / 100
plot(p, qnorm(p), type = "l", ylab = "Normal quantile(p)",
     cex.lab = 1.75, col = "blue")

###  dev.off()


#################################################################
#################################################################

#      Section 5.2: Transformations to Normality

#################################################################
#################################################################


#  Figure 5.5:  Histograms of six transformed apartment rents

### pdf(file = "transrent.pdf")

layout(mat=matrix(c(1, 3, 5, 2, 4, 6),  3, 2),
         rep(2, 6),  rep(2, 6))

housing <- read.table("housing.txt", 
                      header = TRUE, row.names = 1)

rent <- housing$Apartment
hist(rent,breaks=12,main = "Original values",
     ylab = "",xlab = "", col = "red")  #1
hist(sqrt(rent), breaks = 12, 
     main = "Square root",ylab = "",xlab = "", col = "red")     #2
hist(log(rent), breaks = 14, main = "Logarithm", ylab = "",
     xlab = "", col = "red")        #3
hist(-rent^(-1/2), breaks = 12,main = "-1/2 Exponent",
     ylab = "",xlab = "", col = "red") #4
hist(-1/rent,breaks=16,main="-Reciprocal",ylab="",xlab="", col="red")        #5
hist(-rent^(-1.5),breaks=16,main="-1.5 Exponent",ylab="",xlab="", col="red") #6

### dev.off()

################################################################

#   Fig 5.6:  QQplot of apartment rents

###  pdf(file="rentqq.pdf")

housing <- read.table("housing.txt", header=TRUE, row.names=1)
op <- layout(mat=matrix(c(1,0,2,0),2,2),c(2,2),c(2,2))
          #  side by side plots on one page

qqnorm(housing$Apartment, main="QQ plot of rents",
   pch=16, col="red", cex=1.25)
qqline(housing$Apartment, col="blue")

qqnorm(-housing$Apartment^(-1/2), main="Transformed values",
   pch=16, col="red", cex=1.25)
qqline(-housing$Apartment^(-1/2), col="blue")

par(op)

###  dev.off()


################################################################
################################################################

#             Secton 5.3: Tests for Normality

################################################################
################################################################


housing <- read.table("housing.txt", header=TRUE, row.names=1)

library(fBasics)

#####  The chi-squared test of binned frequencies

pchiTest(housing$Apartment, 
    description="Original apartment values")

##### JB test

jbTest(housing$Apartment, 
    title="Original apartment values")

jbTest(-housing$Apartment^(-1/2), 
     title="Transformed apartment values")

##### Kolmogorov-Smirnov

z <- housing$Apartment
z <- ( z -mean(z)) / sd(z)
ksnormTest(z)


#####  Shapiro-Wilk test: 

# original apartment values:
shapiro.test(housing$Apartment)

#  and those transformed:
shapiro.test(-housing$Apartment^(-1/2))


################################################################
################################################################

#            Section 5.4: Inference on Means

################################################################
################################################################


# Student t- distribution: Figure 5.7

### pdf(file="t_density.pdf")

x <- (-100:100)/25
plot(x,dnorm(x),type = "l", ylab = "Density", xlab = "t value", 
    xlim = c(-4,4), col = "blue", cex.lab = 1.75)
lines(x,dt(x,1), type = "l", col = "blue")
lines(x,dt(x,2), type = "l", col = "blue")
lines(x,dt(x,5), type = "l", col = "blue")
lines(x,dt(x,10), type = "l", col = "blue")
text(rep(2.5,4), c(dnorm(0), dt(0,5), dt(0,2), dt(0,1)), cex = 1.2,
   c("normal", "5df", "2 df", "1df"))
lines(c(2,0), rep(dnorm(0),2), type="l")
lines(c(2,0), rep(dt(0,5),2), type="l")
lines(c(2,0), rep(dt(0,2),2), type="l")
lines(c(2,0), rep(dt(0,1),2), type="l")

### dev.off()

################################################################

sample <- rnorm(6)
sample
mean(sample)
var(sample)
sd(sample)

t.test(sample)

t.test(sample, mu=2)

sample2 <- rnorm(5) + 1.5 
sample2
t.test(sample, sample2)

################################################################

# Table 5.1 and Figure 5.8: Simulate confidence intervals

reps <- 50                # number of intervals to generate
sim.mean <- 2             # true population mean
cover <- 0                # % coverage of true value
ints <- NULL
for (i in 1:reps)         # generate the confidence intervals
   {
    int <- t.test(rnorm(25, mean =sim.mean, sd =5))$conf.int[1:2]
    ints <- rbind(ints,int) 
    if(int[1] <= sim.mean &&
       int[2] >= sim.mean) cover <- cover+1
   }
ints                      # print these
cover <- cover/reps
cover

### pdf(file = "simCI.pdf")
op <- par(mfrow =c(1,2), cex.lab = 1.5)  # side-by side plots
plot(0,0, xlim = c(min(ints[,1])-.5, max(ints[,2])+.5),
   ylim=c(0,reps), type="n", ylab="Simulation number",
   xlab="Confidence interval") # create an empty shell
for (i in 1:reps)         # plot the intervals
   {
   lines(ints[i,],c(i,i), col=rainbow(6)[4], lwd=3.5)
   points(ints[i,],c(i,i), col="red", pch=16, cex=.5)
   }
lines(rep(sim.mean,2), c(-5,reps+5), lty=3, col="blue")
 
lengths <- ints[,2] - ints[,1]
hist(lengths, xlab="Lengths of intervals", 
   main="", breaks=10, col=rainbow(7)[5])
par(op)                   # reset plotting option
### dev.off()


#####################################################################

#  Male and Female math exam scores

score <- scan(file = "math_scores.txt")
female <- "F"
male <- "M"
sex<- c(rep(female, 52), rep(male, 31))
math <- data.frame( cbind(score, sex))
math

#  ----------- Figure 5.9 -------------
### pdf(file = "math_hist.pdf")    
op <- par(mfrow=c(2, 1), cex.lab = 1.2)
attach(math)
hist(score[sex == female], main = "Female",  xlim = c(20, 100), 
   ylab = "", xlab = "", col = "red", yaxt = "n")
hist(score[sex == male], main="Male", xlim = c(20, 100),
   xlab="Exam score", ylab = "", col = "blue", yaxt = "n")
par(op)           # reset
### dev.off()

###############################################################

sd(score[sex==female])  # Female standard deviation
sd(score[sex==male])  # Male standard deviation

###############################################################

#  ----------  Fig 5.10  ----------------
### pdf(file="math_qq.pdf")

op <- par(mfrow =c(1,2), cex.lab = 1.5)  

qqnorm(score[sex == female], main = "Female", col = "red", 
   pch = 16, cex = 1.5)
qqline(score[sex == female])
qqnorm(score[sex == male], main = "Male", col = "blue", 
   pch = 16, cex = 1.5, ylab = "")
qqline(score[sex == male])

par(op)
### dev.off()

#################################################################


t.test(score[sex == female], score[sex == male])


################################################################
################################################################

#          Section 5.4:  Inference on Variances

################################################################
################################################################


sample <- sqrt(6)*rnorm(10)
sample

      #####   numbers in text:
 sample <- (  5.8661171 , 0.3683464 , 0.8060940 , 0.1836058 ,
    -1.6013981 , 2.2258245 , 0.6948691 , -0.2916154 , -4.9956032 ,
     -1.8808992)

var(sample)

chis95 <- c( qchisq(.975, 9), qchisq(.025, 9))
chis95

9 * var(sample) / chis95   #   95% confidence interval

chis05 <- qchisq(.95, 9)  # 95% one-sided confidence interval
chis05

9 * var(sample) / chis05


#################################################################
#################################################################

## Section 5.5 Maximum Likelihood Estimation, Part I

#################################################################
#################################################################


> z <- rnorm(25, mean=5, sd=10)
> z[1 : 5]
[1]  3.861693  9.953657 14.686397 17.601439 -8.863527
> mean(z)
[1] 3.949079
> sd(z)
[1] 10.33858


#  Likelihood function:

 lik <- function(parm){prod(dnorm(z, mean = parm[1], sd = parm[2]))}

#  LOG likelihood function:

llik <- function(parm) {sum( dnorm(z, mean = parm[1], 
                    sd = parm[2], log = TRUE ))}

#################################################################

##   Figure 5.11: Perspective plot

### pdf(file = "loglik.pdf")

##  z <- rnorm(25, mean = 5, sd = 10)
     trange <- seq(from = 0, to = 10, length = 25) # range of theta
     srange <- seq(from = 6, to = 14, length = 25) # range of sigma
     drawll <- function(th, sig)                   # the log-likelihood function
          {
           sum(dnorm(z, mean = th, sd = sig, log = TRUE))
          }
     # evaluate the log-likelihood at every pair of parameter values
     zvals <- matrix(0, length(trange), length(srange))
     for (i in 1 : length(trange))
         { for (j in 1 : length(srange))
            { zvals[i, j] <- drawll(trange[i], srange[j])
         }  }

     persp( x = trange, y = srange, z = zvals, cex.lab = 1.5, 
            zlim = c(min(zvals), max(zvals) + .5),
            xlab = "mean", ylab = "std deviation",
            zlab = "Log likelihood", col = rainbow(6)[3],
            axes = TRUE, box = TRUE, theta = -30, shade = .3) 

###  dev.off()

################################################################


 nll <- function(par)      # negative log-likelihood 
+      {-sum(dnorm(z, mean = par[1], sd = par[2], log = TRUE))}


 nlm(nll, c(10, 15))   # call nlm with initial values

###############################################################


##  Larger MLE example:  SD proportional to the mean

z <- rnorm(50, mean = 10, sd = 20)  # generate some data
z                                # print the data
prop <- function(par)            # negative log-likelihood
       { -sum(dnorm(z, mean = par[1], 
          sd = par[1] * par[2], log = TRUE ))}

nlm( prop, c( 5, 5))              # maximize with starting values

###############################################################
###############################################################
# ------------------  end of this file -----------------------
###############################################################
###############################################################
