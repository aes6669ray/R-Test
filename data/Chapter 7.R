############################################################

############    Chapter 7: Multivariate Normal    ##########

############################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

############################################################
#                           Section 7.2
############################################################

#  --------------  Figure  7.1  --------------
library(ICSNP)
data(LASERI)

### pdf(file = "LASERI.pdf")

Lplot <- LASERI[, 25 : 28]
n <-dim(LASERI) [1]
                     # append a vector of zeros for axis
Lplot <- rbind(Lplot, rep(0, 4))  
Lplot[(n - 1) : (n + 1), ]

pairs(Lplot,  gap = 0, col = c(rep("red", n), "blue"),
      pch = c(rep(16, n), 3), cex = c(rep(1, n), 40),
      xaxt = "n", yaxt = "n")

### dev.off()

############################################################

for (j in 25 : 28)
{
  print(noquote(c("Test for", colnames(LASERI)[j]))
  print(t.test(LASERI[, j]))
}
HotellingsT2(LASERI[, 25 : 28])
  
  
#############################################################

#   Section 7.3 Patterned Covariance Matrix

#############################################################

CS <- read.table(
    file = "HomePrice.txt",     # Case-Schiller home prices 
    header = TRUE, row.names  = 1)
colnames(CS) <- 2001:2011       # columns are years
CS <- log(CS/100)               # log of data values

##########################################################

#   Figure 7.2: Spaghetti plot of home prices

year <- as.integer(colnames(CS))
year <- c(2000, year)
ncity <- dim(CS)[1]
yrange <- range(CS)
### pdf(file = "CSspaghetti.pdf")   #    Figure 7.2
plot(year , c(0, CS[1,]), type = "l", ylim = yrange, col = "blue",
     xlab = "Year", ylab = "Log C-S Index", cex.lab = 1.5)
text(2009, -.35, labels = "Detroit")
for (i in 2:ncity)
  lines(year , c(0, CS[i, ]), type = "l", ylim = yrange, col = "blue")
lines(year, c(0, colMeans(CS)), type = "l", col = "red", 
      lwd = 2)
### dev.off()

library(mvtnorm)                # library with -dmvnorm-

###########################################################
  
# correlation matrix in Section 7.3
print(cor(CS), digits = 2)

###########################################################

#    Figure 7.2: Annual means and standard deviations 
#   of log Case-Schiller values

### pdf(file = "homesd.pdf")

sigma = sapply(CS, sd)
plot(2000 : 2011, c(0, sigma), xlab = "Year", ylab = "", 
     cex.lab = 1.5, cex = 3, pch = 16, 
     col = 2, ylim = c(0., .3))
lines(2000 : 2011, c(0, sigma), col = "blue", lwd = 3)

ms <- colMeans(CS) / 2
axis(side = 4, at = c(0, .1, .2, .3), 
     labels = c(0, .2, .4, .6))
lines(2000 : 2011, c(0, ms), pch = 16, cex = 3, 
      col = rainbow(5)[3], type = "p")
lines(2000 : 2011, c(0, ms), col = rainbow(5)[4], lwd = 3, 
      type = "l")
text(c(2010, 2010), c(.15, .25), 
     labels = c("means", "sd''s"), cex = 1.5)

### dev.off()

############################################################

#  Autocorrelation of CS data and Exercise 7.8

autocov <- function(data)
             #  Autocorrelation of annual columns in data 
   {
    nyears <- dim(data)[2] - 1
    for (lag in 1 : nyears)
       {
         lagcor <- 0
         for (year in 1 : (nyears - lag + 1))
              lagcor <- lagcor + 
                  cor(data[,year], data[, year + lag])
         lagcor <- lagcor / (nyears - lag + 1)
         if (lag == 1) autocov <- lagcor
              else autocov <- c(autocov, lagcor)
       }
    autocov                     #  returned value
   }

ac <- autocov(CS)

###########################################################

### pdf(file = "achome.pdf")    #   Figure 7.3

plot(1 : length(ac), ac, pch = 16, cex = 3, col = "red", 
     cex.lab = 1.5, xlab = "Lag", ylab = "Autocorrelation")
lines(1 : length(ac),ac,col = "blue",lwd = 3)

### dev.off()

##########################################################
##########################################################

#               Section 7.4:  MLE, Part III

##########################################################
##########################################################

#   Fit mean models for Case-Schiller data.

library(mvtnorm)                # library with -dmvnorm-

CS <- read.table(
    file = "HomePrice.txt",     # Case-Schiller home prices 
    header = TRUE, row.names = 1)
colnames(CS) <- 2001 : 2011     # columns are years
CS <- log(CS / 100)             # log of data values

print(CS, digits = 2)

###########################################################

yrmean <- colMeans(CS - 
          t(matrix(rep(as.vector(colMeans((CS))), 20),
                   11, 20)))
yrmean

###########################################################

mvnMean <- function(parms)
# Log-likelihood function to fit means to a multivariate 
# normal distribution to CS data
# meanModel is external function that determines the mean

   {
    n <- dim(CS)[1]               # sample size
    p <- dim(CS)[2]               # number of variables

    mu <- meanModel(parms, p)     # mean vector of length p
    resid <- 
       CS - t(matrix(rep(mu, n), p, n)) # subtract the mean
    vm <- var(resid)              # the variance of the data
    mvnMean <- sum(dmvnorm(resid, log = TRUE, 
          sigma = var(resid)))    # multivariate normal density
    print(c(parms, mvnMean))      # likelihood at this estimate
    -mvnMean                      # return negative to minimize
   }

######################################################

meanModel <- function(parms,p)
#  Model for one mean
   { 
    return(rep(parms[1], p))
   }

oneMout <- nlm(mvnMean, 0, hessian = TRUE)
oneMout
print(sqrt(1 / oneMout$hessian), 3)
loglik <- oneMout$minimum

#####################################################

meanModel <- function(parms, p)
#  Model for mean as linear function of year
   {
    return(parms[1] + parms[2] * (1 : p))
   } 

linMout <- nlm(mvnMean, c(0, 0), hessian = TRUE)
linMout
invhess <- solve(linMout$hessian) # invert hessian matrix

sqrt(invhess[1, 1])  # est"d std error of intercept
sqrt(invhess[2, 2])  # est"d std error of slope

loglik <- c(loglik, linMout$minimum)

####################################################

meanModel <- function(parms, p)
#  Model for mean as quadratic function of year
   {
    parms[1] + parms[2] * (1 : p) / 10 + 
           parms[3] * ((1 : p) ^ 2) / 100)
   } 

quadMout <- nlm(mvnMean, rep(0, 3), hessian = TRUE)
quadMout
for (i in 1 : 3)print(c(quadMout$estimate[i],
   sqrt(solve(quadMout$hessian)[i ,i])), digits = 3)
loglik <- c(loglik, quadMout$minimum)

#####################################################

meanModel <- function(parms, p)
# Model for separate mean in each year
   parms

sepMout <- nlm(mvnMean, rep(0, 11), hessian = TRUE)
sepMout
sepvar <- solve(sepMout$hessian)
for (i in 1 : 11) print( c(
   sepMout$estimate[i], sqrt(sepvar[i, i]), 
   mean(CS[, i]), sd(CS[, i]) / sqrt(20) ), 
   digits = 3)

loglik <- c(loglik, sepMout$minimum)

loglik

########################################################

# build data.frame summarizing mean fitting

loglik <- abs(loglik)        # reverse signs
ch0 <- 2 * (loglik - loglik[1])# compare to one mean model
df0 <- c(NA, 1, 2, 10)
p0 <-  1 - pchisq(ch0, df = df0)
p0[1] <- NA

ch1 <- 2*(loglik - 
   loglik[c(1, 1, 2, 3)])       # compare to previous model
df1 <- c(NA, 1, 1, 7)
p1  <- 1 - pchisq(ch1, df = df1)
p1[1] <- NA
msum <- data.frame(cbind(loglik, ch0, df0, p0, 
     ch1, df1, p1))

row.names(msum) <- c("one mean", "linear",
    "quadratic", "separate")

msum

###############################################################
###############################################################

#        Section 7.5  Fit patterned Covariance matrices

###############################################################
###############################################################

#  Detrended home price data

CS <- read.table(
  file = "HomePrice.txt",       # Case-Schiller home prices 
  header = TRUE, row.names  = 1)
colnames(CS) <- 2001:2011       # columns are years
CS <- log(CS/100)               # log of data values

dc <- dim(CS)
detr <- CS - t(matrix(colMeans(CS), dc[2], dc[1]))

year <- as.integer(colnames(CS))
year <- c(2000, year)
ncity <- dim(CS)[1]
yrange <- range(detr)
### pdf(file = "CSdetrended.pdf")   #    Figure 7.5
plot(year , c(0, detr[1,]), type = "l", ylim = yrange, col = "blue",
     xlab = "Year", ylab = "Detrended C-S Index", cex.lab = 1.5)
text(2009, -.6, labels = "Detroit")
for (i in 2:ncity)
  lines(year , c(0, detr[i, ]), type = "l", ylim = yrange, col = "blue")
lines(c(1990, 2012), c(0,0), type = "l", col = "red")
### dev.off()

###################################################################

autocorr.mat <- function(size = 1, rho = 0) 
# build autocorrelation correlation matrix. Attributed to Peter Dalgaard
  rho ^ abs(row(diag(size)) - col(diag(size)))

exchange.mat <- function(size = 1, rho = 0) 
# build exchangeable matrix
  matrix(rho, size, size) + (1 - rho) * diag(size)

cov.model <- function(size, parms)
# shell to fit patterned covariance matrix 
#  mv.vars models variances,  mv.cors models correlations
{
  rho <- parms[1]                             # rho is first parameter
  rho <- max(.0001, min(rho, .9999))          # must be between zero and one
  sigma <- sqrt(mv.vars(size, parms[-1]))     # model for marginal st dev's
  sigma %*% t(sigma) * mv.cors(size, rho)     # covariance matrix
}

library(mvtnorm)                # library with -dmvnorm-

fit.cov <- function(parms) # Shell called by nlm
  # computes log-likelihood of detrended home price data (detr)
  # checks for positive definite covariance matrix
{
  logl <- 0
  size <- dim(detr)[2]          # size of the detrended data
  cov <- cov.model(size, parms) # the trial built  covariance matrix
  print(det(cov))               # check on trial covariance matrix 
  if(det(cov) <= 1.0e-30)       # is the covariance positive definite?
    {
       cov <- cov + diag(size)  # adjust the covariance... 
       logl <- -1000            #  ... and penalize the likelihood 
  }                             # log multivariate normal density
  logl <- logl + dmvnorm(detr, mean = rep(0, size), 
                  sigma = cov, log = TRUE)
  logl <- sum(logl)             # log likelihood of detrended data
  print(c(parms, logl))         # trace the progress of nlm
  -logl                         # return negative log likelihood
}


format.nlm <- function(nlm.out)  # format output from nlm
{
  est <- nlm.out$estimate
  p <- length(est)
  estvar <- solve(nlm.out$hessian)
  line <- -nlm.out$minimum
  for (i in 1:p)
    line <- c(line, est[i], sqrt(estvar[i, i]))
  line
}


###### Fit models in Section 7.5

size <- dim(detr)[2]

mv.vars <- function(size, parms)  rep(exp(parms[1]), size)   #  same std dev for all
mv.cors <- function (size, rho) exchange.mat(size, rho)      #  exchangeable correlation
nlm.out <- nlm(fit.cov, c(0.6499924, -3.2107213), hessian = TRUE)  # 125.8429352
print(format.nlm(nlm.out), 3)

# # # #

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size)), # log-linear vars
         rep(10, size))                         # check for overflow
mv.cors <- function (size, rho) exchange.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.6729004, -4.7436157,  0.2289721), hessian = TRUE)  #   149.8109957
print(format.nlm(nlm.out), 3)

# # # #  

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size) + parms[3] * (1 : size) ^2), # log-quad vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) exchange.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.7068494, -6.64529169, 0.97739795, -0.06020731),
               hessian = TRUE)  #  180.2007
print(format.nlm(nlm.out), 3)


######################  autocorrelation models  ################


mv.vars <- function(size, parms)  rep(exp(parms[1]), size)   #  same std dev for all
mv.cors <- function(size, rho) autocorr.mat(size, rho)       #  autocorrelation

nlm.out <- nlm(fit.cov, c(0.9265689, -3.388858), hessian = TRUE)   # 256.174784
print(format.nlm(nlm.out), 3)

# # # # 

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size)), # log-linear vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) autocorr.mat(size, rho)      #  autocorrelation

nlm.out <- nlm(fit.cov, c( 0.90914786,  -4.8670958,  0.1964539), hessian = TRUE)  # 268.7139643
print(format.nlm(nlm.out), 3)

# # # #


mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size) + parms[3] * (1 : size) ^2), # log-quad vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) autocorr.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.93926171, -6.11727682, 0.80739825, -0.04945812),
               hessian = TRUE)  #  292.0728
print(format.nlm(nlm.out), 3)



########################################################
######  Section 7.6: Tests for Multivariate Normal #####
########################################################

candy <-  read.table(file = "candy.txt", 
                     row.names = 1, header = TRUE)
mah <- mahalanobis(candy, colMeans(candy), var(candy))
n <- dim(candy)[1]                # number of rows               
ncols <- dim(candy)[2]            # number of data columns
print(ncols)                      # check # of cols

# Fig 7.6: QQ plot of Mahalanobis distances 

### pdf(file = "mahcandy.pdf")   

qqplot(qchisq((1 : n) / (n + 1), df = ncols), mah, 
    pch = 16, cex = 3, col = "red", xlim = c(1.25, 15),
    xlab = "6 df chi-squared quantile", cex.lab = 1.5, 
    ylab = "Mahalanobis distance from sample mean") 
abline(0, 1, col = "green", lwd = 4)  # connect quartiles

nbig <- 2                 # How many of the largest to name?  
bigest <- order(mah)[(n - nbig + 1) : n] # indices of largest
text(qchisq(((n - nbig + 1) : n) / (n + 1), df = ncols) - .2, 
   mah[bigest] - c(0, .5), pos = c(2, 1), cex = 1.5, 
   labels = row.names(candy)[bigest])

### dev.off()

###########################################################

### Test of this QQ plot:

shapiro.test(qnorm(pchisq(mah,ncols)))

###########################################################

# Figure 7.7:  Parallel coordinates of candy values

### pdf(file = "parcandy.pdf") 
op <- par(cex.axis = 1.5)
library(MASS)
color <- rep("green", n)           # Default colors
color[bigest] <- 
     rainbow(4)[rep(4, nbig)]      # Color for extremes 
lw <- rep(1, n)                    # Line widths
lw[bigest] <- 2                    # Extra wide for 3 extremes
parcoord(candy, col = color, lwd = lw, cex.axis = 1.5) 
                                   # Parallel coordinates
text(c(1.5, 4.5), c( .96, .96), col = "red", cex = 1.5,
  labels = row.names(candy)[bigest])  # Label the extremes
par(op)

### dev.off()

#######################################################

#   Skewness and kurtosis of candy data

require(psych)
mardia(candy)

######################################################

#  Multivariate Shapiro-Wilk test

candy <-  read.table(file = "candy.txt", 
                     row.names = 1, header = TRUE)

library(mvnormtest)
mshapiro.test( t( candy))

#######################################################

#   Figure 7.8:  Individual QQ plots of candy data

### pdf(file = "candy4QQ.pdf")   
 op <- par(mfrow = c(3, 2))    #  3x2 plot

for (i in 1 : 6)
   {
      qqnorm(candy[,i], col = "red", pch = 16, 
             cex = 1.5, cex.lab = 1.5,
          main = colnames(candy)[i])
      qqline( candy[, i], col = "blue", lwd = 2 )
      sh <- shapiro.test( candy[, i] )$p.value
      print(sh)
      text(-1, max(candy[, i]) * .9, cex = 1.5, labels =  
          paste("p = ", format(sh, digits = 2), sep = ""))
   }

 par(op)

### dev.off()

########################################################

#  Energy test

library( energy )
mvnorm.etest( candy )

########################################################

# Energy residuals (in the Solutions section)

msqrt <- function(a)
# finds matrix square root of positive definite matrix
    {
     a.eig <- eigen(a)           # eigenvalues and eigenvectors 
     if ( min(a.eig$values) < 0) # check for positive definite
         warning("Matrix not positive definite")

     a.eig$vectors %*% diag(sqrt(a.eig$values)) %*%
         t(a.eig$vectors)
    }

energy.resid <- function(dat, R = 300)
   {
    n <- dim(dat)[1]            # observations
    p <- dim(dat)[2]            # variables
    std <- dat - t(matrix(colMeans(dat), p, n))
    s <- var(dat)
    std <- as.matrix(std)  %*% solve(msqrt(s)) # std data
    rand <- matrix(rnorm(p * R), R, p) # independent normal data

    A <- rep(0, n)
    B <- rep(0, n)
    for (i in 1 : n)
       {
        a <- 0 
        b <- 0
        for (j in 1 : R) 
          a <- a + sqrt(sum((std[i, ] - rand[j, ]) ^ 2)) 
        A[i] <- a / R       # ave dist between data and random
        for (j in 1 : n) 
          b <- b + sqrt(sum((std[i, ] - std[j, ]) ^ 2))
        B[i] <- b / (n - 1)       # ave dist between data
       }
    cc <- 0
    for(i in 2 : R)  for(j in 1 : (i - 1)) 
          cc <- cc + sqrt(sum((rand[i, ] - rand[j, ]) ^ 2))   
    C <- 2 * cc / (R * (R - 1)) # ave dist between random values

    2 * A - B - C               # returned value
}


##########################################################

library(ICSNP)
data(LASERI)

library(MASS)
cov.rob(LASERI[, 2:10])

##########################################################
##########################################################

##  European stock prices

data(EuStockMarkets)          # European stocks, early 1991
stocks <- data.frame( EuStockMarkets[15 : 29, 1 : 4] )
stocks

mah <- mahalanobis(stocks, colMeans(stocks), var(stocks))
n <- dim(stocks)[1]                # number of rows               
cols <- dim(stocks)[2]             # number of data columns

### pdf(file = "mahstocks.pdf")    # Fig. 7.?: QQ plot 

qqplot(qchisq((1 : n) / (n + 1), df = cols), mah, 
    pch = 16, cex = 2, col = 2, 
    xlab = "4df chi-squared quantile",
    ylab = "Mahalanobis distance from sample mean") 
abline(0, 1, col = 3, lwd = 4)    # connect the quartiles
text(qchisq(n / (n + 1), cols) - .2, max(mah), pos = 2,
   labels = "Jan 22, 1991")

### dev.off()

#########################################################
#########################################################

#        ---  end of this file ---
