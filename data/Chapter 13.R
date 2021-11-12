###################################################

#   R code for figures in Chapter 13: Other Topics

##################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

###################################################

# Bradley-Terry ranking of probability journals

prcite <- read.table(file = "probcite.txt", header = TRUE, 
                     row.names = 1)
(prcite <- prcite[-7, -7])  # remove Totals in margins
                            # number of non-self citations
ns <- colSums(prcite) - diag(as.matrix(prcite))

library(BradleyTerry2)
# convert contingency table to list and remove self reference
clist <- countsToBinomial(prcite)
names(clist)[1 : 2] <- c("journal1", "journal2")
clist

ni <- ns[match(clist$journal1, names(ns))]
nj <- ns[match(clist$journal2, names(ns))]
os <- log(nj/ni)              # offset
clist <- cbind(clist, os)

fitBTo <- BTm(cbind(win1, win2), journal1, journal2, 
     offset = os, data = clist)
summary(fitBTo)

# fit without the offset

fitBT <- BTm(cbind(win1, win2), journal1, journal2, 
             data = clist)
summary(fitBT)

fitBT$coefficients
fitBT$residual
fitBTo$residual

cbind(clist, fitBT$residual)
names(clist)[5] <- residual
clist

########################################################

#   Section 13.2: Regression Trees

########################################################

##   Simple example: Figure 11.13

library(mvpart)
people <- read.table(file = "people.txt", header = TRUE)
people <- data.frame(people)
print(people)

### pdf(file = "people.pdf")
fit <- mvpart(Classify ~ Age + Sex, data = people)
### dev.off()

############################################################

#  Univariate example: Characteristics of cars in 1974

### pdf(file = "cartree.pdf")    # Figure 11.14

require(mvpart,datasets,graphics)
univ <- mvpart(mpg ~ cyl + disp + am + carb, data = mtcars) 

### dev.off()

###########################################################

# Classification of crabs

library(MASS)
data(crabs)
crabs[c(1, 51, 101, 151),]
group <- 1 + 2 * (crabs[,1] == "O") + (crabs[,2] == "F")
groupch = c( "B", "b", "O", "o")[group]
cg <- cbind(crabs, group, groupch)
cg[c(1, 51, 101, 151), ]

### pdf(file = "crabs.pdf")
library(mvpart)
crabtree <- mvpart(groupch ~ FL + RW + CL + CW + BD, 
                   data = cg, method = "class", size = 8)
### dev.off()

#########################################################

#  plot CP statistics

crabtree <- mvpart(groupch ~ FL + RW + CL + CW + BD, 
                   data = cg, method = "class", size = 20)
crabcp <- printcp(crabtree)

### pdf(file = "crabcp.pdf")
plot(crabcp[,2], crabcp[,4], col = "red", 
     ylim = c(0.0, 1.25),
     xlab = "Number of splits", 
     ylab = "Cross validated errors",
     type = "b", pch = 16, cex = 1.5, cex.lab = 1.5)
lines(crabcp[,2], crabcp[,3], col = "blue", type = "b",
      pch = 16, cex = 1.5)
lines(crabcp[,2], 6 * crabcp[,1], col = "violet", 
      type = "b", pch = 16, cex = 1.5)
text(c(5, 4, 3), y = c(.9, .45, .2), col = 
       c("red", "blue", "violet"),
     labels = c("Std error", "Rel error", "CP"), cex = 1.35)
axis(side = 4, tick = TRUE, at = c(1.2, .9, .6, .3, 0) ,
     labels = c(.2, NA , .1, NA, 0), col.axis = "violet", 
     xlab = "CP", col = "violet")
### dev.off()

#######################################################
#######################################################

# Section 13.3:  Canonical Correlations

#######################################################
#######################################################


library(datasets) 
library(CCA)
                        # cyl, disp, carb, drat, gear,vs
design <- mtcars[ , c(2, 3, 5, 8, 10, 11)] 
                        # mpg, hp, wt, qsec, am
driver <- mtcars[ , c(1, 4, 6 ,7, 9)]    

cancor(design, driver)

cc(design, driver)   #  CCA package enhancement

ccs <- cc(design, driver)

descc1  <- ccs$scores$xscores[ , 1]
drivcc1 <- ccs$scores$yscores[ , 1]

sdr <- sort(drivcc1)
sdr <- sdr[c(1, length(sdr) - 1)]   # first and last
ext <- match(sdr, drivcc1)

### pdf(file = "carcc.pdf")

plot( descc1, drivcc1, cex.lab = 1.5,
      xlab = "Design canonical scores", 
      ylab = "Driver canonical scores",
      pch = 16, cex = 1.75, col = "red")
text(descc1[ext], drivcc1[ext], 
     labels = rownames(mtcars)[ext],
      pos = c(4, 2), cex = 1.0, col = "blue")

###  dev.off()

#  Check:
cancor(design, driver)$cor
cor(descc1, drivcc1)

sdes <- sort(descc1)[1 : 4]

###################################################

#     Behavioral Risk Factor Surveilance System 

# read data and make data frame

BRFSS <- read.table("BRFSS.txt", header = TRUE)
BRFSS <- data.frame(BRFSS[ , 2 : 19], 
                    row.names = BRFSS[ , 1])

                 # health outcome variables
out <- c(2, 3, 5, 6, 7, 8, 11, 14)
                 # behavioral measures
beh <- c(1, 4, 9, 10, 12, 13, 15, 16, 17, 18) 
BRFSS[1, out]
BRFSS[1, beh]

cancor(BRFSS[,out], BRFSS[,beh])
library(CCA)
#  CCA package enhancement
ccs <- cc(BRFSS[ , out], BRFSS[ , beh])  

outcc1  <- ccs$scores$xscores[ , 1]
behcc1 <- ccs$scores$yscores[ , 1]

sdr <- sort(outcc1)
sdr <- sdr[c(1, length(sdr) - 1)] # first and next-to-last
ext <- match(sdr, outcc1)

########################################################

#   Figure 13.4: Canonical correlations for BRFSS data

### pdf(file = "BRFSScc.pdf")

plot( outcc1, behcc1, cex.lab = 1.5,
      xlab = "Health outcome canonical scores", 
      ylab = "Behavioral canonical scores",
      pch = 16, cex = 1.5, col = "red", 
      xlim = sdr * c(1.1, 1))
text(outcc1[ext], behcc1[ext], 
     labels = rownames(BRFSS)[ext],
     pos = c(1, 2), cex = .75, col = "blue")

### dev.off()

######################################################

#    Figure 13.5: Canonical weights for BRFSS data

###pdf(file = "BRFSSccv.pdf")

library(CCA)
                        # health outcome variables
out <- c(2, 3, 5, 6, 7, 8, 11, 14) 
                        # behavioral measures
beh <- c(1, 4, 9, 10, 12, 13, 15, 16, 17, 18) 
ccs <- cc(BRFSS[,out], BRFSS[,beh]) 
plt.cc(ccs, d1 = 1, d2 = 2, type = "v",
       var.label = TRUE)

### dev.off()


######################################################
######################################################

#   Section 13.3:  Methods for Extremes

######################################################
######################################################


dex <- function(x,j)
#  Marginal density function of the j-th extreme
{
    if(j < 1)return(NA)  # test for valid j = 1,2,...
    dex <- exp( -x)
    dex <- exp( -dex - j * x) / gamma(j)
    dex
}

cdex <- function(x, j) 
# Cumulative, marginal distribution of j-th extreme
{
   if(j < 1) return(NA) # test for valid j = 1,2,...
   cdex <- dex(x, 1) + exp( -exp( -x))  # j = 1
   if(j < 2) return(cdex)
   for (i in 2 : j)
      {
       cdex <- cdex + dex(x, i) / i
      }
   return(cdex)
}

qdex <- function(p, j) # Quantile of the j-th extreme
{
   if(p <= 0)return(NA)     # test validity of arguments
   if(p >= 1)return(NA)
   if(j < 1)return(NA)
   obj <- function(x) #  local objective function
      {
       return(cdex(x, j) - p)
      } 
   qdex <- uniroot(obj, interval = c( -5, 10))$root
   return(qdex)
}

exest <- function(x) 
# estimate of location and scale for extreme value model
{
   Eg <- 0.5772156649               #  Euler"s constant
   k <- length(x) - 1               #  there are k+1 obs
   if(k < 2) return(c(NA, NA))      #  need at least two obs
   sx <- sort(x, decreasing = TRUE) #  sort the data
   a <- mean(sx[1 : k]) - sx[k + 1] #  estimated scale
   S <- 1 : (k + 1)                 #  k+1 Harmonic number
   S <- sum(1 / S)
   b <- a * (S - Eg) + sx[k + 1]    #  estimated location
   c(a, b)                          #  Done!
}


gini.test <- function(x)

#  Test spacings of largest order statistics using Gini test
#  Reference: Gail and Gastwirth 1978
{
   if(length(x) <= 2)return(NA)   #  is data too short?
   sx <- sort(x, decreasing = TRUE) #  sorted data, down
   for (k in 2 : (length(x) - 1))   #  examine the k+1 largest 
      {
                                    # spacings of k+1 largest
       d <- sx[1 : k] - sx[2 : (k + 1)]
       d <- d * (1 : k)             #  normalized spacings
       d <- sort(d, decreasing = TRUE)# ordered, normalized 
       g <- d[1 : (k - 1)] - d[2 : k]   # spacings of spacings
       g <- (1 : (k - 1)) * seq(k - 1, 1, -1) * g
       g <- sum(g) / ((k - 1) * sum(d[1 : k]))
       if(k == 2)Gini <- g          # test statistics
       else  Gini <- c(Gini, g)
      }
   p <- 12 * (1 : length(Gini)) #  1/variances of statistics
   p <- (Gini - .5) * sqrt(p)   #  normalized test statistics
   p <- 2 - 2 * pnorm(abs(p))   #  two-tailed p-values
   cbind(Gini,p)                #  statistics and p-values
}


#######################################################

#   Figure 13.4: 
#   Five extreme distributions" density functions


### pdf(file = "extreme.pdf")
x<-seq(-3.5, 7, .05)
plot(x, dex(x, 5), type = "l", xlab = "z", cex.lab = 1.4,
          ylab = "density of i-th extreme")
text(c(2, .45, .0, -.4, -.5),  c( .2, .45, .6, .75, .85),  
    labels = c("i = 1", "i = 2", "i = 3", "i = 4", "i = 5") )        
for (j in 5 : 1)
   { lines(x, dex(x, j), col = "blue")}
### dev.off()

####################################################

#  Fig 13.5:  Gini p-values

giniplot <- function(data)
   {
    n <- length(data)
    if(n < 3) return(0)
    plot( 1 : n,            #  Plot the pvalues for the data
       c(NA, NA, gini.test(data)[ , 2]), 
       ylim = c(0, 1), cex.lab = 1.4,
       pch = 16, cex = 1.2, col = "blue", type = "b",
       ylab = "P-value for Gini test",
       xlab = "Ranking of values" )
   }


bigdeal <- read.table(file = "bigdeal.txt", header = TRUE, 
                      row.names = 1)
print(bigdeal)
### pdf(file = "giniplot.pdf")
giniplot(bigdeal$value)

### dev.off()

#####################################################

#   Fig. 13.8: Extremal plot of brokered deals

extplot <- function(x)
{
    if(length(x) <= 1) return(NA)
    x <- sort(x, decreasing = TRUE)      # sort down
    parms <- exest(x)                    # estimate a,b
    a <- parms[1]
    b <- parms[2]
    print(parms)

    xrange <- seq(-3.5, 2, .05)
    xplot <- a * xrange + b    # transform to scale of data 
    plot(c(max(0, min(xplot, min(x))),  max(xplot, max(x))), 
        c(1, length(x)), type = "n",      # empty plot area
        xlab = "Deal size in $Billion", 
        ylab = "Ordered values", cex.lab = 1.5)
    
    for(i in 1 : length(x))  # for every observed value:
       {
        points(x[i], i,              # plot data point
           pch = 16, cex = 1.5, col = "red")
        lines(a * c(qdex(.05, i), qdex(.95, i)) + b,
           c(i, i), lwd = 2)               # 95% interval
        lines(a * c(qdex(.25, i), qdex(.75, i)) + b,
           c(i, i), lwd = 4, col = "blue")   # 50% interval
       }
} 

bigdeal <- read.table(file = "bigdeal.txt", 
          header = TRUE, row.names = 1)
print(bigdeal)

### pdf(file = "bigco.pdf")
extplot(bigdeal$value)
### dev.off()

####################################################

### Pistachio production

pistachio <- read.table(file = "pistachio.txt", 
                        header = TRUE, row.names = 1)
pistachio$production <- pistachio$production / 1000

giniplot(pistachio$production)

extplot(pistachio$production)

extll <- function(a, b, x)
#  joint extremal log-likelihood 
     exp( -min((x - b) / a)) - sum((x - b) / a)


pisest <- exest(pistachio$production)

exo <- function(parm)   # extremal objective for nlm
   {
    ll <- extll(parm[1], parm[2], pistachio$production) 
    print(c(parm, ll))
    -ll
   }


nlm(exo, c(100, 200))

#######################################################
###################  end of this file #################
#######################################################

