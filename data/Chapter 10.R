########################################################

#           Chapter 10:  Discriminant Analysis

########################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

########################################################

#  This is possible:
# wines <- read.table(
#  "http://archive.ics.uci.edu/ml/machine-learning-databases/
#    wine/wine.data", 
#   sep=",")

# But it is better to create a local file with variable names:
wines <- read.table("wines.txt", header = TRUE)

##########################################################

#   Figure 10.1: Scatterplot matrix with jittered Class

### pdf(file = "winepair.pdf")  

wines <- read.table("wines.txt", header = TRUE)

colors <- c("green", "red", "blue")[wines$Class]
newwine <-  cbind(               # new dataframe  
         jitter(as.integer(wines$Class)), # jittered Class
         wines[, -1])           # omit old Class variable
names(newwine)[1] <- names(wines)[1]  # old name to new variable
pairs(newwine, pch = 16, cex = .3, gap = 0, col = colors, 
   xaxt = "n", yaxt = "n")

### dev.off()

#########################################################

#  Table 10.2: Univariate analysis

vars <- dim(wines)[2]          # number of variables
mn <- rep(0, 3)
for (i in 2 : vars)            # omit the first, Class variable
   {
    z <- summary.aov(aov(wines[, i] ~ wines$Class))
    print(z)
    p <- z[[1]]$"Pr(>F)"[1]    # capture the p-values
    print(p)
    p <- max(p, .0001)
    for(j in 1 : 3)
       {
        mn[j] <- mean(wines[ wines$Class == j, i])
       }
    if(i == 2)univ <- c(p,mn)
    else univ <- rbind(univ, c(p, mn))
   }

row.names(univ) <- colnames(wines)[2 : vars]
colnames(univ) <- c( "p-value", "Group=1", 
                     "Group=2", "Group=3")
print(univ, 4)
table(wines$Class)      # frequency table for each level

##########################################################

#  Figure 10.2:  Parallel coordinates for wines 

### pdf(file = "wineparc.pdf")

library(MASS)
colors <- c("green", "red", "blue")[wines$Class]
parcoord(wines[, c(9, 11 : 14, 7, 8)], col = colors)

### dev.off()

#######################################################
#   Section 10.2: Multinomial Logistic Regression
#######################################################

# Two stepwse, univariate logistic models:


wines <- read.table("wines.txt", header = TRUE)

w12 <- wines[wines$Class != 3, ]
w12$Class <- w12$Class -1 
full12 <- glm(Class ~ Alcohol + Ash + Alcal + Abs + Proline, 
      family = binomial, data = w12)
summary(full12)
st <- step(full12)
summary(st)

######################################################

#  Output 10.1

wines <- read.table("wines.txt", header = TRUE)
require(nnet)
wines$Class <- as.factor(wines$Class) # create factor categories
wines$rClass <- 
  relevel(wines$Class, ref = 2)  # set reference category 

winelogit <- multinom(rClass ~ 
   Alcohol + Ash + Alcal + Abs  + Proline, 
   data = wines, maxit = 200)
print(ws <- summary(winelogit), digits = 4)
tratio <- ws$coefficients / ws$standard.errors
# two tailed p values from the t with $edf = effective df
print( 2 * (1 - pt(abs(tratio), df = ws$edf)), digits = 4)


ws$fitted.values[c(1:3, 175 : 178), ]


#########################################################


nmlr <- function(fit, obs)
   {               # normalized  multivariate logistic residuals
    nmlr <- matrix(0, nrow = dim(fit)[1], 
                   ncol = dim(fit)[2])  # initialize 
    for (i in 1 : (dim(fit)[1]))
       {
        for (j in 1 : (dim(fit)[2]))
           {
            p <- fit[i, j]              # fitted p parameter
            ob <- obs[i, j]
            res <- (ob - p)  /  (sqrt( p * (1 - p) ) )
            nmlr[i, j] <- res
           }
       }
    nmlr
   }

##########################################################

# Figure 10.3: Plot of fitted values for logistic model

### pdf(file = "mlwine.pdf")
### op <- par(mfrow = c(2, 2))

colors <- c("green", "red", "blue")[wines$Class]
plot(winelogit$fitted.values[, 2 : 3], 
   col = colors, cex = 1.25,
   xlab = "Fitted  probability of  y=1",  
   ylab = "Fitted probability of y=3")
text( rep(.42, 4), c(1, .92,  .84, .76), pos = rep(4, 4),
   col = c("black", "green", "red", "blue"),
   labels = c("Observation codes:", "y=1 category", 
        "y=2 category (ref)", "y=3 category"))
lines( c(.4, .4, 1.2), c(1.2, .7, .7))

lines(c(0, 1), c(1, 0), lty = "dotted")
lines(c(0, 0), c(1, 0), lty = "dotted")
lines(c(1, 0), c(0, 0), lty = "dotted")

###  Plot of residuals

obs <- cbind(as.integer(wines$Class == 1),
             as.integer(wines$Class == 3))
colnames(obs) = c("1", "3")
res <- nmlr(winelogit$fitted.values[, 2 : 3], obs)
plot(res, col = colors, cex = 1.25,
   xlab = "Std residual for fitted probability of y=1",
   ylab = "Std residual for y=3")

### par(op)
### dev.off()

#######################################################
#     Section 10.3: Linear Discriminant Analysis  
#######################################################


wines <- read.table("wines.txt", header = TRUE)
colors <- c("green", "red", "blue")[wines$Class]
wines$Class <- as.factor(wines$Class) # factor categories

# Figure 10.4: Linear discriminant for wine data

### pdf(file = "winelda.pdf")

library(MASS)
ld <- lda(Class ~ ., data = wines)
loading <- as.matrix(wines[, 2 : 14])  %*%   ld$scaling
   
plot(loading, col = colors, pch = 16, cex = 2,
   xlab = "First linear discriminator",
   ylab = "Second linear discriminator")
for (i in 1 : 3)       # add class number to each centroid
   {
     centx <- mean(loading[wines[, 1] == i, ] [, 1] )
     centy <- mean(loading[wines[,1] == i, ] [, 2] )
     text(centx, centy, i, cex = 2)
   }

### dev.off()

##########################################################

# Figure 10.5: plot of posterior LDA predicted probabilities

wines <- read.table("wines.txt", header = TRUE)
colors <- c("green", "red", "blue")[wines$Class]
library(MASS)
ld <- lda(Class ~ ., data = wines)
loading <- as.matrix(wines[, 2 : 14])  %*%   ld$scaling

### pdf(file = "ldafit.pdf")

ldp <- predict(ld)$posterior
plot(ldp[, 1], ldp[, 3], col = colors, cex = 1.25,
   xlab = "Fitted probability of y=1", cex.lab=1.5,
   ylab = "Fitted probability of y=3")
text( rep(.42, 4), c(1, .92,  .84, .76), pos = rep(4, 4),
   col = c("black", "green", "red", "blue"), cex=2,
   labels = c("Observation codes:", "y=1 category", 
        "y=2 category (ref)", "y=3 category"))
lines( c(.4, .4, 1.2), c(1.2, .7, .7))

lines(c(0, 1), c(1, 0), lty = "dotted")
lines(c(0, 0), c(1, 0), lty = "dotted")
lines(c(1, 0), c(0, 0), lty = "dotted")

### dev.off()

#############################################################

#   Figure 10.6:  Bivariate normal contours

library(MVA)
library(mvtnorm)

corcon <- function(x, y)
    {

     nx <- length(x)
     ny <- length(y)
     z <- matrix(rep(0, nx * ny), nx, ny)  # build lattice
     for (i in 1 : nx)
        {
         for (j in 1 : ny)
            {
               xy <- c( x[i], y[j] )
               z[i,j] <- pi1 * dmvnorm(xy, mu1, cov) + 
                 pi2 * dmvnorm(xy, mu2, cov)
            }
         }
     z
    }

###########################################################

#  Figure 10.6: Normal mixtures

     pi1 <- .6                   # Prior probilities
     pi2 <- .4
     mu1 <- c(-1, -1)             # two mean vectors
     mu2 <- c(1, 1)
     cov <- matrix( c( 1, -.4, -.4, 1.2), 2, 2)
 
### pdf(file = "normmix.pdf")
### layout(t(matrix(c(2 : 1, 
###       rep(0, 2)), 2, 2)), widths = c(1, 1))

del <- .1      # how fine the grid
lim <- 3.2     # normals plotted on +/- lim
x <- seq(-lim, lim, del)
y <- x
z <- corcon(x, y) 

contour(x, y, z, drawlabels = FALSE, axes = FALSE, 
   frame = TRUE, col = "blue", lwd = 2)

persp(x, y, z, r=1,
    axes = FALSE, xlab=" ", ylab=" ", box = FALSE, 
    col = "green", shade = .05)

#   Linear discriminant function boundary:

sinv <- solve(cov)
const <- log(pi1 / pi2) -.5 * (mu1 + mu2)

### par(op)
### dev.off()


###########################################################
#              Section 10.3  Support Vector Machine
###########################################################

# Figure 10.7 (left panel): Linear SVM  

library(kernlab)
### pdf(file = "introsvm1.pdf")

svmex <- read.table(file = "svmex.txt", header = TRUE, 
                    row.names = 1) # artifical data
svmex <- svmex[svmex[, 1] > -2, ]  # omit outlier
type <- 1 + (svmex[, 1] + svmex[, 2] > 0)      # known types
plot(svmex[, 1 : 2], pch = 16, cex = 1.75,
   col = c("red", "blue")[type], xlab = " ", 
   ylab = " ", xaxt = "n", yaxt = "n")

# line A: does not discriminate completely
lines(c(-.5,.5), c(-2.5, 3.25), type = "l", 
   col = "blue") 
text(.25, 2.5, label = "A", cex = 1.2)

# line B: is not maximum margins
lines(c(-.5, .5), c(3.25, -2.5), type = "l", col = "blue")
text(-.5, 2.5, labels = "B")

sv <- ksvm(type ~ . , data = svmex, type = "C-svc", 
           prob.model = TRUE)
# Extract a and b from the model	
a <- colSums(coef(sv)[[1]] * svmex[SVindex(sv), ])
b <- b(sv)
abline( b / a[1],  -a[2] / a[1], 
        col = "blue") # maximum margin line
text(-1.4, 1.7, labels = "C")
abline( (b + 6.9) / a[1], -a[2] / a[1], lty = 2) # upper margin
abline( (b - 6.9) / a[1], -a[2] / a[1], lty = 2) # lower margin

# circle the support vectors
ai <- alphaindex(sv)[[1]]    # indices for support vectors
points(svmex[ai, ], pch = 1, cex = 2.00)   # three thick circles
points(svmex[ai, ], pch = 1, cex = 2.25)
points(svmex[ai, ], pch = 1, cex = 2.50)

### dev.off()

######################################################

#  Figure 10.7 (Right panel) Introduce SVM 

### pdf(file = "introsvm2.pdf")

library(mvtnorm)

x <- rmvnorm(n = 250, mean = c(0, 0), 
             sigma = matrix(c(1, .1, .1, 1),2 , 2))
wavy <- sin(pi * x[, 1]) / 2               # wavy boundary
include <- (abs( wavy - x[, 2]) > .25 )# 2x width of boundary
x <- x[include, ]                      # include those inside 
wavy <- wavy[include]                 #    the wavy boundary
group <- 1 + (x[, 2] > wavy)          # above or below boundary?
all <- data.frame(x = x[, c(2, 1)], 
   group = group)                     # build a data.frame
colnames(all) <- c("x2", "x1", "group")

sv <- ksvm(group ~ . , data = all, type = "C-svc")

plot(sv, data = all)

### dev.off()

######################################################

## Figure 10.8: SVM on wine data in a pairs() display

### pdf(file = "svwine.pdf")

wines <- read.table("wines.txt", header = TRUE)
levels(wines$Class) <- 1 : 3     # grouping variable
class(wines$Class) <- "factor"   # classify the variable type
colors <- c("green", "red"," blue")[wines$Class]

svw <- ksvm(Class ~ . , data = wines, type = "C-svc", 
   prob.model = T)

svi <- ( c(alphaindex(svw)[[1]], alphaindex(svw)[[2]], 
        alphaindex(svw)[[3]]))   # list of all support vectors
svi <- svi[ !duplicated(svi)]    # remove duplicates

svwine <- wines[svi, ]           # subset support vectors
svwine[, 1] <- jitter(as.integer(svwine[, 1])) # jitter Class
svcol <- colors[svi]             # and their colors

pairs(svwine, pch = 16, cex = .3, gap = 0, col = svcol, 
   xaxt = "n", yaxt = "n")

### dev.off()

#########################################################

# list of support vectors, without duplicates

wines <- read.table("wines.txt", header = TRUE)
library(kernlab)
(svw <- ksvm(Class ~ . , data = wines, 
             type = "C-svc", cross = 5))

svi <- ( c(alphaindex(svw)[[1]], alphaindex(svw)[[2]], 
        alphaindex(svw)[[3]]))  # list of support vectors
svi <- svi[ !duplicated(svi)]   # remove duplicates

######################################################

library(kernlab)
wines$Class <- as.factor(wines$Class)

svw <- ksvm(Class ~ . , data = wines, 
            type = "C-svc", prob.model = T) 
print(predict(svw, wines[, -1], 
              type = "probabilities"), digits = 2)

##########################################################

# Figure 10.9:  Fitted probabilities and residuals for wines

wines <- read.table("wines.txt", header = TRUE)
#levels(wines$Class) <- 1 : 3    # grouping variable
#class(wines$Class) <- "factor"  # classify the variable type
colors <- c("green", "red", "blue")[wines$Class]

library(kernlab)
svw <- ksvm(Class ~ . , data = wines, type = "C-svc", 
            prob.model = T)
prob <- predict(svw, wines[, -1], type = "probabilities")

 pdf(file = "svfitwine.pdf")
 op <- par(mfrow = c(2, 2))

# Fitted probabilities:

plot(prob[, 1], prob[, 3], col = colors, cex = 1.25,
   xlab = "Fitted  probability of  y=1",  
   ylab = "Fitted probability of y=3")
text( rep(.42, 4), c(1, .92,  .84, .76), pos = rep(4, 4),
   col = c("black", "green", "red", "blue"),
   labels = c("Observation codes:", "y=1 category", 
        "y=2 category (ref)", "y=3 category"))
lines( c(.4, .4, 1.2), c(1.2, .7, .7))  #  box around the text

lines(c(0, 1), c(1, 0), lty = "dotted")
lines(c(0, 0), c(1, 0), lty = "dotted")
lines(c(1, 0), c(0, 0), lty = "dotted")

### dev.off()

######  Residual plot

nmlr <- function(fit, obs)
   {       # normalized  multivariate logistic residuals
    nmlr <- matrix(0,nrow = dim(fit)[1], ncol = dim(fit)[2])  
    for (i in 1 : (dim(fit)[1]))
       {
        for (j in 1 : (dim(fit)[2]))
           {
            p <- fit[i, j]              # fitted p parameter
            ob <- obs[i, j]
            res <- (ob - p)  /  (sqrt( p * (1 - p) ) )
            nmlr[i, j] <- res
           }
       }
    return(nmlr)
   }

###  Plot of residuals

obs <- cbind(as.integer(wines$Class == 1),
             as.integer(wines$Class == 3))
colnames(obs) = c("1", "3")
res <- nmlr(prob[, 2 : 3], obs)
plot(res, col = colors, cex = 1.25,
   xlab = "Std residual for fitted probability of y=1",
   ylab = "Std residual for y=3")

 par(op)
 dev.off()

######################################################


      panel.up <- function(x, y, ...)
         {     # function to produce scatterplot in pairs()
          usr <- par("usr")        # save par() values
          on.exit(par(usr))        # set par() at end
                                   # set bounds:
          par(usr = c(max(y), min(y), max(x), min(x))) 
                                   # plot data points:
          points(y, x, col = colorup, pch = 16)        
         }

## Another example with the famous iris data
data(iris)

## Create a kernel function using the build in rbfdot
rbf <- rbfdot(sigma = 0.1)
rbf

## train a bound constraint support vector machine
irismodel <- ksvm(Species ~ . , data = iris, type = "C-bsvc",
                  kernel = rbf,C = 10, prob.model = TRUE)

irismodel

## get fitted values
fitted(irismodel)

## Test on the training set with probabilities as output
predict(irismodel, iris[, -5], type = "probabilities")


#########################################################
#########################################################

# Section 10.5: Regression Trees and Recursive Partitioning

#########################################################
#########################################################

# Regression trees for wines

### pdf(file = "winetree.pdf")
wines <- read.table("wines.txt", header = TRUE)
colors <- c("green", "red", "blue")[wines$Class]
library(mvpart)
winetree <- mvpart(Class ~ . , data = wines, method = "class", 
                   size = 4)

### dev.off()

########################################################

# Classification of crabs   Figure 10.11 and Output 10.5

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

######################################################

#  Plot CP statistics:  Figure 10.13

crabtree <- mvpart(groupch ~ FL + RW + CL + CW + BD, 
                   data = cg, method = "class", size = 20)
crabcp <- printcp(crabtree)

### pdf(file = "crabcp.pdf")
plot(crabcp[,2], crabcp[,4], col = "red", ylim = c(0.0, 1.25),
     xlab = "Number of splits", 
     ylab = "Cross validated errors",
     type = "b", pch = 16, cex = 1.5, cex.lab = 1.5)
lines(crabcp[,2], crabcp[,3], col = "blue", type = "b",
      pch = 16, cex = 1.5)
lines(crabcp[,2], 6 * crabcp[,1], col = "violet", type = "b",
      pch = 16, cex = 1.5)
text(c(5, 4, 3), y = c(.9, .45, .2), 
     col = c("red", "blue", "violet"),
     labels = c("Std error", "Rel error", "CP"), cex = 1.35)
axis(side = 4, tick = TRUE, at = c(1.2, .9, .6, .3, 0) ,
     labels = c(.2, NA , .1, NA, 0), col.axis = "violet", 
     xlab = "CP", col = "violet")
### dev.off()

################################################################

beech <- read.table(file = "beech.txt", header = TRUE)
dim(beech)
beech[1:10,1:6]

library(MASS)
ld  <- lda(Region ~ . , data = beech)
loading <- as.matrix(beech[ ,2 : 27] ) %*% ld$scaling

plot(loading, col = c("blue", "red", NA)[beech[, 1]], 
     pch = 16, cex = .05)


##########################################################

# Regression trees for wines
wines <- read.table("wines.txt", header = TRUE)
colors <- c("green", "red", "blue")[wines$Class]
model <- "Class ~ Alcohol + Malic + Ash + Alcal + Mg + Phenol + Flav +  Nonf + Proan + Color + Hue + Abs + Proline"
library(mvpart)
winetree <- mvpart(Class ~ Alcohol + Malic + Ash + Alcal + Mg + 
                     Phenol + Flav +  Nonf +  Proan + Color + Hue + 
                     Abs + Proline, data = wines, method = "class", 
                   size = 4)


########################################################

#  Univariate example: Characteristics of cars in 1974

### pdf(file = "cartree.pdf")    # Exercise

require(mvpart,datasets,graphics)
univ <- mvpart(mpg ~ cyl + disp + am + carb, data = mtcars) 

### dev.off()


##########################################################
#################  End of this file  #####################
##########################################################

