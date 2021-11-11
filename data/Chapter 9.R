###########################################################################

#               Chapter 9: Linear Regression

############################################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

############################################################################

#  Univariate example: Characteristics of cars in 1974

#  Figure 9.1: Univariate fitted and residuals of mpg

library(datasets)

univ <- lm(formual = mpg ~ cyl + wt + am + carb , data = mtcars) 
summary(univ)
coefficients(univ)
fit <- univ$fitted.values
resid <- univ$residuals

boundx <- c(.9 * min(fit), 1.1 * max(fit)) # find bounds for plot area
boundy <- c(min(resid) * 1.15, max(resid))

r1  <- -sort(-resid)[1]           # find largest positive residual
ext <- match(r1, resid)           # identify which observations
r2  <- sort(resid)[1 : 2]         # two most-negative residuals
ext <- c(ext,match(r2,resid))     # indices of three extreme residuals


### pdf(file = "mpg_resid.pdf")   #   Figure 9.1

library(graphics)
plot(fit, resid,                    
    xlim = boundx, ylim = boundy, # set bounds of plot
    xlab = "Fitted values", ylab = "Residuals",
    axes = FALSE, pch = 16, cex = 3,  # Omit axes, large bold circles
    col = "red", cex.lab = 1.5)   # choice of color, larger label font
lines(boundx, c(0, 0))            # horizontal axis line

rug(fit, side = 1)                # Add pale fringes instead of axes
rug(resid, side = 2)

text(fit[ext], resid[ext],        # identify unusually small fitted 
   cex = 0.8, pos = c(2, 3, 2),   # small text 
   labels = rownames(mtcars)[ext])  # labels for points

### dev.off()

univ.glm <- glm(formula = mpg ~ cyl + wt + am + carb , data = mtcars) 
summary(univ.glm)
tt <- summary(univ.glm)
print( tt$coefficients, digits=4)



#####################################################################

#  Section 9.3:   Multivariable regression
                          #   y = mpg
univ.mpg <- aov(mpg ~ cyl + disp + am + carb, data = mtcars)   
summary(univ.mpg)
coefficients(univ.mpg)
fit1 <- univ.mpg$fitted
res1 <- univ.mpg$residuals

                          #  y = hp
univ.hp <- aov( hp ~ cyl + drat + am + gear + carb, data = mtcars) 
summary(univ.hp)
coefficients(univ.hp)

                          #   y = wt
univ.wt <- aov( wt ~ cyl + disp + drat + am + carb, data = mtcars) 
summary(univ.wt)
coefficients(univ.wt)

                          #  y = qsec
univ.qsec <- aov( qsec ~ cyl + disp + drat + vs + am + gear , data = mtcars)
summary(univ.qsec)
coefficients(univ.qsec)


####################################################################

#   Figure 9.2:  four fitted and residual plots

### pdf(file = "car4.pdf")

op <- par(mfrow = c(2, 2))    #  2x2 plots of residuals and fitted values


# ------------------  1 st of 4:  mpg   -----------------------


fit1 <- univ.mpg$fitted
res1 <- univ.mpg$residuals

bound1<- c(min(fit1) * .9, max(fit1)) # find bounds for plot area
bound2<- c(min(res1) * 1.25, max(res1))

plot(fit1 , res1, 
    xlab = "Fitted mpg", ylab = "residuals",
    xlim = bound1, ylim = bound2,     
    axes = FALSE, pch = 16, cex = 1.25, # Omit axes, large bold circles
    col = "red")                    # choice of color
lines(bound1, c(0, 0))             # horizontal axis line
rug(fit1, side = 1)                 # Add fringes instead of axes
rug(res1, side = 2)

r1  <- -sort(-res1)[1]            # find largest residual
ext <- match(r1, res1)             # identify which observations
r2  <- sort(res1)[1 : 2]            # two most negative residuals
ext <- c(ext, match(r2, res1))      # indices of extreme residuals

text(fit1[ext], res1[ext],        # identify unusually small fitted 
   cex = 0.7, pos = c(2, 3, 2),         # small text 
   labels = rownames(mtcars)[ext])  # labels for points


# ------------------   2-nd of 4:  hp   ---------------------


fit2 <- univ.hp$fitted
res2 <- univ.hp$residuals

bound1<- c(min(fit2) * .9, max(fit2)) # find bounds for plot area
bound2<- c(min(res2) * 1.25, max(res2))

plot(fit2 , res2, 
    xlim = bound1, ylim = bound2,
    xlab = "Fitted hp", ylab = "residuals",
    axes = FALSE, pch = 16, cex = 1.25, # Omit axes, large bold circles
    col = "red")                    # choice of color
lines(bound1, c(0, 0))             # horizontal axis line
rug(fit2, side = 1)                 # Add fringes instead of axes
rug(res2, side = 2)

r2  <- -sort(-fit2)[1]            # find largest fitted
ext <- match(r2, fit2)             # identify which observation
r2  <- sort(fit2)[1]              # find smallest fitted
ext <- c(ext, match(r2, fit2))      # identify which observation
r2 <- sort(res2)[1]               # largest negative residual
ext <- c(ext,match(r2,res2))      # list of extreme points

text(fit2[ext], res2[ext],        # identify unusual in plot
   cex=0.7, pos=c(2,4,2),         # small text 
   labels=rownames(mtcars)[ext])  # labels for point


# -------------------   3-rd of 4: wt   -----------------


fit3 <- univ.wt$fitted           #  Weight residual plot
res3 <- univ.wt$residuals

bound1<- c(min(fit3)*.9,max(fit3)) # find bounds for plot area
bound2<- c(min(res3)*1.25, max(res3))

plot(fit3 , res3, 
    xlim=bound1, ylim=bound2,
    xlab="Fitted wt", ylab="residuals",
    axes=FALSE, pch=16, cex=1.25,  # Omit axes, large bold circles
    col="red")                     # choice of color
lines(bound1, c(0,0))              # horizontal axis line
rug(fit3, side=1)                  # Add fringes instead of axes
rug(res3, side=2)

r3  <- sort(res3)[1:2]             # find 2 largest negative residuals
ext <- match(r3,res3)              # identify which observation

text(fit3[ext], res3[ext],         # identify unusual in plot
   cex=0.7, pos=c(4,3),            # small text 
   labels=rownames(mtcars)[ext])   # labels for point


# ------------------   4-th of 4: qsec   --------------------


fit4 <- univ.qsec$fitted
res4 <- univ.qsec$residuals

bound1<- c(min(fit4)*.92,max(fit4)) # find bounds for plot area
bound2<- c(min(res4)*1.25, max(res4))

plot(univ.qsec$fitted, univ.qsec$residuals, 
    xlim=bound1, ylim=bound2,
    xlab="Fitted qsec", ylab="residuals",
    axes=FALSE, pch=16, cex=1.25,  # Omit axes, large bold circles
    col="red")                     # choice of color
lines(bound1, c(0,0))              # horizontal axis line
rug(fit4, side=1)                  # Add fringes instead of axes
rug(res4, side=2)

r41  <- -sort(-res4)[1]           # find largest residual
ext <- match(r41,res4)            # identify which observation

text(fit4[ext], res4[ext],        # identify unusual in plot
   cex=0.7, pos=2,                # small text 
   labels=rownames(mtcars)[ext])  # labels for point

r42  <- sort(fit4)[1:2]           # find 2 smallest fitted values
ext <- match(r42,fit4)            # identify which observation

text(fit4[ext], res4[ext]-c(0,.2), # identify unusual in plot
   cex=0.7, pos=c(3,1),           # small text 
   labels=rownames(mtcars)[ext])  # labels for point

par(op)

### dev.off()


###################################################################


# Figure 9.3:  Multivariate residuals for car data

univ.mpg$residuals

car.res<-cbind(univ.mpg$residuals,
               univ.hp$residuals, 
               univ.wt$residuals, 
               univ.qsec$residuals)   #  build data.frame of residuals
colnames(car.res)<- c("mpg_res", "hp_res", "wt_res", "qsec_res")
car.res<-data.frame(car.res)            
print(car.res, digits=2)

### pdf(file="car4res.pdf")
pairs(car.res, panel = panel.smooth, 
    col.smooth="blue", pch=16, cex=2, lwd=2,
    col="red", gap=0.25, xaxt="n", yaxt="n") 

### dev.off()


####################################################################
####################################################################


#   Test multivariate normality of residuals

mah <- mahalanobis(car.res, colMeans(car.res), var(car.res))
n <- dim(car.res)[1]                # number of rows       
ncols <- dim(car.res)[2]            # number of data columns
print(ncols)                        # check # of cols


### pdf(file="carmah.pdf")          # QQ plot of Mahalanobis distances 

out <- match(max(mah), mah)         # identify one large outlier
qqplot(qchisq((1:n)/(n+1), df = ncols), mah, 
    pch=16, cex=2, col=2, xlim=c(0, 11), cex.lab=1.5,
    xlab = "4 df Chi-squared quantile",
    ylab = "Mahalanobis distance from the sample mean") 
abline(0, 1, col = "green", lwd = 4) # connect the quartiles
text(qchisq(n/(n+1), df=ncols), max(mah), 
   labels=names(mah)[out], pos=2)   # label the outlier

### dev.off()

### Test of this QQ plot:

shapiro.test(qnorm(pchisq(mah,ncols)))

#   Skewness and kurtosis of car residuals

library(psych)
mardia(car.res)

#  Multivariate Shapiro-Wilk test of car residuals

library(mvnormtest)
mshapiro.test( t( car.res))

#  Omit outlier and test again:

outi <- match(max(mah),mah)  # index of outlier
mshapiro.test( t( car.res[-outi,] ))


####################################################################
####################################################################


####    Figure 9.4: Principal components of residuals

print(prcomp(car.res, scale = TRUE), digits = 3)

### pdf(file = "carrespc.pdf")

biplot(prcomp(car.res, scale = TRUE), cex = c(.6, 1.2), 
   col = c(3, 4), cex.lab=1.5,
   xlab = "First principal component",
   ylab = "Second principal component")

### dev.off()


###################################################################
###################################################################

#                    Section 9.4: BRFSS Data  

###################################################################
###################################################################


#     Behavioral Risk Factor Surveilance System (BRFSS)

# read data and make data frame

BRFSS<-read.table("BRFSS.txt", header=TRUE)
BRFSS<-data.frame(BRFSS[,2:19],row.names=BRFSS[,1])

out <- c(2, 3, 5, 6, 7, 8, 11, 14)   # health outcome variables
beh <- c(1, 4, 9, 10, 12, 13, 15, 16, 17, 18) # behavioral measures
BRFSS[1,out]
BRFSS[1,beh]

###################################################################

#   figure 9.5: Pair for outcome variables

 pdf(file="BRFSSout.pdf")    #  works correctly with WARNINGS
pairs(BRFSS[, out], panel = panel.smooth, 
      col.smooth = "blue", pch = 16, cex = .5,
      col = "red", gap = 0, xaxt = "n", yaxt = "n")
 dev.off()

########################################################################


univ.As <- glm(Asthma ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.As)
coefficients(univ.As)


univ.HA <- glm(HeartAt ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)
summary(univ.HA)
coefficients(univ.HA)


univ.Dia <- glm(Diabetes ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.Dia)
coefficients(univ.Dia)


univ.Preg <- glm(PregDaib ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.Preg)
coefficients(univ.Preg)


univ.Pre <- glm(PreDia ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.Pre)
coefficients(univ.Pre)


univ.Dis <- glm(Disability ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.Dis)
coefficients(univ.Dis)


univ.GH <- glm(GeneralH ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.GH)
coefficients(univ.GH)


univ.T <- glm(Teeth ~ 
  Binge+Colonosc+Exercise+Coverage+FluShot+Dentist+BMI+PSA+Smokers+PAP, 
  data=BRFSS)   
summary(univ.T)
coefficients(univ.T)

#####################################################################


# Figure 9.6: Heatmap of statistical significance

tt<- cbind(                # capture all t statistics
       summary(univ.As)$coefficients[,3],
       summary(univ.HA)$coefficients[,3],
       summary(univ.Dia)$coefficients[,3],
       summary(univ.Preg)$coefficients[,3],
       summary(univ.Pre)$coefficients[,3],
       summary(univ.Dis)$coefficients[,3],
       summary(univ.GH)$coefficients[,3],
       summary(univ.T)$coefficients[,3]
          )
          # assign column names of dependent variables

colnames(tt) <- colnames(BRFSS[, out])

### pdf(file="regheat.pdf")  #  Table 9.6 

heatmap(sqrt(abs(tt)), scale="none")

### dev.off()

######################################################################

#  BRFSS residuals

BRFSS.resid <- cbind( 
         univ.As$residual,   univ.HA$residual,  univ.Dia$residual,
         univ.Preg$residual, univ.Pre$residual, univ.Dis$residual, 
         univ.GH$residual, univ.T$residual)  # combine all residuals

colnames(BRFSS.resid) <- colnames(BRFSS[,out]) # inherit variable names
rownames(BRFSS.resid) <- rownames(BRFSS)  #  inherit city names
BRFSS.resid <- data.frame(BRFSS.resid)    #  convert to data.frame

######################################################################

# Output 9.2: Correlation matrix of BRFSS residuals

cr <- cor(BRFSS.resid)
print(cr, digits = 2)

########################################################################

# Figure 9.7

### pdf(file="BRFSSresid.pdf")   #  Matrix scatter plot of all residuals

pairs(BRFSS.resid, pch = 16, cex = .5, ### Works correctly with warnings
   col = "red", gap = 0, xaxt = "n", yax = "n", 
   panel = panel.smooth, col.smooth = "blue")

### dev.off()

########################################################################

# Figure 9.8: Scatter plots of Disability and general health, 
#        as raw data and as residuals

library(MVA)

### pdf(file="BRFSSdgh.pdf")

op <- par(mfrow=c(2,2))

ext <- "HuntingtonWV"   # name of extreme city
exti <- match(ext, rownames(BRFSS.resid))    # index of extreme
ybound <- c(.99*min(BRFSS$GeneralH), max(BRFSS$GeneralH))
xbound <- c(.925*min(BRFSS$Disability), max(BRFSS$Disability))
plot(BRFSS$Disability, BRFSS$GeneralH, 
   xlim=xbound, ylim=ybound, pch=16, cex=1, col="red",
   xlab="Disabilty", ylab="General Health")
#bvbox(cbind(BRFSS$Disability, BRFSS$GeneralH), 
#   pch = 16, cex = 1, col = "red",
#   xlab = "Disability", ylab = "General health")
rug(BRFSS$Disability, side = 1)         # marginal distributions 
rug(BRFSS$GeneralH, side = 2)
text(BRFSS$Disability[exti], BRFSS$GeneralH[exti],
   labels = ext, cex = .6, pos = 2) 

ext <- c("HuntingtonWV", "FayettevilleAR")   # names of extreme cities
exti <- match(ext, rownames(BRFSS.resid))    # indices of extremes
xbound <- c(1.075 * min(BRFSS.resid$Disability),
                 max(BRFSS.resid$Disability))
ybound <- c(1.075 * min(BRFSS.resid$GeneralH), 
                max(BRFSS.resid$GeneralH))
plot( BRFSS.resid$Disability, BRFSS.resid$GeneralH, 
   pch = 16, cex = 1, col = "red", ylim = ybound, xlim = xbound,
   xlab = "Disability residual", ylab = "General health residual")
#bvbox(cbind(BRFSS.resid$Disability, BRFSS.resid$GeneralH), 
#   pch = 16, cex = 1, col = "red",
#   xlab = "Disability residual", ylab = "General health residual")

rug(BRFSS.resid$Disability, side = 1)         # marginal distributions 
rug(BRFSS.resid$GeneralH, side = 2)
text(BRFSS.resid$Disability[exti], BRFSS.resid$GeneralH[exti],
   labels = ext, cex = .6, pos = c(2, 4)) 

lines(2 * xbound, c(0, 0), type = "l")           # origin axes for residuals
lines(c(0, 0), ybound * c(1.2, .95), type = "l")

par(op)

### dev.off()

########################################################################

#   Figure 9.8: Biplot of BRFSS residuals

Bpc <- princomp(BRFSS.resid)
### summary(Bpc)

pdf(file = "BRFSSbiplot.pdf")
biplot(Bpc, cex = c(.4, 1), col = c(3,2), xlim = c(-.22, .24),
   xlab = "First principal component",
   ylab = "Second principal component")
### dev.off()

############################################################################

tt<- cbind(                # capture all t statistics
       summary(univ.As)$coefficients[,3],
       summary(univ.HA)$coefficients[,3],
       summary(univ.Dia)$coefficients[,3],
       summary(univ.Preg)$coefficients[,3],
       summary(univ.Pre)$coefficients[,3],
       summary(univ.Dis)$coefficients[,3],
       summary(univ.GH)$coefficients[,3],
       summary(univ.T)$coefficients[,3]
          )
          # assign column names of dependent variables
colnames(tt) <- c( "As", "HA", "Dia", "Preg",
       "Pre", "Dis", "GH", "T")
dt <- dim(tt)                     # dimensions of beta matrix

xvals <- sort(rep(1 : dt[2], dt[1]))      # column values
yvals <- 1 + dt[1] - rep(1 : dt[1], dt[2]) # rows
colors <-  c(2, 3)[1 + (tt > 0)]       # red or green
symb <- c(0, 15)[1 + (abs(tt) > 2)]   # empty or full box
plot(xvals, yvals, cex = 4.5, pch=symb, col=colors,
    ylim=c(.5, .5 + dim(tt)[1]))

#########################################################################

#  Exercises

#  Cost of living in the 50 states

cost <- read.table(file = "costofliving.txt", header = TRUE, row.names = 1)
cost[1:3,]

univ.r <- glm(rent ~  pop + income, data = cost)   
summary(univ.r)
coefficients(univ.r)

univ.h <- glm(house ~ pop+income, data = cost)
summary(univ.h)
coefficients(univ.h)

univ.c <- glm(COL ~ pop + income, data = cost)
summary(univ.c)
coefficients(univ.c)

cost.resid <- cbind(    #  combine three sets of residuals
         univ.r$residual,   univ.h$residual,  univ.c$residual)
colnames(cost.resid) <- c( "rent", "house", "COL")
cost.resid[1:3,]

pc.cost <- princomp(cost.resid, cor = TRUE)

### pdf(file = "costbp.pdf")

biplot(pc.cost, cex = c(.6,1), col = c(4,2), xaxt = "n", yaxt = "n",
   xlab = "First principal component", xlim=c(-.75, .3),
   ylab = "Second principal component")

###dev.off()



########################################################################


#  Exercise 9.4: Mining fatalities


### pdf(file = "miners.pdf")
coal <- read.table(file = "coal mining.txt", header = TRUE)
plot(coal[,1], log(coal[,2]), pch = 16, col = "red", cex.lab = 1.5,
   xlab = "Year", ylab = "Log scale", ylim = c(2, 14))
lines(coal[,1], log(coal[,3]), pch = 16, col = "blue", type = "p" )
text(c(2000, 2000), c(11, 5), labels = c("Miners", "Fatalities"), cex = 1.25)
### dev.off()


########################################################################


#   Exercise 9.5 Ramus (jaw) bones


setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

jaw <- read.table(file = "Ramus.txt", header = TRUE, row.names = 1)
jaw
n <-  dim(jaw)[1]
### pdf(file = "ramus.pdf")
plot(x = NA, type = "n", xlim = c(8, 9.5), ylim = c(min(jaw), max(jaw)),
xlab = "Age", ylab = "Ramus", cex.lab = 1.5)
age <- c(8., 8.5, 9, 9.5 )
longa <- NULL
longj <- NULL
for (i in 1:n)
  {
    longa <- c(longa, age)
    ramus <- jaw[i,]
    longj <- c(longj, as.double(ramus))
    lines (age, ramus, col = "red")
  }
lines(longa, longj, pch = 16, col = "blue", type = "p", cex = .8)
model <- lm(longj ~ longa)
fit <- model$coefficients[1] + model$coefficients[2] * age
lines ( age, fit, type = "l", col = "green", lwd = 3)
###      dev.off()
jres <- jaw - fit               # residuals
sapply(jres, sd)
      
#########################################################################

#  Stepwise regression

library(datasets)
library(MASS)
data(mtcars)
univ <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
backward <- stepAIC(univ)
summary(backward)


#########################################################################
#####################  end of this file #################################
#########################################################################
