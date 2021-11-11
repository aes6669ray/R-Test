#########################################################

#    R programs for Chapter 12: Time Series

#########################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

#########################################################

#  Section 12.1: Examples and Simple Analyses

#########################################################

#   Figure 12.1: US Cancer Rates, 1975 - 2007.

acan <- read.table(file = "AnnualCancer.txt", header = TRUE)
acan[1 : 2, ]
ofc <- acan[ , c("Overall", "Wfemale", "Bfemale")] 
ofc[1 : 2, ]
yrange <- range(ofc) + c( -15, 10)

### pdf(file="UScancer.pdf")

plot(acan$Year, ofc[, 1], ylim = yrange, cex.lab = 1.5,
   type = "l", lwd = 2.5, col = rainbow(3)[1],
   xlab = "Year", ylab = "Cancer rate")
lines(acan$Year, ofc[ , 2], col = rainbow(3)[2], 
      lwd = 2.5, type = "l")
lines(acan$Year, ofc[ , 3], col = rainbow(3)[3], 
      lwd = 2.5, type = "l")
text(rep(1977.5, 3), c(420, 335, 383), 
   labels = c("Overall", "White female", "Black female"),
   cex = 1.2)

###  dev.off()

#######################################################

# Figure 12.2: Environmental measures

clim <- read.table(file = "climatechange.txt", header = TRUE)
x <- clim[, 1] + (clim[, 2] - 1 ) / 12    # time axis

### pdf(file="climate.pdf")

ybound <- c(0, 690)
xbound <- c(1971.75, 2011)
co2 <- (clim[, 3] - 333) * 5 + 400
plot(x, co2, xlim = xbound, ylim = ybound, 
     type = "l", cex.lab = 1.25,
     xlab = "Year", ylab = "", yaxt = "n", col = "blue", 
     lwd = 1.5)
text(1974, 410, labels = "CO", cex = 1.25)
text(1975.3, 400, labels = "2", cex = .75)

ip <- 300 + 55 * (clim[, 4] - 4.3) / 12.14
lines(x, ip, type = "l", col = "blue", lwd = 1.5)
text(1974, 325, labels = "Percent\n sea ice", cex = 1.25)

ia <- 200 + 55 * (clim[, 5] - 2.78) / 11.06
lines(x, ia, type = "l", col = "blue", lwd = 1.5)
text(1974, 225, labels = "Area of\n sea ice", cex = 1.25)

snow <- 100 + 50 * (clim[, 6] - 2.32) / 47.55
lines(x, snow, type = "l", col = "blue", lwd = 1.5)
text(1974, 125, labels = "Snow", cex = 1.25)

nino <- 55 * (clim[, 7] - 18.57) / 10.58
lines(x, nino, type = "l", col = "blue", lwd = 1.5)
text(1974, 25, labels = "El Ni\u148o", cex = 1.25)

### dev.off()

#################################################

NYCbirth <- 
  scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
NYCbirth
NYCts <- ts(NYCbirth, frequency = 12, start = c(1946, 1))
NYCts

#rm(NYCsmooth)
NYCsmooth <- ts(cbind( 
     NYCbirth,
     SMA(NYCbirth, n = 6),
     SMA(NYCbirth, n = 20) ),
     frequency = 12, start = c(1946, 1))
colnames(NYCsmooth) <- c("Raw data", "n = 8", "n = 20")
plot(NYCsmooth, xlab = "Year", main = "")

library(TTR)

### plot.ts(SMA (NYCts, n = 4))

####################################################

# Figure 12.3: Time series decomposition: NYC Birth rates

NYCbirth <- 
   scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
print(NYCbirth, digits = 3)

NYCts <- ts(NYCbirth, frequency = 12, start = c(1946, 1))
print(NYCts, digits = 3)

#  Figure 12.3 (Upper) Decomposed components
### pdf(file="NYCbmod1.pdf")

NYCd <- decompose(NYCts)
plot(NYCd, xlab = "Year", col = "red", cex.lab = 1.5, 
     cex.main = 1.75)

### dev.off()

################################################

#  Figure 12.3 (Lower) Observed and fitted trend
### pdf(file="NYCbmod2.pdf")

op <- par(cex.lab = 1.5)
plot(NYCd$x, xlab = "Year", ylab = "NYC birth rate", 
     col = "blue")
lines(NYCd$trend, col = "red", lwd = 3, type = "l")
par(op)

### dev.off()


####################################################

#  Figure 12.4: Moving averages of birth rates

NYCbirth <- 
  scan(file = NYCbirth.txt"")

print(NYCbirth, digits=3)

NYCts <- ts(NYCbirth, frequency=12, start=c(1946,1))
print(NYCts, digits=3)

### pdf(file="NYCma.pdf")
op <- par(cex.lab = 1.25)
plot(NYCts, ylim = c(-5, 30), yaxt = "n", 
   ylab = "", xlab = "Year", cex.lab = 1.25,
   xlim = c(1946, 1963), col = "blue", lwd = 2)
ytext <- NYCts[length(NYCts)]   # last observation
text(1960, ytext, labels = "Original \n data",
     pos = 4, cex = 1.25)
temp <- NYCts

for (i in c(3, 5, 10, 20, 30))
   {
   temp <- temp - 5 
   lines(filter(temp, rep(1 / i, i)), 
         col = "blue", lwd = 2)
   ytext <- ytext - 5
   text(1960, ytext, labels = paste("w =", i), 
        pos = 4, cex = 1.25)
   }
par(op)
### dev.off()

###############################################
###############################################

#      Section 12.2: Autoregression Models

###############################################
###############################################

#  Figure 12.5 Autocorrelation function

library(stats)

### pdf(file="normalacf.pdf") 
acf(rnorm(125), col = "red", lwd = 3, 
   main = "ACF for random normals", cex.lab = 1.5,
   ylab = "Autocorrelation")
### dev.off() 

#################################################

#  Figure 12.6

### pdf(file="acanacf.pdf")   # Center panel: Total US rates
acan <- read.table(file = "AnnualCancer.txt", header = TRUE)
acf(acan$Overall, col = "red", lwd = 3, cex.lab = 1.25,
    ylab = "Autocorrelation",
   main = "ACF for Overall US Cancer Rates")
### dev.off()

###################################################

#  Figure 12.7
### pdf(file="acorf.pdf")   # right: Wfemale and Total US
ccf(acan$Overall, acan$Wfemale, col = "red", lwd = 3, 
    ylab = "Crosscorrelation",
   main = "White Female Rates Lagged by Overall Rates", 
   cex.lab = 1.25)
### dev.off()

##################################################

# Autoregressive residuals in Figure 12.6 - top
#  VAR - vector autoregressive US cancer rates

library(vars)

acan <- read.table(file = "AnnualCancer.txt", 
                   header = TRUE)
ofc <- acan[ ,c("Overall", "Wfemale", "Bfemale")] 
varofc <- VAR( ofc, p = 1, type = "trend" )

summary(varofc)    #   Output printed in Table 12.3

### pdf(file="UScanres.pdf")       # Figure 12.8
res <- residuals(varofc)           # residuals
res <- scale(res)                  # scaled
yr <- range(res)
n <- dim(res)[1]
cols <- dim(res)[2]
plot(acan$Year[-1], res[, 1], pch = 16, cex = 2, 
     cex.lab = 1.25, col=rainbow(cols)[1], ylim = yr, 
     ylab = "Scaled Residual", xlab = "Year")
rug(side = 4, res, ticksize = -.15)
xtitle <- 2002.7                    # locations for key
ytitle <- 2.7
points(xtitle, ytitle, pch = 16, 
       col = rainbow(cols)[1], cex = 2)
text(xtitle + .5, ytitle, 
     labels = colnames(ofc)[1], pos = 4) 
for (i in 2 : cols)
   {
    points(acan$Year[ -1], res[, i], pch = 16, 
       col=rainbow(cols)[i], cex = 2)
    ytitle <- ytitle - .22           # build key
    points(xtitle, ytitle, pch = 16, 
           col = rainbow(cols)[i], cex = 2)
    text(xtitle + .5, ytitle, 
         labels = colnames(ofc)[i], pos = 4)  
   }
lines( c(xtitle - .75, xtitle - .75, max(acan$Year + 3)),
       c(max(res + 5), ytitle - .15, 
         ytitle - .15), type = "l")
lines(c(1970, 2010), c(0,0), lty = 'dotted')

### dev.off()

######################################################

# Test residuals of autoregressive model:

library(vars)
acan <- read.table(file = "AnnualCancer.txt", 
                   header = TRUE)
ofc <- acan[ , c("Overall", "Wfemale", "Bfemale")] 
varofc <- VAR( ofc, p = 1, type = "both" )
res <- residuals(varofc)

normality.test(res)

#######################################################

# Figure 12.8
#  Box test of residuals from autoregression model

acan <- read.table(file = "AnnualCancer.txt", 
                   header = TRUE)
ofc <- acan[ , c("Overall", "Wfemale", "Bfemale")] 
library(vars)
varofc <- VAR( ofc, p = 1, type = "trend")

res <- residuals(varofc)           # residuals
res <- scale(res)                  # scaled

nser <- dim(res)[2]                # Number of series
kmax <- 12                         # maximum no of lags
BoxP <- matrix(1, kmax, nser)      # accumulate p-values    

for (i in 1 : nser)
   {
    for (k in 1 : kmax)
       {
        p <- Box.test(res[, i], lag = k, 
          type = "Ljung-Box")$p.value
        BoxP[k, i] <- p   
       }
   }
BoxP

### pdf(file = "LBtest.pdf")   #  Figure 12.8 bottom panel
plot(1 : kmax, BoxP[, 1], ylim = c(0, 1), 
     col = rainbow(3)[1], type = "b", pch = 16, 
     cex = 2, lwd = 2, ylab = "P-value",
     main = "Ljung-Box test for serial correlation", 
     cex.lab = 1.25, xlab = "Lag")
for (i in 2 : 3)
   lines(1 : kmax, BoxP[, i], col = rainbow(3)[i],
      type = "b", pch = 16, cex = 2, lwd = 2)
lines(c(-1, kmax + 2), c(.05, .05), lty = 2)
### dev.off()

#####################################################

#  Figure 12.9
# A test for stationarity: moving variance estimates

acan <- read.table(file = "AnnualCancer.txt", 
                   header = TRUE)
ofc <- acan[ , c("Overall", "Wfemale", "Bfemale")] 
library(vars)
window <- 15      # moving window model correlations
n <- dim(ofc)[1]  # length of series
start <- acan$Year[1 : (n - window)]
for (i in 1:(n-window)) # move the window
   {
    varofc <- VAR( ofc[i : (i + window), ],
       p=1, type = "trend" )   # fit model
    res <- residuals(varofc) # capture residuals
    v <- var(res)            # variance matrix
    vrow <- c( v[1, 1], v[2, 2], v[3, 3], v[1, 2], 
               v[1, 3], v[2, 3])
    print(i)
    print(vrow)
    if (i == 1) mvars <- vrow
    else mvars <- rbind(mvars, vrow)
    print(i)
   }
mvars <- data.frame(mvars)
mvars <- log(mvars)
colnames(mvars) <- c("Overall", "Wfemale", "Bfemale",
   "Overall_Wfemale", "Overall_Bfemale", 
   "Wfemale_Bfemale") 
mvars

### pdf(file = "movvar.pdf")  # Figure 12.9,  top

plot(start, mvars[ , 1], col = rainbow(6)[1], type = "l",
   ylim = c(2, 5.75), xlab = "Start of Window", 
   cex.lab = 1.25,
   ylab = "Log Variances and Covariances", lwd = 2)
for (j in 2 : (dim(mvars)[2]))
    {
     lines(start, mvars[ , j], col = rainbow(6)[j], 
         type = "l", lwd = 2)
    }
xloc <- 12 + start[1]               # location of color key                       
yloc <- 5.65
text(c(xloc - 4.75, xloc + .25), c(yloc, yloc), pos = 4,
   labels = c("Variances", "Covariances"))
yloc <- yloc - .25
for (j in 1 : 3)
    {
     lines(xloc - 5, yloc, pch = 16, cex = 2, 
           col = rainbow(6)[j], type = "p")
     text(xloc - 4.75, yloc, labels = colnames(mvars)[j], 
          pos = 4)
     lines(xloc, yloc, pch = 16, cex = 2, 
           col=rainbow(6)[j + 3], type = "p")
     text(xloc + .25, yloc, labels = colnames(mvars)[j + 3], 
          pos = 4)
     yloc <- yloc - .25
    }
### dev.off()

#######################################################

# Figure 12.9, bottom panel
#  Moving estimated regression coefficients

acan <- read.table(file = "AnnualCancer.txt", header = TRUE)
ofc <- acan[ ,c("Overall", "Wfemale", "Bfemale")] 
library(vars)
window <- 15      # moving window model correlations
n <- dim(ofc)[1]  # length of series
start <- acan$Year[1 : (n - window)]
for (i in 1 : (n - window)) # move the window
   {
    varofc <- VAR( ofc[i : (i + window), ],  
                   p = 1, type = "trend" )   # fit model
    coeff <- Phi( varofc, nstep = 1)[ , , 2]
    brow <- as.vector(coeff)   # regression coefficients
    if (i == 1) mb <- brow
    else mb <- rbind(mb, brow)
   }
mb

### pdf(file = "movbet.pdf")   # moving regression coefficients

plot(start, mb[ , 1], col = rainbow(6)[1], type = "l",
   xlab = "Start of Window", ylim = range(mb), cex.lab = 1.25,
   ylab = "Estimated Regression Coefficients", lwd = 2)
for (j in 2 : 9)
    {
     lines(start, mb[ , j], col = rainbow(9)[j], 
           type = "l", lwd = 2)
    }
xloc <- 1979.8                         # Key for plot
yloc <- -.75
lines(c(xloc - .5, xloc - .5, 2000), 
      c( -1, yloc + .1, yloc + .1))
for (j in 1 : 9)
   {
    first <- 1 + floor((j - 1) / 3)         # first subscript
    second <- 1 + ((j - 1) %% 3)            # second subscript
    text(xloc, yloc, col = rainbow(9)[j],
      labels = expression(Theta))
    text(xloc + c(.35, .55), rep(yloc - .05, 2), 
         col = rainbow(9)[j],
      labels = c(first, second), cex = .65)
    xloc <- xloc + 1.5
   }
### dev.off()

#########################################################
#########################################################

#  ARIMA models:   Output for Output 12.2

library(TSA)
arima( acan[ ,"Overall"], order = c(1, 0, 0))  # autoregressive
arima( acan[ ,"Overall"], order = c(1, 0, 1))  # ARIMA (1,1)
arima( acan[ ,"Overall"], order = c(2, 0, 2)) # ARIMA (2,2)

fit <- arima( acan[ ,"Overall"], 
              order = c(1, 0, 1)) # ARIMA (1,1)
tsdiag(fit)


#######################################################
#######################################################

##            Section 12.3: Spectral Decomposition

#######################################################
#######################################################

clim <- read.table(file = "climatechange.txt", 
                   header = TRUE)
Date <- clim[ , 1] + (clim[ , 2] - 1) / 12  # time axis
climate <- cbind(Date, clim[ , 3 : 7])
climate[1 : 3, ]

## Figure 12.6  
### pdf(file = "rawperiodogram.pdf")  
op <- par(lwd = 1, yaxt = "n", cex.lab = 1.25)
          #  raw periodogram
spec.pgram(climate[ , -1], detrend = TRUE,    
  ci = -1, sub = "", col = rainbow(5),  lty = 1,
  main = "Raw periodogram of climate data", 
  xlab = "Frequency", ylab = "Spectrum")
par(op)
### dev.off()


### pdf(file = "smoothperiod.pdf")  
op <- par(lwd = 1, yaxt = "n", cex.lab = 1.25)
spec.pgram(climate[ , -1], detrend = TRUE, 
   span = c(9, 9),              # smoothed periodogram 
   ci = -1, sub = "", col = rainbow(5), 
   main = "Smoothed periodogram", xlab = "Frequency",
   ylab = "", lwd = 2, lty = 1 )
##      draw key
    xloc <- .4               # location of top item
    yloc <- 2500
for (i in 1 : 5)
   {
    points(xloc, yloc, pch = 16, 
           cex = 2, col = rainbow(5)[i])
    text(xloc + .01, yloc, labels = colnames(climate)[1 + i], 
         pos = 4)
    yloc <- yloc / 2            # Y-coord for next item
   }
par(op)
### dev.off()

plot(mfdeaths.spc, plot.type = "coherency")

#################################################

NYCbirth <- 
  scan(file = "NYCbirth.txt")

NYCts <- ts(NYCbirth, frequency = 12, start = c(1946, 1))

### pdf(file = "NYCcpgram1.pdf")
op <- par(lwd = 2, yaxt = "n", cex.lab = 1.5, col = "red")
cpgram(NYCts, main="Original data")
par(op)
### dev.off()


### pdf(file = "NYCcpgram2.pdf")
op <- par(lwd = 2, yaxt = "n", cex.lab = 1.5)
cpgram(ar(NYCts, order.max=2)$resid, main = "AR(2) residuals")
par(op)
### dev.off()

###############################################
#             Chapter 12 Exercises
###############################################

Exercise 12.7: Tests of stationarity

acan <- read.table(file = "AnnualCancer.txt", 
                   header = TRUE)
ofc <- acan[ , c("Overall", "Wfemale", "Bfemale")] 
library(vars)
varofc <- VAR( ofc, p = 1, type = "trend")
res <- residuals(varofc)

PP.test(res[ , 1]) # Phillips-Perron test
PP.test(res[ , 2])
PP.test(res[ , 3])

library(tseries)
kpss.test(res[ , 1]) # KPSS test
kpss.test(res[ , 2])
kpss.test(res[ , 3])

adf.test(res[ , 1])
adf.test(res[ , 2])
adf.test(res[ , 3])


##################################################
#                   End of this file
##################################################

