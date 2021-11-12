
#   Figures and computing for Chapter 1

############################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

############################################################

#    Figure 1.1: Joint confidence intervals for housing data

housing <- read.table(file = "Housing.txt")
housing
housing$Apartment

bivCI <- function(s, xbar, n, alpha, m)
#  returns m (x,y) coordinates of 1-alpha 
# joint confidence ellipse of mean

   {
                           # m points on a unit circle  
    x <- sin(2 * pi * (0 : (m - 1)) / (m - 1))  
    y <- cos(2 * pi * (0 : (m - 1)) / (m - 1))
    # chisquared critical value
    cv <-  qchisq(1 - alpha, 2)
    # value of quadratic form
    cv <- cv / n                                 
    for (i in 1 : m)
       {
         pair <- c(x[i], y[i])        # ith (x,y) pair
         q <- pair %*% solve(s, pair) # quadratic form
         x[i] <- x[i] * sqrt(cv / q) + xbar[1]
         y[i] <- y[i] * sqrt(cv / q) + xbar[2]
       }
    cbind(x, y)
   }


### pdf(file = "housingCI.pdf")

aptci <- t.test(housing$Apartment)$conf.int
hoci <- t.test(housing$House)$conf.int

plot(bivCI(var(housing), mean(housing), 
     dim(housing)[1], .05, 2000), lwd = 3,
     type = "l", xlab = colnames(housing)[1], 
     ylab = colnames(housing)[2], col = 2,
     xlim = aptci * c(.97, 1.03),
     ylim = hoci * c(.96, 1.04))

lines(mean(housing)[1], mean(housing)[2], pch = 3, 
      cex = 2, type = "p", col = 4, lwd = 3)

lines(aptci[c(1, 2, 2, 1, 1)], hoci[c(1, 1, 2, 2, 1)], 
      type = "l", col = 3, lwd = 3)

### dev.off()

#############################################################

# Shapley galaxy data

galaxy <- read.table(
  "http://astrostatistics.psu.edu/datasets/Shapley_galaxy.dat", 
  header=TRUE)

galaxy[1 : 5, ]

galaxy <- galaxy[galaxy$Mag > 0, ] # Omit missing values?

### pdf(file = "galaxy.pdf")
plot(galaxy$R.A., galaxy$Dec., pch = 16, cex = .5,
   xlab = "Right ascension",
   ylab = "Declination",
   col=c("blue", "red")[1 + (galaxy$Mag < 16)])
### dev.off()

##########################################################

###pdf(file = "PISA.pdf")

PISA <- read.table(file = "OECD PISA.txt", 
                   row.names= 1, header=TRUE)
pairs(PISA, gap = 0, col="red", pch=16, xaxt="n", yaxt="n")

###dev.off()

################################################################
###################  End of this file  #########################
################################################################
