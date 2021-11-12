###############################################################
###############################################################

#  R programs for Chapter 3

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

###############################################################
###############################################################

#  Simple plot of chi-squared densities (no figure number)

### pdf(file="chisquared1.pdf")  

x <- (0 : 120) / 10
plot (x, dchisq(x, 4), type = "l")
lines(x, dchisq(x, 5))
lines(x, dchisq(x, 6))
lines(x, dchisq(x, 7))
lines(x, dchisq(x, 8))

### dev.off()

##########################################################################

#  Figure 3.1:  Chi-squared densities with labels

 pdf(file = "chisquared2.pdf")    

x <- (0 : 120) / 10
plot(x,dchisq(x, 4), type = "l", cex.lab = 1.25,
    ylab="Chi-squared density")
lines(x, dchisq(x, 5))
lines(x, dchisq(x, 6))
lines(x, dchisq(x, 7))
lines(x, dchisq(x, 8))

text( c(3, 4.2, 5.2, 6.2, 8), c(.18, .153, .137, .123, .11),
    labels=c("4 df", "5 df", "6 df", "7 df", "8 df"))
     
 dev.off()


##########################################################################
##########################################################################

#    Section 3.2: Displays for univariate data

##########################################################################
##########################################################################


library(boot)
data(cd4)
cd4 

stem(cd4$baseline)

library(aplpack)
stem.leaf.backback ( cd4$baseline, cd4$oneyear )


#########################################################################


#  Figure 3.2 Boxplots

library(boot)
data(cd4)
n <- length(cd4$baseline)                  # sample size
 pdf(file = "boxplot.pdf")
op <- par(mfrow = c(1, 4), cex.lab = 1.5)  # four plots and large labels
plot(rep(.5, n), cd4$baseline, axes = FALSE, xlab = "Baseline", 
     xlim = c(0, 1), ylim = range(cd4), col = "red",
     ylab = "CD4 values", cex.lab = 1.5, cex = 2)    
plot(rep(.5, n), cd4$oneyear, axes = FALSE, xlab = "1 year", 
     xlim = c(0, 1), ylim = range(cd4), col = "red",
     yaxt = "n", ylab = "", cex.lab = 1.5, cex = 2) 
text(c(0, 1), c(0, 0), 
      c(boxplot(cd4$baseline, xlab = "Baseline", col = "red", 
            ylim = range(cd4),axes = FALSE),
        boxplot(cd4$oneyear, xlab = "1 year", col = "red",
            ylim = range(cd4), axes = FALSE)))
par(op)
 dev.off()


##########################################################################

# Figure 3.3:   Side-by-side histograms

library(boot)                       # contains the cd4 dataset
data(cd4)
cd4 
### pdf(file="CD4histogram.pdf")
op <- par(mfrow = c(1, 2), cex.lab = 1.25)  #  side by side plots on one page
hist(cd4$baseline, main = "", xlab = "At baseline")
hist(cd4$oneyear, main = "", xlab = "At 1 year")
par(op)
### dev.off()  #   Close CD4histogram.pdf


############################################################################


library(boot)                       # contains the cd4 dataset
data(cd4)
cd4 
### pdf(file = "CD4plot.pdf")   #   Figure 3.4
plot(cd4$baseline,cd4$oneyear,
   xlab = "Baseline", ylab = "At 1 year", cex.lab = 1.25)
### dev.off()


#############################################################################


# Figure 3.5:  Jittered CD4 values and spaghetti

library(boot)                       # contains the cd4 dataset
data(cd4)
cd4 

### pdf("jittered.pdf")
n <- length(cd4$baseline)           # sample size
join <- c(cd4$baseline,cd4$oneyear) # concatenate all CD4 values
group <- rep(c(0, 1), each = n)     # assign group membership
par(mfrow = c(1, 3), cex.lab = 1.5)     # three side by side plots on one page
plot(group, join,                   # 1st plot with simple X axis
   ylab = "CD4 counts", cex.lab = 1.5, xlab = "", axes = FALSE, 
   ylim = c(0, max(join)), xlim = c(-.25, 1.25))
text(c(0, 1), c(1, 1), labels = c("Baseline", "1 year"), cex=1.5)

plot(jitter(group), join,           # 2nd plot with jittered X-axis
   ylab = "", xlab = "", axes = FALSE, 
   ylim = c(0, max(join)), xlim = c(-.25, 1.25))
text(c(0, 1), c(1, 1), labels = c("Baseline", "1 year"), cex = 1.5)

plot(group, join,                   # 3rd plot: spaghetti
   ylab = "", xlab = "", axes = FALSE, 
   ylim = c(0, max(join)), xlim = c(-.25, 1.25))
text(c(0, 1), c(1, 1), labels = c("Baseline", "1 year"), cex = 1.5)
for (i in 1 : n)                      # draw individual connecting lines
  lines(c(0, 1),c(cd4$baseline[i], cd4$oneyear[i]))
### dev.off()  # Close jittered.pdf


####################################################################
#                Section 3.3 Bivariate data 
####################################################################


#  Figure 3.6: Simple plot of housing data with no plot options

housing <- read.table(file = "Housing.txt")
housing

### pdf(file="house1.pdf")
plot(housing, cex.lab = 1.25)
### dev.off()  # close house1.pdf


##########################################################################


#  Figure 3.7

# High data to ink ratio, fringes instead of axes 

# Illustrate plot options: pch=  cex=  and col=
 
housing <- read.table(file = "Housing.txt")
housing

### pdf(file = "house2.pdf")

require(graphics)                           # library for rug program
attach(housing)
boundA <- range(Apartment) * c(.95, 1)      # find bounds for plot area
boundH <- range(House) * c(.8, 1)

plot(housing, xlim = boundA, ylim = boundH, # Plot data with these bounds
    axes = FALSE, cex.lab = 1.5,            # Omit usual axes, larger font
    cex = 1.5, pch = 16,                    # Plot large, solid circles
    col = "red")                            # Select color of points
rug(Apartment, side = 1)                    # Add rug fringes instead of axes
rug(House, side = 2)

### dev.off()  # close house2.pdf


####################################################################


# Small, unlabeled list of graphic plot characters, run into the text

### pdf(file="pch.pdf")

plot(0 : 20, rep(0, 21), pch = 0 : 20,                 # plot the available characters 
   xlab = "", ylab = "", axes = FALSE)                 # without axes, or labels
text(0 : 20, rep(-.1, 21), labels = 0 : 20, cex = .7)  # label each character

### dev.off()

#######################################################################

# Similar plot, to illustrate colors

### pdf(file="col.pdf")

plot((1 : 8) / 2, rep(0 , 8), pch = 15,  col = 1 : 8, # plot the available colors 
   xlab = "", ylab = "", axes = FALSE, cex = 5)   # without axes, or labels
text((1 : 8) / 2, rep(-.2, 8), labels = 1 : 8, cex = .7) # label each character

### dev.off()

#######################################################################

### A rainbow of colors:

### pdf(file="rainbow.pdf")

plot( c(0, 1), c(0, 1), type = "n", xlab = " ", ylab = " ", axes = FALSE)

n <- 125
for (i in 1 : n)
   {
     lines(rep(i / (n + 1), 2),  c(0, 1), lwd = 4, 
       col = rainbow(n)[i])
   }

### dev.off()

###############################################################################

#  Figure 3.8:  Third housing plot with bivariate boxplot and outliers

housing <- read.table(file = "Housing.txt")
attach(housing)

### pdf(file = "house3.pdf")

library(MVA)                              # library for bvbox
extreme <- c( "DC", "HI", "CA")           # Three extreme states 
exst <- match(extreme, rownames(housing)) # identify index in data

bvbox(housing, xlab = "Apartment", ylab = "House", cex.lab = 1.5,
   pch = 19, cex = 1.25, col = "red")
text(housing$Apartment[exst], housing$House[exst], 
   labels = extreme, cex = 1.25,            # label names, small font 
   pos=c(2, 2, 3))                          # position labels: L,L, and top

### dev.off() # close house3.pdf


##########################################################################


# Figure 3.8: Single convex hull of the housing data

### pdf(file = "chull1.pdf")

housing <- read.table(file = "Housing.txt")

ch <- chull(housing)   # find the indices of the convex hull
ch <- c(ch, ch[1])    # loop back to the beginning
plot(housing, pch = 19, col = "red", cex.lab = 1.5,
   cex = 1.25)           # plot the original data
lines(housing$Apartment[ch], housing$House[ch],
   type = "l", col = "green", lwd = 2) # bold, green lines

### dev.off()  # close chull1.pdf

############################################################################

# Figure 3.10: Convex hull and five layers of onion peeling

housing <- read.table(file = "Housing.txt")

### pdf(file = "chullheat.pdf")

library(aplpack)
nlev <- 5            # Number of levels
colors <- heat.colors(9)[3 : (nlev + 2)]
plothulls(housing, n.hull = nlev, col.hull = colors,
   xlab = "Apartment", ylab = "House", cex.lab = 1.5,
   lty.hull = 1 : nlev, density = NA, col = 0, main = "")
points(housing, pch = 16, cex = 1, col = "blue")

### dev.off()

#    solution to Exercise 3.7:
### pdf(file = "chull2.pdf")
#x <- housing$Apartment            # temporary copies of the data
#y <- housing$House
#plot(x, y, xlab = "Apartment", ylab = "House",
#  pch = 19, col = 2, cex = 1.25)  # initial scatterplot
#for (i in 1 : 5)                  # number of onion layers
#    {
#     ch <- chull(x, y)            # indices of convex hull
#     chl <-c (ch, ch[1])          # loop back to the first point
#     lines(x[chl], y[chl], type = "l",
#        col = 3)                  # draw the layer
#     x <- x[ -ch]                 # peel away the layer
#     y <- y[ -ch]
#    }                             # ... and repeat
### dev.off()                      # close chull2.pdf


#############################################################################

#  Figure 3.11:  Bubbleplot map of US 

### pdf(file="USbubblemap.pdf")

JanTemp <- read.table(file = "JanTemp.txt", header = TRUE)
require(MASS)
attach(JanTemp)
longe <- 180 - long                           # Reverse East and West

plot(longe,lat, pch = 16,  cex = .7,  cex.lab = 1.5,  # Plot dots at cities
   xlab = "Longitude, east", ylab = "Latitude", col = "red")                                 
with(JanTemp,symbols(longe,lat, circles = alt,   # Altitude sized circles
   inches = .3, add = TRUE, lwd = 3, fg = "green"))
landmarks <- c( "AK", "HI", "MA", "PR")       # Four landmark states 
lmi <- match(landmarks, state) # identify landmark"s index in data
text(longe[lmi], lat[lmi], labels=landmarks,  # Identify landmarks
   pos=c(1, 1, 4, 3), col = "blue")

### dev.off()  #  close USbubblemap.pdf

############################################################################

#  Figure 3.12:  Kriging map of US altitudes

JanTemp <- read.table(file = "JanTemp.txt", header = TRUE)
JanTemp <- data.frame(JanTemp)

library(MASS)
library(spatial)
attach(JanTemp)

### pdf(file = "USKrig.pdf")
longe <- 180 - long                # Longitude, east
alt.kr <- surf.ls(4, longe, lat, alt) # Kriging surface
altsur <- trmat(alt.kr, 55, 110, 
         28, 50, 50)        # Set limits of plot, excluding outliers
eqscplot(altsur, xlab = "Longitude, east", cex.lab = 1.5,
    ylab = "Latitude",type = "n")    # plot in equal scale coordinates
contour(altsur, levels = c(0, 1000, 2000, 4000), add = TRUE, col = "blue") 
points(longe, lat, pch = 16, col = "red") # add original cities
ex <- c("Miami", "Seattle", "SanFrancisco", 
    "Denver", "Cheyenne")           # List of special cities 
exi <- match(ex, name)              # index for these cities
text(longe[exi], lat[exi], labels = ex, # label special cities
   pos = c(4, 3, 4, 4, 4), cex = 1)

### dev.off()

############################################################################

# Figure 3.12:  Burger scatterplot matrix

burger <- read.table(file = "burger.txt", header = TRUE,
     row.names = 1)

### pdf(file = "burger_pairs.pdf")

pairs(burger, pch = 16, col = "red")

### dev.off()


############################################################################

# Figure 3.13: Smoothed burger scatterplot matrix.

### pdf(file = "burger_pairs2.pdf")

burger <- read.table(file = "burger.txt", header = TRUE,
    row.names = 1)
pairs(burger, lwd = 3, pch = 16, cex = 1.25, col = "red", 
    gap = 0, xaxt = "n", yaxt = "n",
    panel = panel.smooth, col.smooth = "blue")

### dev.off()

###################################################################

#  Figure 3.14: bagplot.pairs

burger <- read.table(file = "burger.txt", header = TRUE,
    row.names = 1)

### pdf(file = "bagplot.pdf")
library(aplpack)
bagplot.pairs(burger[,4 : 7], gap = 0, col.baghull = "green", 
   main = "", pch = 16, cex = 1.25)
### dev.off()

###################################################################

#  Figure 3.15: Burger coplot calories by fat for stratified sodium

### pdf(file = "burger_coplot.pdf")

burger <- read.table(file = "burger.txt", header = TRUE, row.names = 1)

library(graphics)
coplot(Cal ~ Sodium | Fat,  data=burger, 
     rows=1, pch=16, cex=1.75, col="red",
     bar.bg = c(num = "blue", fac = gray(0.95) ))

### dev.off()

#############################################################################

#  Figure 3.16: Star plot of burger data

#   Basic stars:
###  stars(burger, len = 1, key.loc = c(12.5, 1.75),
###   labels = 1:40, cex = .5, key.labels = colnames(burger))

### pdf(file = "burgerstar.pdf")

burger <- read.table(file="burger.txt", header=TRUE,
   row.names=1)

library(graphics)                    # for the stars program
palette(rainbow(7))                  # set colors
burs <- burger[ order(-burger$Cal), ]# order by Cal, decreasing
stars( burs, len=1, cex=0.5, key.loc=c(12.5, 2),
      labels=row.names(burs), draw.segments=TRUE)

### dev.off()

#############################################################################

# Figure 3.17:  Burgers as Chernoff faces

### pdf(file="burger_faces.pdf")

library(aplpack)
faces(burs[1:9,])

### dev.off()


###############################################################################


# Figure 3.19:  Iris plotted as parallel coordinates

### pdf(file="iris_parallel.pdf")

library(MASS)
op <- par(cex.axis = 1.4)
ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3]) 
colnames(ir) <- c( "SepalL", "SepalW", "PetalL", "PetalW")
parcoord(log(ir)[, c(3, 4, 1, 2)], cex.axis = 1.4,  #  Order the axes
   col = c(rep("red",50), rep("green",50), rep("blue",50)))
par(op)

### dev.off()

#############################################################################



#    solution to Exercise 3.7:
### pdf(file = "chull2.pdf")       # Figure 3.20


housing <- read.table(file = "Housing.txt")
x <- housing$Apartment            # temporary copies of the data
y <- housing$House
plot(x, y, xlab = "Apartment", ylab = "House", cex.lab = 1.5,
  pch = 19, col = 2, cex = 1.25)  # initial scatterplot
for (i in 1 : 5)                  # number of onion layers
    {
     ch <- chull(x, y)            # indices of convex hull
     chl <-c (ch, ch[1])          # loop back to the first point
     lines(x[chl], y[chl], type = "l",
        col = 3)                  # draw the layer
     x <- x[ -ch]                 # peel away the layer
     y <- y[ -ch]
    }                             # ... and repeat
### dev.off()                      # close chull2.pdf


##############################################################################
##############################################################################

#-----------------  end of this file ------------------------------
