
#  Chapter 8: Factor Methods

########################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")
rm(list = ls())


########################################################

#  Table 8.1: Investment allocations

invest <- read.table(file = "invest_alloc.txt", 
   header = TRUE, row.names = 1)

pc <- princomp(invest)
summary(pc)
pc$loadings

##########################################################

#  Figure 8.1: Investment allocation screeplot

### pdf(file = "invest_scree.pdf")

op <- par(cex.axis = 1.5, cex.lab = 1.5)
screeplot(pc, col = "blue", pch = 16, 
   type = "lines", cex = 2, lwd = 2, main = " ")
par(op)
### dev.off()

###########################################################

#  Figure 8.2: Investment allocation biplot

### pdf(file = "invest_biplot.pdf")

pc <- princomp(invest)
biplot(pc, col = c(2,3), cex = c(.75, 1.5),
   xlim=c(-.45, .45),
   xlab = "First principal component",
   ylab = "Second principal component",
   main = "Biplot for investment allocations")

### dev.off()

########################################################

# Kuiper belt objects

Kuiper <- read.table(file = "Kuiper.txt", 
   header = TRUE, row.names = 1)

KuiPC <- princomp(Kuiper)
summary(KuiPC)

KuiPC$loadings

sapply(Kuiper, sd)

KuiPCc <- princomp(Kuiper, cor = TRUE)
summary(KuiPCc)

KuiPCc$loadings

#########################################################

#   Figure 8.3: Kuiper belt objects biplot

### pdf(file = "Kuiper_pc.pdf")

biplot(KuiPCc, col = c(2, 3), cex = c(.75, 1.5), 
   xlab = "First principal component", cex.lab = 1.5,
   ylab = "Second principal component",
   main = "Biplot for Kuiper objects")

### dev.off()

##########################################################
##########################################################

# US hospitals

hosp <- read.table(file = "USHospitals.txt", 
                   header = TRUE, row.names = 1)
# remove rows with any NA's
hosp <- hosp[complete.cases(hosp),]  

phosp <- princomp(hosp, cor = TRUE)
summary(phosp)

### pdf(file = "screehosp.pdf")
op <- par(cex.lab = 1.5, cex.axis = 1.5)
screeplot(phosp, type = "l", col="red", pch = 16, 
          cex.lab = 1.5, cex.axis = 1.75, cex = 3, 
          lwd = 2, main = "Screeplot for US hospitals")
par(op)

### dev.off()

print(phosp$loadings[ , 1], digits = 2)


########################################################
########################################################

###    Section 8.2  Factor Analysis

########################################################
########################################################

hemangioma <- read.table(file = "hemangioma.txt", header = T)

factanal(hemangioma,factors = 3 )

########################################################

burger <- read.table(file = "burger.txt", header = T)
factanal(burger[, -1], factors = 2)  #  Omit burger number

factanal(burger[, -1], factors = 3)  # Now try three factors

########################################################


## Principal components:oil consumption data

oil <- read.table(file = "oil_consumption.dat", row.names = 1, 
                  header = TRUE)
oil
pairs(oil, col = "blue", pch = 16, cex = 1.3)

pcoil <- princomp(oil, cor = TRUE)
pcoil
screeplot(pcoil, type = "l", col="red", pch = 16, 
  cex = 2, lwd = 2, main = "Screeplot for oil consumption")

pcoil$loadings


### pdf(file = "oilbiplot.pdf")
biplot(pcoil,  cex = c(.6, 1), col = c(4, 2), xaxt = "n", 
  yaxt = "n",
  xlab = "First principal component",
  ylab = "Second principal component",
  main = "Biplot for oil consumption data")

### dev.off()


oil2 <- oil[,c(-2, -5)]
pairs(oil2, col = "blue", pch = 16, cex = 1.3)
pcoil2 <- princomp(oil2)
screeplot(pcoil2, type = "l", col = "red", pch = 16, 
  cex = 2, lwd = 2, 
  main = "Screeplot for abbreviated oil consumption")
pcoil2$loadings

#############################################################
#####################  End of this file  ####################


#  Factor analysis on cars

library(datasets)

fa <- factanal( ~ mpg + cyl + disp + hp + wt + qsec,
   data=mtcars, factors=3, 
   scores='regr', rotation='varimax')

f1 <- fa$scores[,1]

plot(f1, fa$scores[,2], pch=16, col='red', cex=1.6, 
   xlim=c(-1.7,2), ylim=c(-3, 2))
names <- row.names(data.frame(f1))
text(labels=names, fa$scores[,1], fa$scores[,2], pos=1, cex=.6)

###############################################################
###############################################################
###############################################################
