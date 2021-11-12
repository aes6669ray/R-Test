############################################################

#                    Chapter 11: Clustering

############################################################

setwd("C:/Users/zeldan/Desktop/Projects/Multivariate/Data")

############################################################


#  Canadian healthcare workers  Fig 11.1

Cmed <- read.table(file = "Canmed.txt", 
    header = TRUE, row.names = 1)

Cmed <- Cmed[-14 , ]      # omit  "Canada total" row

d <- dist(scale(Cmed))                  #

### pdf(file = "Cmed.pdf")

clust <- hclust(d)
plot(clust, xlab = " ", sub = " ", 
     main = " ", ylab = " ", yaxt = "n")

### dev.off()


#########################################################

#   Figure 11.2: Cluster iris data

### pdf(file  = "irisclust.pdf")

id <- dist(scale(iris[ , 1 : 4]))
fit <- hclust(id)   

library(ape)               # graphics for trees
plot(as.phylo(fit), type = "phylogram", cex = .5, 
     label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), 
                rep("green" , 50)))

### dev.off()

###############################################

#  Figure 11.3:  Baseball teams and six different displays

### pdf(file = "bbdend.pdf")

baseball <- read.table(file = "baseball_teams.txt", 
                       header = TRUE, row.names = 1)

league <- baseball[, 1]  
division <- baseball[, 2]

league

division

d <- dist(scale(baseball[, c(-1, -2)])) # scale values but 
                                # not league or division
library(ape)               # variety of plotting options
fit <- hclust(d)

op <- par(mfrow = c(3, 2))      # 3x2 plots on one one graph

# four plots on the top:
plot(fit, main = "Plain dendrogram", hang = -1, cex = .8,
   xlab = " ", ylab = " ", sub = " ", axes = FALSE)   
plot(as.dendrogram(fit), type = "triangle",
   main = "Triangle branches", cex = .8, axes = FALSE)
plot(as.phylo(fit), type = "unrooted", cex = .8, 
   main = "Unrooted tree")
plot(as.phylo(fit), type = "fan", cex = .8,
   main = "Leaves spread in a circle")

#    methods for different colored leaves

fit <- as.dendrogram(hclust(d))

# function to get color labels
colLab <- function(n)         # function on dendrograms 
   {
    if (is.leaf(n))           # when we find a leaf
      {
       a <- attributes(n)     # capture its attributes
                              # which team number is it? 
       teamn <- match(a$label, row.names(baseball))
                              # color for league or division
       leafcolor <- c("red", "blue")[teamtype[teamn]]
                              # assign new attributes to leaf 
       attr(n, "nodePar") <- 
                c(a$nodePar, lab.col = leafcolor)
      }
    n                         # return the dendrogram
   }


# Two panels on the bottom of the figure:
teamtype <- league
colfit = dendrapply(fit, colLab) # dendrapply to tree
plot(colfit, main = "Color by league", 
   cex = .8, axes = FALSE) 

teamtype <- division
colfit = dendrapply(fit, colLab)
plot(colfit, main = "Color by division", 
   cex = .8, axes = FALSE)

par(op)

### dev.off()

#######################################################

library(ape)
data(chiroptera)

### pdf(file = "bat1.pdf")
plot(chiroptera, type = "r", cex = .01)
###dev.off()

###pdf(file = "bat2.pdf")
plot(chiroptera, type = "u", cex = .01)
###dev.off()

#######################################################

##########  cover art  ################################

library(ape)
data(chiroptera)
### pdf(file = "cover_art.pdf")
plot(chiroptera, type = "u", cex = 0.01)
tiplabels(col = "red", pch = 16, cex = .5)
edgelabels(col = "green", pch = 16, cex = .5)
nodelabels(col = "blue", pch = 16, cex = .5)
### dev.off()

######################################################
######################################################
#            Section 11.2: K-means                      
######################################################
######################################################

# Figure 11.5: Japanese smokers

smokers <- read.table(file = "smokers.txt", row.names = 1)
colnames(smokers) <- c("Age", "Ht", "Wt", "Nico", "Tar",
   "Astart", "Ncigs", "Nyears", "PkYr")
smokers

require(graphics, stats)

### pdf(file = "smokerk.pdf") 

(cl <- kmeans(smokers, centers = 4))
plot(smokers[,c(3,9)], xlab = "Weight in kg",
   ylab = "Pack  years", cex.lab = 1.5,
   pch = 16, col = cl$clust, cex = 2.5)
text(smokers[, 3], smokers[, 9], labels = row.names(smokers),
  pos = 2, cex = 1.5)

### dev.off()

########################################################
########################################################

#  Figure 11.6: K-means percent variability and 
# within cluster sum of squares in Japaneese smokers

smokers <- read.table(file = "smokers.txt", row.names = 1)
colnames(smokers) <- c("Age", "Ht", "Wt", "Nico", "Tar",
   "Astart", "Ncigs", "Nyears", "PkYr")
smokers

ssm <- scale(smokers)

pctp <- 0
within <- kmeans(ssm, centers = 1)$totss
for (k in 2 : 8)        # for different values of k
   {
    clk <- kmeans(ssm, 
         centers = k)      # perform a k-means cluster
    wi <- sum(clk$withinss)# within clusters, sum of squares
    pcte <- clk$betweenss / clk$totss         
   
    pctp <- c(pctp, pcte)
    within <- c(within, wi)
   }

within
pctp

### pdf(file = "smokep.pdf")
plot(pctp, col = "blue", pch = 16, cex = 3, 
    type = "b", lwd = 2,
   ylim = c(0,1), xlab = "Number of clusters",
   col.axis = "blue",
   ylab = "Percent variability explained", fg = "blue",
   col.lab = "blue", cex.lab = 1.5)
points(within/100, col = "red", pch = 16, cex = 3, 
       type = "b", lwd = 2)
axis(side = 4, labels = c("0", "25", "50", "75", "100"), 
           at = c(0, .25, .5, .75, 1), cex.lab = 1.5,
   col = "red", col.ticks = "red", col.axis = "red")
mtext(side = 4, text = "Within cluster sum of squares", 
   line = -1.5, col = "red", cex = 1.5)
### dev.off()


#  Not used:

# Determine number of clusters 
# Diagnostic only, not appearing in book
wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var))
for (i in 2 : 15) wss[i] <- sum(kmeans(mydata,
   centers = i)$withinss)
plot(1 : 15, wss, type = "b", xlab = "Number of Clusters",
  ylab = "Within groups sum of squares") 

#########################################################
#########################################################

pvar <- function(x) 
#  percent variability for K-means
   {
     n <- dim(x)[1]
     pvar <- c(1, 0)
     for (j in 2 : min(10, floor(n / 2)))
        {
         km <- kmeans(scale(x), centers = j)
         ss <- km$betweenss / km$totss
         pvar <- rbind(pvar, c(j, ss))
        }
    colnames(pvar) <- c("K", "%BetwSS")
    pvar
   }


########################################################
########################################################

#  Milk data

library(robustbase) 
pvar(milk)

milk1 <- milk
colnames(milk1) <- c("dens", "fat", "prot", "casein",
  "Fdry", "Ldry", "drysub", "cheese")

########################################################


###  Figure 11.7: Three panels for milk data: original,
##   omit outlier,  and % variability explained

### pdf(file = "milk.pdf")

op <- par(mfrow = c(2, 3), cex.lab = 1.5)

#####  First panel of 3 - identify outlier

id1 <- dist(scale(milk1))
fit1 <- hclust(id1)
plot(fit1, main = "", xaxt = "n", yaxt = "n", ylab = "",
   xlab = "original milk data", sub = "")

#### Second panel of 3: omit outlier, 
##  color code by casein > 25.5

milk2 <- milk1[ -70 , ]   # omit outlier
id2  <- dist(scale(milk2))        
fit2 <- as.dendrogram(hclust(id1))

##### function to get color labels
colLab <- function(n)         # function on dendrograms 
   {
    if (is.leaf(n))           # when we find a leaf
      {
       a <- attributes(n)     # capture its attributes
       lab <- milk2[a$label,] # vector of data values
       leafcolor <- c("red", "blue")[1 + (lab[4] > 25.5) ]
                              # assign new attributes to leaf 
       attr(n, "nodePar") <- c(a$nodePar, lab.col = leafcolor)
      }
    n                         # return the dendrogram
   }

colfit <- dendrapply(fit2, colLab) # dendrapply to tree
plot(colfit, main = "", xlab = "omitted outlier",  
     cex = .8, axes = FALSE, 
     sub = "blue for casein > 25.5, others in red") 

#### Third panel: % variability
plot(pvar(milk2), type = "b", col = "red", 
     pch = 16, cex = 2,
     xlab = "K means", ylab = "% Between SS")

par(op)

### dev.off()

########################################################
########################################################

## Figure 11.8: K-means pair() for milk 
## with different colors in upper and lower panels

### pdf(file = "milkpair.pdf")

library(robustbase)

milk2 <- milk[ -70, ]              # omit outlier
colnames(milk2) <- c("dens", "fat", "prot", "casein", "Fdry", 
  "Ldry", "drysub", "cheese")      # supply new names
                                   # color schemes for K-means
colorlow <- rainbow(3)
              [kmeans(scale(milk2), centers = 2)$cluster]
colorup  <- rainbow(3)
              [kmeans(scale(milk2), centers = 3)$cluster]

panel.up <- function(x, y, ...)
   {           # function to produce scatterplot in pairs()
    usr <- par("usr")              # save par() values
    on.exit(par(usr))              # set par() at end
                                   # set plot bounds:
    par(usr = c(max(y), min(y), max(x), min(x)))  
                                   # plot data points:
    points(y, x, col =  colorup, pch = 16)           
   }

pairs(milk2, pch = 16, gap = 0, xaxt = "n", yaxt = "n",
   col = colorlow, upper.panel = panel.up)

### dev.off()

##################################################

## Figure 11.9: K-mean simulations

library(mvtnorm)

n <- 100
sig <- matrix(c(1, -.6, -.6, 1), 2, 2)
sig
rmvn <- rmvnorm(n, mean = c(0, 0), sigma = sig)
rmvn <- rbind(rmvn,
    rmvnorm(n, mean = c(3, 3), sigma = sig))
rmvn <- rbind(rmvn,
    rmvnorm(n, mean = c(4, -1), sigma = sig))

colnames(rmvnorm) <- c("X", "Y")

### pdf(file = "simkmean.pdf") #  Six panels of Figure 11.9 
op <- par(mfrow = c(3, 2))    #   3x2 plot
plot(rmvn, col = c(rep(1, n), rep(2, n), rep(3, n)), 
   pch = 16, xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
text( 5.25, 5, labels  = "known populations")

for (k in 1 : 8)
   {
    clk <- kmeans(rmvn, centers = k)
    pcte <- clk$betweenss / clk$totss   # % of var exlained
    if(k =  = 1) pctp <- pcte           # save these values
       else pctp <- c(pctp, pcte)
   }

plot(pctp, col = "blue", pch = 16, cex = 3, 
     type = "b", lwd = 2,
     xlab = "K clusters", ylab = "% var explained")

for (k in 2 : 5)
   {
    cl <- kmeans(rmvn, centers = k)
    plot(rmvn, col = cl$clust, pch = 16, xaxt = "n", 
         yaxt = "n",  xlab = " ", ylab = " ")
    text(6.5, 5, labels = paste("K = ", k))
   }
### par(op)
dev.off()

#####################################################
#    Section 11.3: Diagnostic and Validation
#####################################################

#   Affymetrix data on mouse mesenchymal cells

library(clValid)
data(mouse)
print(mouse[c(1 : 3, 145 : 147), ], digits = 4)

##########  Figure 11.10: Left panel #########

short <- mouse[ , 2 : 7]            #     remove text columns 
totss <- kmeans(short, centers = 1)$totss
pve <- 0
cmax <- 12
for (i in 2 : cmax)                #    % variability explained 
   {
    pve <- c(pve, 
       kmeans(short, centers = i)$betweenss / totss)
   }

library(fpc)

 pdf(file = "mouse1.pdf")       #  name left panel

plot(1:cmax, pve, type = "b", 
     xlab = "Number of Clusters", cex.lab = 1.5,
     ylab = " ", cex = 2, pch = 16, col = "red", 
     ylim = c(0,1) ) 

ps <- prediction.strength(short, Gmax = cmax)
ps$mean.pred     # mean prediction following simulation

lines(1 : cmax, ps$mean.pred, type = "b", cex = 2,
   pch = 16, col = "blue")

text(c(10, 10), c(.8, .15),  cex = 1.5, 
   labels = c("% variability \n explained",
     "simulated \n prediction \n strength"),
   col = c("red", "blue"))

 dev.off()

#################  Figure 11.10 Right panel ##############

kms <- kmeans(short, centers = 4)
kms$withinss / (kms$size - 1)

iclass <- as.integer(mouse[, 8]) # integer categories 
                                 # functional classes
iclass
nclass <- max(iclass)
nclass

moused <- dist(scale(short))
clust <- moused
fit <- hclust(moused)

library(ape)                     # graphics for trees

### pdf(file = "mouse2.pdf")     #  name right panel

plot(as.phylo(fit), type = "phylogram", 
   cex = .5, label.offset = .1, 
   tip.col = rainbow(nclass)[iclass] )# color by class

### dev.off()

#######################################################

## Figure 11.12: Heatmap of mouse data

### pdf(file = "heatmap.pdf")

heatmap( as.matrix( mouse[ , 2 : 7] ), 
   RowSideColors = rainbow(nclass)[iclass] ) 

### dev.off()


######  not run:  R code in text
    library(ape)
      library(clValid)
      library(stats)
      data(mouse)
      iclass <- as.integer(mouse$FC)  # integer categories 
      heatmap( as.matrix( mouse[ , 2 : 7] ), 
         RowSideColors = rainbow(max(iclass))[iclass] )

#########################################################

library(fpc)
library(ape)
library(clValid)
data(mouse)
short <- mouse[ , 2 : 7]            # remove text columns 

############   Bootstrap simulate K = 3 clusters

cbj <- clusterboot(short, bootmethod = "jitter",
   krange = 3, clustermethod = kmeansCBI)
cbj$jittermean

cbb <- clusterboot(short, bootmethod = "boot",
   krange = 3, clustermethod = kmeansCBI)
cbb$bootmean                # mean Jaccard Index values

############   and again, with K = 4:

cbj <- clusterboot(short, bootmethod = "jitter",
   krange = 4, clustermethod = kmeansCBI)

cbb <- clusterboot(short, bootmethod = "boot",
   krange = 4, clustermethod = kmeansCBI)
cbb$bootmean
cbj$jittermean


########### my simulation

library(fpc)
library(ape)
library(clValid)
data(mouse)
short <- mouse[ ,2 : 7]      # remove text columns 
d <- dist(short)
cl <- kmeans(short, centers = 3)
dunn <-                  # Dunn Index of original data
   cluster.stats(d, cl$cluster)$dunn 

n <- dim(short)[1]
for (i in 1 : 100)          # number of bootstrap samples               
   {
    print(i)
    booti <-  as.integer(1 + n * runif(n))  # bootstrap indices
                               # distances of bootstrap sample:
    bd <- dist( short[booti , ])          
    clb <- kmeans(bd, centers = 3) # cluster bootstrap sample
    dunn <- c(dunn,              # bootstraped Dunn values 
       cluster.stats(bd, clb$cluster)$dunn )
   }

print(dunn)

### pdf(file = "Dunn.pdf")
hist(dunn, col = "red", main = " ", ylab = " ", yaxt = "n",
   freq = F, xlab = " ", breaks = 15)
lines(rep(dunn[1], 2), c(0, 8), lwd = 8, col = "blue")
### dev.off()

########################################################
#                       Exercises                      #
########################################################


# Try principal components, Kmeans, milk: Exercise 11.5 a.

library(robustbase)      # library with the milk dataset

milk2 <- milk[ -70, ]                # omit outlier
colnames(milk2) <- c("dens", "fat", "prot", "casein", "Fdry", 
  "Ldry", "drysub", "cheese")      # supply new names
                              # color schemes for K-means
color3  <- rainbow(3)[kmeans(scale(milk2), 
                             centers = 3)$cluster]

pcm <- princomp(milk2, scores = TRUE)
###pdf("milkpc.pdf")
plot(pcm$scores[ , 1], pcm$scores[ , 2], 
     col = color3, pch = 16,
   xlab = "First principal component", xaxt = "n", yaxt = "n",
   ylab = "Second principal component",
   main = "K-means clusters plotted by principal components")
###dev.off()


#########################################################


oil <- read.table(file = "oil companies.txt", header = TRUE,
                  row.names = 1)
doil <- dist(oil)
fitoil <- hclust(doil)
plot(fitoil)                     #  Gazprom is very different

d2 <- dist(oil[-15,])
fit2 <- hclust(d2)
plot(fit2)                      #  There are three other groups

require(graphics, stats)
(cl <- kmeans(oil, centers = 4))
pdf(file = "oilprod.pdf")
plot(oil[, 2:3], col = cl$clust, pch = 16, cex = 2.5,
     xlab = "Years of reserves", cex.lab = 1.75,
     ylab = "2006 Production")
text(oil[15 : 16, 2 : 3], labels = row.names(oil)[15 : 16], 
     cex = 1.5, pos = c(2, 4))
dev.off()


#########################################################
########################  End of this file  #############
#########################################################
