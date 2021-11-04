#install.packages("MVA")

# require(graphics)

housing  <- read.table(file  =  "housing.txt")
attach(housing)
bounda <- range(Apartment) * c(.95, 1)
boundb <- range(House) * c(.8, 1)

# plot(housing, xlim = bounda, ylim = boundb, axes = FALSE, pch = 16, col = "red")
# rug(Apartment, side = 1)
# rug(House, side = 2)

library(MVA)
aa = c("DC","HI","CA")
exst = match(aa, rownames(housing))

#bvbox(housing,xlab = "Apartment",ylab = "House", pch=19,col="red")

#text(housing$Apartment[exst],housing$House[exst],labels=aa,pos=c(2,2,3))

library(aplpack)
zh = chull(housing)
ah = c(zh,zh[1])
a=housing[-zh,] 
bh = chull(a)
ch = c(bh, bh[1])
b =a[-bh,]
dh = chull(b)
dh = c(dh,dh[1])

# plot(housing, pch= 19)
# lines(housing$Apartment[ah],housing$House[ah],col="green")
# lines(a$Apartment[ch],a$House[ch],col="red")
# lines(b$Apartment[dh],b$House[dh],col="blue")
nlev=5
colors=heat.colors(9)[3:(nlev+2)]
plothulls(housing, n.hull = nlev, col.hull = colors,
main="", lty.hull=1:nlev, density = NA)







