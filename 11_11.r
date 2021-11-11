burger = read.table(r"(C:\Users\aes6669ray\Desktop\R Test\data\burger.txt)", header = TRUE, row.names = 1)
library(aplpack)
#pairs(burger,col = "red",gap  = 0 ,xaxt = "n", yaxt =  "n", panel = panel.smooth, col.smooth =  "blue")

#bagplot.pairs(burger[,4:7],gap=0,factor = 4,pch = 16,col.baghull = "green",main = "")

#library(graphics)
#coplot(Cal ~ Sodium |Fat, data=burger,rows=1,pch=16,number = 3,col="red",bar.bg = c(num="blue",fac=gray(0.95)))

#palette(rainbow(7))
#burs = burger[order(burger$Cal),]
#stars(burs,len=1,key.loc=c(12.5,2),labels=row.names(burs),draw.segments = TRUE)

library(MASS)
op=par(cex.axis=1.4)
ir=rbind(iris3[,,1],iris3[,,2],iris3[,,3])
#print(ir)
#parcoord(log(ir))

#1. (3,15,6)
#2. (4,4,8)
#3. (10)
x=c(1,2,3,4)
y=c(5,6,7,8,4)
#j=crossprod(x,y)

k=diag(2,3,3)
print(k)