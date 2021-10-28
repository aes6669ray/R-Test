# x <- (0:120)/10
# plot(x,dchisq(x,4),type="o")

# lines(x,dchisq(x,5))

# text(c(3,4.2),c(.18,.153),labels=c("4df","5df"))

library(boot)
data(cd4)

n= length(cd4$baseline)
group= rep(c(0,1),each=n)
join = c(cd4$baseline,cd4$oneyear)
# par(mfrow=c(1,3),cex.lab=1.5)
# plot(group,join,ylab="cd4count",ylim=c(0,max(join)),xlim=c(-.25,1.25))

# plot(jitter(group),join,ylab="",axes=FALSE,ylim=c(0,max(join)),xlim=c(-.25,1.25))

# plot(group,join,
# for (i in (1:n)){
#     lines(c(0,1),c(cd4$baseline[i],cd4$oneyear[i]))
# })

p=c(jitter(group))

plot(p,join,
for (i in (1:n)){
    lines(c(p[i],p[i+20]),c(cd4$baseline[i],cd4$oneyear[i]))
})
