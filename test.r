h <- c(1, 2, 3, 4, 5, 6)
M <- c("A", "B", "C", "D", "E", "F")
barplot(h,
    names.arg = M, xlab = "X", ylab = "Y",
    col = "#00cec9", main = "Chart", border = "#fdc46e"
)

# attach(mtcars)
# par(mfrow=c(2,2))
# plot(wt,mpg, main="Scatterplot of wt vs. mpg")
# plot(wt,disp, main="Scatterplot of wt vs disp")
# hist(wt, main="Histogram of wt")
# boxplot(wt, main="Boxplot of wt")
