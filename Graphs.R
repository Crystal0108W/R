###Scatter Plot
attach(mtcars)
par(mar = c(5,4,4,2))
par(mfrow = c(1,1))
plot(wt, mpg,
     main = " Basic Scatter Plot of MPG vs. Weight",
     xlab = "Car Weight",
     ylab = "Miles Per Gallon", pch = 19)
abline(lm(mpg~wt), col = "pink", lwd = 2, lty=1)
lines(lowess(wt, mpg), col = "grey", lwd = 2, lty = 2)


library(car)
scatterplot(mpg~wt | cyl, data = mtcars, lwd = 2, span = 0.75,
            main = "Scatter Plot of MPG vs. Weight by # Cylinders",
            xlab = "Weight of Car",
            ylab = "Miles per Gallon",
            legend.plot = TRUE,
            id.method = "identify",
            labels = row.names(mtcars),
            boxplots = "xy")

###Scatter-plot Matrices
pairs(~mpg+disp+drat+wt, data = mtcars,
      upper.panel = NULL,
      main = "Basic Scatter Plot Matrix")

scatterplotMatrix(~mpg+disp+drat+wt, data = mtcars,
                  spread = FALSE, smoother.args=list(lty=2),
                  main = "Scatter Plot Matrix via car package")

###High-density Scatter Plot
set.seed(1234)
n <- 1000
c1 <- matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2), ncol = 2)
mydata <- rbind(c1,c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x","y")

with(mydata, plot(x,y,pch = 18, main = "Scatter plot with 10,000 Observations"))
with(mydata, smoothScatter(x,y,main="Scatter Plot Colored by Smooth Densities"))

install.packages("hexbin")
library(hexbin)
with(mydata, {
      bin <- hexbin(x,y,xbins=50)
      plot(bin, main = "Hexagonal Binning with 10,000 Observations")
})

install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, 
              pch = 16,
              highlight.3d = TRUE,
              type = "p",
              main = "Scatter Plot with Verticle Lines")

s3d <- scatterplot3d(wt, disp, mpg, 
                     pch = 16,
                     highlight.3d = TRUE,
                     type = "p",
                     main = "Scatter Plot with Verticle Lines with Regression Plane")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)


install.packages("plot3D")
library(plot3D)
install.packages("rgl")
library(rgl)
attach(mtcars)
plot3D::scatter3D(wt, disp, mpg, col = "pink", size=5)
with(mtcars, scatter3D(wt, disp, mpg))

###Bubble Plot
r <- sqrt(disp/pi)
symbols(wt,mpg,circles = r, inches = 0.3,
        fg = "white", bg = "pink",
        main = "Buble Plot with poit size proportional to displacement",
        ylab = "Miles Per Gallon",
        xlab = "Weight of Car")
text(wt, mpg, rownames(mtcars), cex = 0.6)
detach(mtcars)


opar<- par(no.readonly = TRUE)
par(mfrow=c(1,1))
t1 <- subset(Orange, Tree==1)
plot(t1$age, t1$circumference, xlab = "Age", ylab ="Circumference", 
     main = "Orange Tree 1 Growth")
plot(t1$age, t1$circumference, xlab = "Age", ylab ="Circumference",
     main = "Orange Tree 1 Growth", 
     type = "b")
par(opar)


Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)

xrange <- range(Orange$age)
yrange <- range(Orange$circumference)

plot(xrange, yrange, 
     type = "n",
     xlab = "Age",
     ylab = "Cir")

colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18+ntrees, 1)

for (i in 1:ntrees) {
  tree <- subset(Orange, Tree == i)
  lines(tree$age, tree$circumference,
        type = "b",
        lwd = 2, 
        lty = linetype[i],
        col = colors[i],
        pch = plotchar[i])
}
title("Tree growth", "Example of line plot")
legend(xrange[1], yrange[2],
       1:ntrees, cex = 0.8, col = colors,
       pch = plotchar,
       lty = linetype, 
       title = "Tree")

###Corrgrams
options(digits = 2)
install.packages("corrgram")
library(corrgram)

par(mar = c(5,4,2,2))
corrgram(mtcars, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)
title("Corrgram of mtcars intercorrelations")

cols<- colorRampPalette(c("pink","salmon","grey","cyan"))
corrgram(mtcars, order = TRUE, col.regions = cols, 
         lower.panel = panel.shade,
         upper.panel = panel.conf, text.panel = panel.txt)
title("A Corrgram of a Different Color")

###Mosaic Plots
install.packages("vcd")
library(vcd)
ftable(Titanic)
mosaic(Titanic, shade = TRUE, legend = TRUE)
mosaic(~Class+Sex+Age+Survived, data = Titanic, shade =TRUE, legend = TRUE)
