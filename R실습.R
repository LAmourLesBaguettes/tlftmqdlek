library(MVA)
library(mvtnorm)
library(KernSmooth)

head(USairpollution)

#2.scatterplot
mlab <- "Manufactoring enterpirese with 20 or more workers"
plab <- "Population size (1970 census) in thousands"
plot(popul~manu, data=USairpollution, xlab=mlab, ylab=plab, main="Scatterplot of two variables: manu vs popul")
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)

op <- par(mai=rep(0.2,4), oma=rep(0.2,4)) #mai = inner margin , oma = outer margin
layout(matrix(c(2,0,1,3), nrow = 2, byrow = TRUE), widths = c(2,1),
       heights = c(1,2), respect = T)
xlim <- with(USairpollution, range(manu))*1.1
plot(popul~manu, data = USairpollution, cex.lab=0.9,
     xlab=mlab, ylab=plab, type="n", xlim=xlim)
with(USairpollution, text(manu, popul, cex=0.6,
                          labels=abbreviate(row.names(USairpollution))))
with(USairpollution, hist(manu, main="", xlim=xlim))
with(USairpollution, boxplot(popul))
par(op)
dev.off()

#bivariate boxplot
outcity <- match(lab <- c("Chicago", "Detroit", "cleveland", "Philadelphia", "Houston"),
                 rownames(USairpollution))
x <- USairpollution[,c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x$manu[outcity], x$popul[outcity], labels = lab,
     cex=0.7, pos=c(2,2,4,2,2))

with(USairpollution, cor(manu, popul))
cor(USairpollution$manu, USairpollution$popul)

with(USairpollution, cor(manu[-outcity], popul[-outcity]))

#BUbble plot
ylim <- with(USairpollution, range(wind))*c(0.95,1)
plot(wind ~ temp, data = USairpollution, xlab="AAT",
     ylab="AAWS", pch=10, ylim=ylim)
with(USairpollution, symbols(temp, wind, circles = SO2, inches = 0.5, add=TRUE))

#Star plot
stars(USairpollution, cex=0.5)

#Scatterplot matrix
pairs(USairpollution, pch=".", cex=1.5)
round(cor(USairpollution),4)
pairs(USairpollution, panel = function(x,y,...){
  points(x,y,...)
  abline(lm(y ~ x), col="gray50")
  }, pch = ".", cex = 1.5)

#3D scatter plot
library(lattice)
library(scatterplot3d)
install.packages("scatterplot3d")
with(USairpollution, scatterplot3d(temp, wind, SO2, angle =55 ,type="h"))

xyplot(SO2~temp|cut(wind,2),data=USairpollution)


##Kernel function
rec <- function(x){(abs(x)<1)*0.5}
tri <- function(x){(abs(x)<1)*(1-abs(x))}
gauss <- function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}

x.vec <- seq(-3, 3, length=1000)
y.rec <- rec(x.vec)
y.tri <- tri(x.vec)
y.gauss <- gauss(x.vec)

plot(x.vec, y.rec, main = "The kernel functions",
     ylim = c(0,1), xlab="x", ylab="K(x)", type="l", lty=3)
lines(x.vec, y.tri, lty=2, col="blue")
lines(x.vec, y.gauss, lty=1, col="red")

x <- c(0,1,1.1,1.5,1.9,2.8,2.9,3.5)
n <- length(x)
xgrid <- seq(from=min(x)-1, to=max(x)+1, by=0.01)
h <- 0.4
bumps <- sapply(x, function(a)gauss((xgrid-a)/h)/n*h)
plot(xgrid, rowSums(bumps), ylab=expression(hat(f)(X)), type="l", xlab="X", lwd=2)
out <- apply(bumps, 2, function(b)lines(xgrid,b))

#density
plot(density(x, bw=0.4))

#bivariate
library(KernSmooth)

head(CYGOB1)

CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1,dpik))
plot(CYGOB1)
contour(x=CYGOB1d$x1, y=CYGOB1d$x2, z=CYGOB1d$fhat, add = TRUE)
persp(x=CYGOB1d$x1, y=CYGOB1d$x2, z=CYGOB1d$fhat, xlab = "lag surface temperture", ylab = "log light intensity", zlab = "density")
