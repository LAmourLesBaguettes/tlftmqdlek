#4/25 수업
x <- c(1,3,2,1,5)
a=2; a*x
x <-c(1,1); z<-c(0,0)
ax <- a*x
plot(x=x[1],y=x[2],xlim=c(-3,3),ylim=c(-3,3))
abline(v=0,col="gray50", lty=2)
abline(h=0, col="gray50", lty=2)

lines(c(z[1],x[1]),c(z[2],x[2]),xlim=c(-3,3), ylim=c(-3,3))
lines(c(z[1],ax[1]),c(z[2],ax[2]),xlim=c(-3,3), ylim=c(-3,3), col="red", lty=2)

#Addition
x <- c(1,3,2,1,5); y <- c(3,2,1,4,6)
x+y

#linear combination
x1 <- c(1,2,3) ; x2 <- c(4,2,1) ; x3 <- c(4,2,5)
a1=1; a2=2; a3=3

y <- a1*x1 + a2*x2 + a3*x3
print(y)

#linearly dependent
x1 <- c(1,3,5); x2 <- c(2,6,10)
a1 = -2 ; a2 = 1
print(a1*x1 + a2*x2)

library(Matrix)
X <- cbind(x1,x2)
rankMatrix(X)

#linearly dependent
x1 <- c(1,2,3); x2 <- c(3,0,-1)
X <- cbind(x1,x2)
rankMatrix(X)

# length
x <- c(1,5,3,4,5)
normx <- sqrt(sum(x^2))

# inner product
x <- c(1,2,3,4,5); y <- c(1,1,1,1,2)
t(x)%*%y
sum(x*y)

#angle
theta=30
rad <- theta&pi/180
A <- matrix(c(cos(rad), sin(rad),-sin(rad),cos(rad)),2,2)
x <- c(2,1)
y <- drop(A%*%x)
z <- c(0,0)

#perpendicular
x <- c(1,2) ; y <- c(-2,1)
t(x)%*%y

#projection
x <- c(2,1); y <- c(1,1.5)

#4/30 수업
x <- matrix(c(1,2,3,4,5,6), nrow=2, ncol=3)
x0 <- matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow=T)

N=10; P=5
set.seed(0430)
tmp <- round(rnorm(N*P),2)
X <- matrix(tmp, nrow=N, ncol=P)

Gram <- t(X)%*%X
sigma <- cov(X)

# example
A <- matrix(1:9, nrow=3, ncol=3)
B <- matrix(rep(2,9), nrow=3, ncol=3)
A%*%B == B%*%A
A == t(A)
sigma == t(sigma)

library(Matrix)
A <- matrix(sample(1:20,9),3,3)
rankMatrix(A)
det(A)
solve(A)
B <- cbind(A[,1:2],A[,2]*2.3)
rankMatrix(B)
det(B)
solve(B)

t(solve(sigma)) == solve(t(sigma))
P=5
set.seed(0430)
A <- sigma; B <- matrix(rnorm(P*P),P,P)
solve(A%*%B) == solve(B)%*%solve(A)

A <- sigma
det(A) == det(solve(A))
det(solve(A))
1/det(A)

det(A%*%B) == det(A)*det(B)
det(A%*%B); det(A)%*%det(B)

tr.fun <- function(X){
  sum(diag(X))
}
tr.fun(A)

tr.fun(2*A) == 2*tr.fun(A)
tr.fun(A+B) == tr.fun(A) + tr.fun(B)
tr.fun(A%*%B) ; tr.fun(B%*%A)
tr.fun(solve(B)%*%A%*%B) ; tr.fun(A)
tr.fun(t(A)%*%A) ; sum(A^2)

theta = 30
O <- matrix(c(cos(theta),0, sin(theta), 0,1,0,
              -sin(theta),0,cos(theta)), 3,3)
t(O)%*%O ; O%*%t(O)

A <- matrix(c(1,3,1,1/2),2,2)
e0 <- eigen(A)
e0$value[1]
e0$vectors[,1]

e1 <- e0$vectors[,1]
e2 <- e0$vectors[,2]

t(e1)%*%e2

N = 100; P = 5
set.seed(1)
X <- matrix(rnorm(N*P),N,P)
A <- round(cov(X),2) ; A
e0 <- eigen(A)

P <- e0$vectors
L <- diag(e0$values)
P;L

P%*%L%*%t(P)
A
A == P%*%L%*%t(P) #동일한데 왜 다르게 뜨지?

# 5/2 수업
library(MASS)
library(mvtnorm)
install.packages("mvtnorm")
library(scatterplot3d)
install.packages("scatterplot3d")
win.graph(width=10, height=10)

# 1) same variance, no correlation
mu <- c(0,0)
sigma <- matrix(c(1,0,0,1),2,2)
x <- y <- seq(-5,5, length.out=100)
z <- matrix(0, 100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}
contour(x,y,z, xlap="x", ylalb="y")
persp(x,y,z, xlab = "x", ylab = "y", zlab = "density")


# 2) different variance, no correlation
mu <- c(0,0)
sigma <- matrix(c(2,0,0,5),2,2)
x <- y <- seq(-5,5, length.out=100)
z <- matrix(0, 100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}
contour(x,y,z, xlap="x", ylalb="y")
persp(x,y,z, xlab = "x", ylab = "y", zlab = "density")

# 3) positive correlation
win.graph(width=15, height=15)
par(mfrow=c(2,2))

mu <- c(0,0)
sigma <- matrix(c(1,0.5,0.5,1),2,2)
x <- y <- seq(-5,5, length.out=100)
z <- matrix(0, 100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}
contour(x,y,z, xlap="x", ylalb="y")
persp(x,y,z, xlab = "x", ylab = "y", zlab = "density")

# 4) negative correlation
mu <- c(0,0)
sigma <- matrix(c(1,-0.5,-0.5,1),2,2)
x <- y <- seq(-5,5, length.out=100)
z <- matrix(0, 100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}
contour(x,y,z, xlap="x", ylalb="y")
persp(x,y,z, xlab = "x", ylab = "y", zlab = "density")

# 5) with eigenvalues, eigenvectors
mu <- c(0,0)
sigma <- matrix(c(3,0.8,0.8,1),2,2)
x <- y <- seq(-5,5, length.out=100)
z <- matrix(0, 100,100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] <- dmvnorm(c(x[i], y[j]), mean = mu, sigma = sigma)
  }
}
contour(x,y,z, xlap="x", ylalb="y")
persp(x,y,z, xlab = "x", ylab = "y", zlab = "density")

e0 <- eigen(sigma)

e.vec1 <- e0$vectors[,1]
e.vec2 <- e0$vectors[,2]

lam1 <- e0$values[1]
lam2 <- e0$values[2]

con = 2
tmp1 <- e.vec1*sqrt(lam1)*con
tmp2 <- -e.vec1*sqrt(lam1)*con

lines(c(0, tmp1[1]), c(0, tmp1[2]), col="red")
lines(c(0, tmp2[1]), c(0, tmp2[2]), col="red")

tmp1 <- e.vec2*sqrt(lam2)*con
tmp2 <- -e.vec2*sqrt(lam2)*con

lines(c(0, tmp1[1]), c(0, tmp1[2]), col="blue")
lines(c(0, tmp2[1]), c(0, tmp2[2]), col="blue")

#chapter 6
library(ICSNP)
math.teach <- data.frame(teacher=factor(rep(1:2, c(3,6))),
                         satis=c(1,3,2,4,6,6,5,5,4),
                         know=c(3,7,2,6,8,8,10,10,6))
math.teach
with(math.teach, tapply(know, teacher, mean))
with(math.teach, tapply(satis, teacher, var))

par(mfrow=c(1,2))
boxplot(know~teacher, math.teach, horizontal = F, ylim=c(0,10))
boxplot(satis~teacher, math.teach, horizontal = F, ylim=c(0,10))

m0 <- with(math.teach, HotellingsT2(cbind(satis,know) ~ teacher))

#240530 수업
##construct the data (Saving data)

y <- c(6,10,3,15,12,5,10,2,14,4)
x1 <- c(34,50,15,80,52,50,45,20,60,18)
x2 <- c(3,4,2,5,3,6,4,3,4,2)

Data <- data.frame(y,x1,x2)
names(Data) <- c("savings", "income", "family")
head(Data)

X <- Data[,1]
colMeans(X); apply(X,2,sd)
plot(Data)

fit <- lm(savings~., data=Data)
anova(fit)
summary(fit)

fit1 <- lm(savings ~ income, data=Data)
summary(fit1)
fit2 <- lm(savings ~ family, Data)
summary(fit2)

library(car)

avPlots(fit, lwd=0.1) #각 변수의 임펙트를 살펴볼때 좋은 함수
sData <- as.data.frame(scale(Data)) # 어떤 변수가 더 이펙트를 가지고 있는지 한번에 확인 가능
s0 <- lm(savings ~., data=sData)
summary(s0)

data(mtcars)
head(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
Y <- as.matrix(mtcars[,c("mpg","disp","hp","wt")]) #다변량 선형 회귀 분석을 위해 
mvmod <- lm(Y ~ cyl + am + carb, mtcars)
coef(mvmod)

mvsum <- summary(mvmod)
mvsum[[1]] # 다변량에서 한 변량에 대한 결과치를 보여줌 / 단변량은 아님

library(MASS)
library(mvtnorm)
library(scatterplot3d)

headsize <- matrix(c(191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190, 188, 163, 195, 186, 181, 175, 192, 174, 176, 197, 190, 155, 149, 148, 153, 144, 157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155, 153, 145, 140, 154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192, 190, 189, 197, 187, 186, 174, 185, 195, 187, 161, 183, 173, 182, 165, 185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 152, 149, 152, 159, 151, 148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 147, 143, 158, 150) ,nrow=25,ncol=4, dimnames=list(character(0),c("head1", "breadth1", "head2", "breadth2")))

headsize <- as.data.frame(headsize)

head(headsize)

head.dat <- headsize[,c("head1", "head2")]
head(head.dat)

colMeans(head.dat)
cov(head.dat)
cor(head.dat)

head.pca <- princomp(x=head.dat)
head.pca
print(summary(head.pca), loading=T)

eigen(cov(head.dat))

#lambda값 차이 > Var / princomp은 n으로 나눔 / eigen, prcomp은 n-1로 나눔

head.pca$scores
var(head.pca$scores)
x.mat <- scale(head.dat, center=T, scale=F)
l.mat <- matrix(as.numeric(head.pca$loading),2,2)

pc1 <- x.mat%*%l.mat[,1]
pc2 <- x.mat%*%l.mat[,2]
cbind(pc1, pc2)

par(mfrow=c(1,2))
mm <- colMeans(head.dat)
plot(head.dat)
xlim <- range(head.pca$scores[,1])
plot(head.pca$scores, xlim=xlim, ylim=xlim)

library(HSAUR2)
data("heptathlon")
head(heptathlon)

#transformation 3 variables : hurdles, run 200m, run 800m
heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)
score <- which(colnames(heptathlon) == "score")

round(cor(heptathlon[,-score]),2)
plot(heptathlon[,-score])
x.mat <- heptathlon[,-score]
p0 <- prcomp(x.mat, scale=T)
summary(p0)

p0$x
plot(p0$x[,1], p0$x[,2])

#6/4 수업
library(psych)

setwd("C:\\Users\\Master\\Desktop\\3학년\\다변량 데이터 분석")
Data <- read.csv("physical.csv")
head(Data)
X <- Data[,-1]
colMeans(X) ; apply(X,2,sd)

##
fit <- principal(X, nfactors = 10, rotate = "none")
names(fit)
plot(fit$values, type="b", main="Scree plot")

fit <- principal(X, nfactors = 2, rotate = "none")
plot(fit, xlim=c(-1,1), ylim=c(-1,1))

fit <- principal(X, nfactors = 2, rotate = "varimax")
plot(fit, xlim=c(-1,1), ylim=c(-1,1))

fscore <- fit$scores #새로운 디자인 매트릭스 활용해 추가적 분석 들어가는 것이 좋음 프로젝트
head(fscore)
plot(fscore, cex=0.5, col="red")
text(fscore, labels=Data$ID, cex=0.6)


# PCA
data("heptathlon", package = "HSAUR2")
head(heptathlon)

heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)
score <- which(colnames(heptathlon) == "score")

x.mat <- heptathlon[,score]
p0 <- prcomp(x.mat, scale=T)
plot(p0$x[,1], p0$x[,-2], xlab = "PC1", ylab = "PC2")

del.id <- which(p0$x[,1] > 6)
x.mat <- x.mat[del.id]

p1 <- prcomp(x.mat)
print(p1)
summary(p1)
predict(p1)
plot(p1)

plot(heptathlon$score[-del.id], p1$x[,1])

#6/6수업

N=5

#construct a distance matrix
dist.mat <- matrix(0, nrow=N, ncol=N)
dist.mat[lower.tri(dist.mat)] <- c(7,1,9,8,6,3,5,8,7,4)

dist.mat <- as.dist(dist.mat, diag=T)

print(dist.mat)

# 1. perform the hierarachical clusering analysis : single linkage
h0 <- hclust(d=dist.mat, method = "single")
plot(h0, main="Cluster Dendrogram : single linkage method", xlab="Observations")

memb <- cutree(h0, k=2) #cluster 몇개에서 끊을 건지 > k로 나타냄
#memb <- cutree(h0, h=3.5)
names(memb) <- paste("obs",1:N, sep="")
print(memb)


library(datasets)
head(iris)

X <- iris[,1:4]
plot(X)

# hierarchical clustering
dist.mat <- dist(X)
h0 <- hclust(d=dist.mat, method = "average")
memb <- cutree(h0, k=3)

# cbind(memb, iris$Species)
table(memb, iris$Species)


# k-means cluster
k0 <- kmeans(x=X, centers=3)

#cbind(k0$cluster, iris$Species)
table(k0$cluster, iris$Species)

#perform the PCA
p0 <- princomp(x=X)
score <- p0$scores

plot(score[,1], score[,2], xlab="PC1", ylab="PC2")

plot(score[,1], score[,2], xlab="PC1", ylab="PC2", type="n")
points(score[iris$Species=="setosa",1], score[iris$Species=="setosa",2], col="black", pch=16)
points(score[iris$Species=="versicolor",1], score[iris$Species=="versicolor",2], col="red", pch=16)
points(score[iris$Species=="virginica",1], score[iris$Species=="virginica",2], col="blue", pch=16)

par(mfrow=c(1,2))
plot(score[,1], score[,2], col=c("red", "blue", "green")[iris$Species],pch=18, main="TRUE")
plot(score[,1], score[,2], col=c("red", "green", "blue")[k0$cluster], pch=18, main="K-means clustering")

plot(X, col=c("red","blue","green")[iris$Species])
