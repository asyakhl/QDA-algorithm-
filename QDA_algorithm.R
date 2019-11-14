rm(list = ls()) 
cat("\014")

library(MASS)
library(ggplot2) 
library(dplyr)
library(gridExtra)
#c
set.seed(1)
n=100
p=2
X = matrix(ncol = p, nrow = n)
p1=0.6
p2=0.3
p3=0.1
rho1=0.25
rho2=0.5
rho3=0.75
o1=0.5
o2=0.5
o3=0.5
C1=o1^2*matrix(c(1,rho1,rho1,1), nrow=2)
C2=o2^2*matrix(c(1,rho2,rho2,1), nrow=2)
C3=o3^2*matrix(c(1,rho3,rho3,1), nrow=2)
mu1=c(0,1)
mu2=c(1,0)
mu3=c(-1,1)
y=as.factor(sample(c(1,2,3), size=n, replace=TRUE, prob=c(p1, p2, p3)))
y=sample(y)
for (i in 1:n){
  mu = (y[i]==1)*mu1 + (y[i]==2)*mu2 + (y[i]==3)*mu3
  C = (y[i]==1)*C1 + (y[i]==2)*C2 + (y[i]==3)*C3
  X[i,] = mvrnorm(1,mu, C)
}
n1 = sum(y==1)
n2 = sum(y==2)
n3 = sum(y==3)
X1 = X[y==1,]
X2 = X[y==2,]
X3 = X[y==3,]
mu1.hat = colMeans(X1)
mu2.hat = colMeans(X2)
mu3.hat = colMeans(X3)
p1.hat=n1/n
p2.hat=n2/n
p3.hat=n3/n

C1.hat=cov(X1)
C2.hat=cov(X2)
C3.hat=cov(X3)

dataf = data.frame(y, X)
plot1 = ggplot(dataf, aes(x=X1, y=X2, colour=y))+geom_point()+ggtitle("train data")

#d
X.grid     =     expand.grid(x=seq(from = min(X[,1]), to =  max(X[,1]), length.out = 100), 
                             y=seq(from = min(X[,2]), to =  max(X[,2]), length.out = 100))

X.grid=as.matrix(X.grid)
Z=c(1:10000)
for (i in 1:10000){
  delta1 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C1)%*%as.vector(X.grid[i,]) + 
              as.vector(X.grid[i,])%*%solve(C1)%*%mu1-0.5*mu1%*%solve(C1)%*%mu1 + log(p1) - 0.5*log(det(C1)))
  delta2 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C2)%*%as.vector(X.grid[i,]) + 
              as.vector(X.grid[i,])%*%solve(C2)%*%mu2-0.5*mu2%*%solve(C2)%*%mu2 + log(p2) - 0.5*log(det(C2)))
  delta3 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C3)%*%as.vector(X.grid[i,]) + 
              as.vector(X.grid[i,])%*%solve(C3)%*%mu3-0.5*mu3%*%solve(C3)%*%mu3 + log(p3) - 0.5*log(det(C3)))
  dmax=which.max(c(delta1, delta2, delta3))
  if(dmax==1){
    Z[i]=1
  }else if( dmax==2){
    Z[i]=2
  }else if(dmax==3){
    Z[i]=3
  }
}
Z=factor(Z)
dataff=data.frame(X.grid, Z)
plot2= ggplot(dataff, aes(x=x, y=y, colour=Z))+geom_point()+ggtitle("Predicted Label Bayes Classifier")

#e QDA
M=c(1:10000)
for (i in 1:10000){
delta1 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C1.hat)%*%as.vector(X.grid[i,]) + 
    as.vector(X.grid[i,])%*%solve(C1.hat)%*%mu1.hat-0.5*mu1.hat%*%solve(C1.hat)%*%mu1.hat + 
    log(p1.hat) - 0.5*log(det(C1.hat)))

delta2 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C2.hat)%*%as.vector(X.grid[i,]) + 
    as.vector(X.grid[i,])%*%solve(C2.hat)%*%mu2.hat-0.5*mu2.hat%*%solve(C2.hat)%*%mu2.hat + 
    log(p2.hat) - 0.5*log(det(C2.hat)))

delta3 = (-0.5*(as.vector(X.grid[i,]))%*%solve(C3.hat)%*%as.vector(X.grid[i,]) + 
    as.vector(X.grid[i,])%*%solve(C3.hat)%*%mu3.hat-0.5*mu3.hat%*%solve(C3.hat)%*%mu3.hat + 
    log(p3.hat) - 0.5*log(det(C3.hat)))
dmax=which.max(c(delta1, delta2, delta3))
if(dmax==1){
  M[i]=1
}else if( dmax==2){
  M[i]=2
}else if(dmax==3){
  M[i]=3
}

}
M=factor(M)
datafff=data.frame(X.grid, M)
plot3= ggplot(datafff, aes(x=x, y=y, colour=M))+geom_point()+ggtitle("Predicted Label QDA")
grid.arrange(plot1, plot2,plot3, nrow =2) 
