rm(list=ls())
#install.packages("RcppArmadillo")
library("RcppArmadillo")
library("Rcpp")
wd = getwd()
sourceCpp(paste(wd,"/armaBayesiana.cpp",sep=""))

#--exemplo gibbs normal-gama
n=150
mi=4
sigma2=2 #phi=1/sigma2 precisão
x=rnorm(n, mean = mi,sd=sqrt(sigma2))

MCMC=gibbsNG(x=x,N=500)
plot(MCMC$cadeias[,1],type="l")
plot(MCMC$cadeias[,2],type="l")
##-------------------------------------------------------------
#--exemplo gibbs regressão linear
rm(list=ls())
#install.packages("RcppArmadillo")
library("RcppArmadillo")
library("Rcpp")
wd = getwd()
sourceCpp(paste(wd,"/armaBayesiana.cpp",sep=""))
n=50
x1=rnorm(n,0,sqrt(0.4))
x2=rbinom(n,1,0.5)
B0=1
B1=0.5
B2=-0.5
sigma2=1.5
erro=rnorm(n,0,sqrt(sigma2))
y=B0+B1*x1+B2*x2+erro;
y=as.matrix(y)
dados=data.frame(rep(1,n),x1,x2)
X=as.matrix(dados)

MCMC2=GibbsRegress(y=y,x=X,N=2000,burnin = 1000)
plot(MCMC2$betas[,1],type="l")
abline(1,0,col="red",lwd=2)
plot(MCMC2$betas[,2],type="l")
abline(0.5,0,col="red",lwd=2)
plot(MCMC2$betas[,3],type="l")
abline(-0.5,0,col="red",lwd=2)
plot(MCMC2$sigma2[,1],type="l")
abline(1.5,0,col="red",lwd=2)
