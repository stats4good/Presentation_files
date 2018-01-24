rsnormtruncada=function(n,mu,sigma,a,b){
  Fa=pnorm(a,mu,sigma)
  Fb=pnorm(b,mu,sigma)
  g = 1/(b-a)
  if(a<=mu&&b>=mu){
    f=dnorm(mu,mu,sigma)*(1/sigma)/(Fb-Fa)
    k=f/g
  }
  if(a>mu&&b>mu){
    f=dnorm(a,mu,sigma)*(1/sigma)/(Fb-Fa)
    k=f/g
  }
  if(a<mu&&b<mu){
    f=dnorm(b,mu,sigma)*(1/sigma)/(Fb-Fa)
    k=f/g
  }
  
  x=NULL
  while(i<=n){
    xestrela=runif(1,a,b)
    u=runif(1)
    festrela=dnorm(xestrela,mu,sigma)*(1/sigma)/(Fb-Fa)
    p=festrela/(k*g)
    if(u<=p){
      x[i]=xestrela
      i=i+1
    }
  }
  #hist(x,freq=F)
}

tempo=system.time(rsnormtruncada(100000,0,1,-1,2))
tempo

# plotando as observações geradas no Ox

setwd("C:/Users/Livia/Dropbox/stat4good") 
#dados=scan("rsnormal.mat")
dados=matrix(dados,ncol=1,byrow=T)
hist(dados)

  