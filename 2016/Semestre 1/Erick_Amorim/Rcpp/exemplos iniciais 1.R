rm(list=ls())
#install.packages("Rcpp")
library("Rcpp")
library("microbenchmark")
wd = getwd()
sourceCpp(paste(wd,"/functions.cpp",sep=""))
##usando o rcpp fora do ambiente .cpp
#exemplo que entra-se com um vetor e sai um vetor
sourceCpp(code='#include <Rcpp.h>
          using namespace Rcpp;
          
          //[[Rcpp::export]]
          NumericVector ab(NumericVector x){ return(x);}
          ')
ab(c(1,2,3,4))
#exemplo que entra-se com uma matriz e sai uma matriz
sourceCpp(code='#include <Rcpp.h>
          using namespace Rcpp;
          
          //[[Rcpp::export]]
          NumericMatrix ab1(NumericMatrix x){ return(x);}
          ')
ab1(matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3))
#exemplo que entra-se com um vetor e sai a soma dos elementos
sourceCpp(code='#include <Rcpp.h>
          using namespace Rcpp;
          
          //[[Rcpp::export]]
          double sumRcpp(NumericVector x){ 
          int n=x.size();
          double total=0;
          for(int i=0; i<n; i++)
            {
             total+=x[i];
            }
           return(total);}
          ')
sumRcpp(c(6,2,4,3,5))
##outra forma de criar funcoes no C++ e carrega-las para o R
code='#include <Rcpp.h>
          using namespace Rcpp;
          
          //[[Rcpp::export]]
          double sumRcpp(NumericVector x){ 
          int n=x.size();
          double total=0;
          for(int i=0; i<n; i++)
            {
             total+=x[i];
            }
           return(total);}
          '
cppFunction(code)
sumRcpp(c(6,2,4,3,5))

#funcao Soma implementada no R
sumR=function(x){
  total=0
  for(i in 1:length(x))
  {
    total=total+x[i]
  }
}
x=rpois(100,6)
microbenchmark(sumR(x),sumRcpp(x),sum(x)) #veja que o tempo da funcao sum é menor



##exemplos com listas
#exemplo no R
rlist=function(x,y,z){
  return( list(x=rnorm(x), y=y, z=z^2) )
}

#lista no R
rlist(4,c("Nívea","Larissa","Juciane","Lívea"),seq(-4,4))
#lista no Rcpp
cpplist(4,c("Nívea","Larissa","Juciane","Lívea"),seq(-4,4))


##exemplo de funcoes que chamam funcoes
rfun=function(x,f){
  for(i in 1:100){ out=f(x)}
  return(out)
}

#exemplos com funcoes no R
rfun(c(1,4,9),sqrt)
rfun(c(1,4,9),function(x){1/x})
#exemplos com funcoes no Rcpp
cppfun(c(1,4,9),sqrt)
cppfun(c(1,4,9),function(x){1/x})

x=1:5
f=sqrt
microbenchmark(rfun(x,f),cppfun(x,f)) #veja aqui que o tempo no R é menor


#exemplo: chamando uma funcao do R no C++

x=rnorm(1000)
fivenum(x)
sum(x)
Rcppfunc(x,fivenum)
Rcppfunc(x,sum)

