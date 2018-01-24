#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector f_if_Rcpp(NumericVector x, double a, double b){
  
  int n =x.length(); NumericVector out(n);
  for(int i=0; i<n; i++)
  {
    double xi=x[i];
    if(xi<a){out[i]=a;} else if (xi>b){out[i]=b;} else {out[i]=xi;}
  }
  return(out);
}

// [[Rcpp::export]]
int fibRcpp(const int x) { 
  if(x<2){return x;}
  else {return ( fibRcpp(x-1) + fibRcpp(x-2) ) ; } 
  }


/* exemplos com listas */

//[[Rcpp::export]]
List cpplist(int x, CharacterVector y, NumericVector z){
  return List::create(Rcpp::Named("x", rnorm(x,0,1)),
                      Rcpp::Named("y", y),
                      Rcpp::Named("z", pow(z,2) ) );
}



/* exemplos de funcoes que chamam funcoes */

//[[Rcpp::export]]
NumericVector cppfun(NumericVector x, Function f){
  NumericVector out(x.length());
  for(int i=0; i<100; i++)
    { 
     out=f(x);
    }
  return out;
}

/* chamando uma funcao do R no C++ */

//[[Rcpp::export]]
NumericVector Rcppfunc(NumericVector x, Function f){
  NumericVector out=f(x);
  return out;
}

/*** R
  x=rpois(100,6)
  Rcppfunc(x,fivenum)
  Rcppfunc(x,sum)
  */

























