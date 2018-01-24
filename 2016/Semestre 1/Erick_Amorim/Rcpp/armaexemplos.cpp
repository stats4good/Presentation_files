#include<RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]

using namespace Rcpp;

//[[Rcpp::export()]]
arma::mat exemplo1(arma::mat x){
  return(x);
}

//[[Rcpp::export()]]
arma::vec exemplo2(arma::vec x){
  return(x);
}

//[[Rcpp::export()]]
arma::mat exemplo3(NumericMatrix x){
  arma::mat y=as<arma::mat>(x);
  return (y);
}

//[[Rcpp::export()]]
NumericMatrix exemplo4(arma::mat x){
  NumericMatrix y=wrap(x);
  return (y);
}

//[[Rcpp::export()]]
arma::mat exemplo5(arma::mat x){
  int r=x.n_rows;
  int c=x.n_cols;
  printf(" linhas:%d \n colunas:%d \n",r,c);
  return(x);
}


//[[Rcpp::export()]]
List exemplo6(int n, int m, double v){
  arma::mat x(n,m); x.fill(v);
  arma::colvec y( rnorm(n,0,1) ); 
  arma::rowvec z(rgamma(m,2,5));
  List retorno;
  retorno["X"]=x; retorno["Y"]=y; retorno["Z"]=z;
  return retorno;
}

//[[Rcpp::export()]]
double exemplo7(arma::mat x, int i, int j){
  return(x(i,j));
}

//[[Rcpp::export()]]
arma::mat exemplo8(arma::mat x, int i){
  return(x.row(i));
}

//[[Rcpp::export()]]
arma::mat exemplo9(arma::mat x, int j){
  return(x.col(j));
}

//[[Rcpp::export()]]
arma::mat exemplo10(arma::mat x){
  return (x+x);
}

//[[Rcpp::export()]]
arma::mat exemplo11(arma::mat x){
  return (x-x);
}

//[[Rcpp::export()]]
arma::mat exemplo12(arma::mat x){
  return (x%x);
}

//[[Rcpp::export()]]
arma::mat exemplo13(arma::mat x){
  return (exp(x));
}

//[[Rcpp::export()]]
arma::mat exemplo14(arma::mat x){
  return (x.t());
}

//[[Rcpp::export()]]
arma::mat exemplo15(arma::mat x){
  return (x.t()*x);
}

//[[Rcpp::export()]]
arma::mat exemplo16(arma::mat x){
  return (  (x.t()*x).i() );
}

//[[Rcpp::export()]]
arma::mat exemplo17(int i){
  arma::mat A(2,3);
  arma::mat B(i,1);
  A.randu(); A.print();
  B.randu(); B.print();
  return (A*B);
}









