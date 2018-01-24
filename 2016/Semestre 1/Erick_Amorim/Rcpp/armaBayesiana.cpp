#include<RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]

using namespace Rcpp;

//[[Rcpp::export]]
List gibbsNG(arma::colvec x, int N){
  int n= x.size();
  arma::mat theta(N,2);
  double mi=0, phi=1;     /* chutes iniciais */
  double a=1, b=1;        /* valores para os hiperparâmetros da a priori */
  double  m=0, v=1;  
  for(int i=0; i<N; i++)
  {
    double A = a + 0.5*n ; 
    double B=0;
    for(int j=0; j<n; j++){ B += pow( (x(j,0)-mi) ,2) ; }
    B = b + 0.5*B;
    phi=R::rgamma(A,1/B);  /* atualiza phi com base em A e B */
    double V = 1/( (n*phi) + (1/v) );
    double M = V*( (m/v) + sum(x)*phi ); 
    mi = R::rnorm(M,V);
    theta(i,0)=mi;
    theta(i,1)=phi;
  }
  List saida;
  saida["cadeias"]=theta;
  return saida;
}

//[[Rcpp::export]]
List GibbsRegress (arma::mat y, arma::mat x, int N=1000, int burnin=500){
  int n = y.n_cols;
  arma::mat beta(3,1); beta.fill(0);
  double sigma2 =1;
  arma::mat sbeta(N-burnin,3); //
  arma::colvec ssigma2(N-burnin); //
  /* valores para os hiperparâmetros da a priori */
  double a=2.1, b=1.1, m_0=0, m_1=0, m_2=0, v_0=10, v_1=10, v_2=10;
  
  for(int t=0; t<N; t++){
    arma::mat mi = x*beta ;
    double soma = arma::as_scalar( sum( pow( y-mi, 2) ) );
    double A = a + 0.5*n;
    double B = b + 0.5*soma;
    sigma2 = 1/R::rgamma(A,1/B); // gerando sigma2
    //printf("sigma2:%f \n",sigma2);
    double rest0 = 0, soma1=0;
    
    for(int i=0; i<n; i++){
      rest0 = arma::as_scalar( x.row(i)*beta - beta(0,0)*x(i,0) );
      soma1 += ( y(i,0)-rest0);
    } 
    double V0= 1/( (n/sigma2)+(1/v_0) );
    double M0= V0*( (soma1/sigma2) + (m_0/v_0));
    beta(0,0)=R::rnorm(M0,sqrt(V0)); // gerando b0
    double rest1=0, soma2=0;
    for(int i=0; i<n; i++){
      rest1 = arma::as_scalar( x.row(i)*beta - beta(1,0)*x(i,1) );
      soma2 += x(i,1)*( y(i,0)-rest1);
      } 
    double V1=1/( arma::as_scalar( (sum( pow(x.col(1),2) ))/sigma2 ) + (1/v_1));
    double M1=V1*( (soma2/sigma2) + (m_1/v_1) );
    beta(1,0)=R::rnorm(M1,sqrt(V1));
    
    double rest2=0, soma3=0;
    for(int i=0; i<n; i++){
      rest2 = arma::as_scalar( x.row(i)*beta - beta(2,0)*x(i,2) );
      soma3 += x(i,2)*( y(i,0)-rest2);
      } 
    double V2=1/( arma::as_scalar( (sum( pow(x.col(2),2 ) ))/sigma2) +(1/v_2) );
    double M2=V2*((soma3/sigma2) + (m_2/v_2) );
    beta(2,0)=R::rnorm(M2,sqrt(V2));
    if(t>burnin){
      sbeta.row(t-burnin)=beta.t();
      ssigma2(t-burnin)=sigma2; }
  }
  List saida;
  saida["betas"]=sbeta;
  saida["sigma2"]=ssigma2;
  return saida;
}




