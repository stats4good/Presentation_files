#include <oxstd.h>
#include <oxprob.h>

main(){
decl time;
time = timer();

decl n, mu, sigma, a, b;

n = 100000;
mu = 0;
sigma = 1;
a = -1;
b = 2;

decl Fa, Fb;

Fa = probn((a-mu)/sigma);
Fb = probn((b-mu)/sigma);

decl g, f, k;
g = 1/(b-a);
  
if(a<=mu&&b>=mu){
  f=(densn((mu-mu)/sigma)/sigma)*(1/sigma)/(Fb-Fa);
  k=f/g;
}
if(a>mu&&b>mu){
  f=(densn((a-mu)/sigma)/sigma)*(1/sigma)/(Fb-Fa);
  k=f/g;
}
if(a<mu&&b<mu){
  f=(densn((b-mu)/sigma)/sigma)*(1/sigma)/(Fb-Fa);
  k=f/g;
}

decl x=<>;
decl i = 0;

while(i <= n){
 decl xestrela, u, festrela, p;
 xestrela = ranu(1,1)*(b-a) + a;
 u = ranu(1,1);
 festrela = (densn((xestrela-mu)/sigma)/sigma)*(1/sigma)/(Fb-Fa);
 p=festrela/(k*g);
 if(u<=p){
  x = x ~ xestrela;
  ++i;
 }
}

savemat("rsnormal.mat",x,1);
println(timespan(time));


}