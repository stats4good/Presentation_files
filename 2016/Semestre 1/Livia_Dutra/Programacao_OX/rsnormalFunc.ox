rsnormal(const n, const mu, const sigma, const a, const b){

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

return x;

}