/* Observa��o: a cada quebra de linhas � um novo programa (fiz isso apenas para evitar v�rios arquivos!).
Deve ser retirado os coment�rios parte por parte */



/* Primeiro programa */



#include <oxstd.h>

main() {

println("oi");

}



/* Criando matrizes */



//#include <oxstd.h>
//
//main() {
//
//decl x1, x2, x3, x4, x5, x6;
//
//x1 = <1,2;3,4>;
//
//x2 = <1,2,3,4>;
//
//x3 = zeros(2,2);
//
//x4 = ones(3,3);
//
//x5 = unit(3,3);
//
//x6 = constant(2,2,3);
//
////println("x1", x1, "x2", x2,"x3", x3,"x4", x4, "x5", x5, "x6", x6);
//
////println(vecindex(x2 .== 2));
//
////println("x1", x1, "x1_{11} = ", x1[0][0]);
//
////println(0 ~ x1);
//
////println(0 | x1);
//
////println("x1", x1, "x6", x6, "x1 * x6", x1 * x6);
//
////println(x1^2, x1 * x1);
//
//}



/* Utilizando uma fun��o externa */



//#include <oxstd.h>
//#include <rsnormalFunc.ox>
//
//main(){
//
//decl x;
//
//x = rsnormal(10, 0, 1, 3, 4);
//
//println(x);
//
//}



/* Salvando os resultados em arquivo externo */



//#include <oxstd.h>
//#include <oxprob.h>  //pacote carregado para gerar de uma distribui��o
//
//main(){
//
//decl x;
//
//x = rann(100,1)*2 + 1; //gerando 100 observa��es (em vetor coluna) de uma Normal(1,4)
//
//savemat("normal_Ox.mat",x,1); //ser� criado um arquivo "normal_Ox.mat" contendo os resultados guardados em x.
//								//O �ltimo argumento = 1 � para especificar que n�o deve ser salvo no documento as dimens�es da matriz:
//								//assim � ideal para leitura em outros programas (para parte gr�fica, por exemplo). Caso for utilizar o arquivo no pr�prio Ox,
//								//deve-se manter a primeira linha com a dimens�o da matriz, omitindo o ultimo argumento da fun��o savemat)
//
//}



/* Loops: for e while. If */



//#include <oxstd.h>
//
//main(){
//
//decl i;
//
////for(i = 0; i < 5; ++i){
////	println(i);
////}
//
////for(i = 4; i >= 0; --i){
////	println(i);
////}
//
////for(i = 0; i < 10; i += 2){
////	println(i);
////}
//
////i=0;
////while(i < 10){
////	println(i);
////	++i;
////}
//
////decl A;
////A = ones(2,3);
////
////if(rows(A) == columns(A)){
////	println("a matriz e quadrada");
////}
////else{
////	println("a matriz nao e quadrada");
////}
//
//}



/* Teste: If - maneira eficiente */



//#include <oxstd.h>
//
//main(){
//
////decl x, xpos;
////x = <-1,2,2,-2,1>;
////
////decl i;
////for(i = 0; i < columns(x); ++i){
////
////	if(x[i] .< 0){
////		x[i] = 0;
////	}
////	xpos = x;
////}
////println(xpos);
//
////xpos = x .< 0 .? 0 .: x;   // (teste) .? ("se fa�a ...") .: ("caso contr�rio fa�a ...")
////println(xpos);
//
////decl y, ypos;
////y = <-1,2,2;-2,1,-1;0,1,-1>;
////
////decl i, j;
////for(i = 0; i < rows(y); ++i){
////for(j = 0; j < columns(y); ++j){
////
////	if(y[i][j] .< 0){
////		y[i][j] = 0;
////	}
////	ypos = y;
////}
////}
////println(ypos);
//
////ypos = y .< 0 .? 0 .: y;
////println(ypos);
//
//
//
////println(xpos);
//
//
//}



/* Leitura de dados externos */



//#include <oxstd.h>
//
//main(){
//decl dados1, dados2;
//dados1 = loadmat("dados.mat");	//leitura dos dados externos: observe que a primeira linha do arquivo dados.mat � a dimens�o da matriz que ser� inserida
//dados2 = dropc(dados1,0);	//retirando a primeira coluna dos dados (vari�vel de identifica��o)
//
//println(meanc(dados2), varc(dados2), sumc(dados2));	 //etc...
//
//}