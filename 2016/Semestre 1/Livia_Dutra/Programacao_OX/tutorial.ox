/* Observação: a cada quebra de linhas é um novo programa (fiz isso apenas para evitar vários arquivos!).
Deve ser retirado os comentários parte por parte */



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



/* Utilizando uma função externa */



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
//#include <oxprob.h>  //pacote carregado para gerar de uma distribuição
//
//main(){
//
//decl x;
//
//x = rann(100,1)*2 + 1; //gerando 100 observações (em vetor coluna) de uma Normal(1,4)
//
//savemat("normal_Ox.mat",x,1); //será criado um arquivo "normal_Ox.mat" contendo os resultados guardados em x.
//								//O último argumento = 1 é para especificar que não deve ser salvo no documento as dimensões da matriz:
//								//assim é ideal para leitura em outros programas (para parte gráfica, por exemplo). Caso for utilizar o arquivo no próprio Ox,
//								//deve-se manter a primeira linha com a dimensão da matriz, omitindo o ultimo argumento da função savemat)
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
////xpos = x .< 0 .? 0 .: x;   // (teste) .? ("se faça ...") .: ("caso contrário faça ...")
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
//dados1 = loadmat("dados.mat");	//leitura dos dados externos: observe que a primeira linha do arquivo dados.mat é a dimensão da matriz que será inserida
//dados2 = dropc(dados1,0);	//retirando a primeira coluna dos dados (variável de identificação)
//
//println(meanc(dados2), varc(dados2), sumc(dados2));	 //etc...
//
//}