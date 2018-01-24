rm(list=ls())
#install.packages("RcppArmadillo")
library("RcppArmadillo")
library("Rcpp")
wd = getwd()
sourceCpp(paste(wd,"/armaexemplos.cpp",sep=""))

exemplo1(diag(3))
exemplo2(c(2,4,6,8))
exemplo3(diag(4)) #transforma de NumericMatrix para arma
exemplo4(matrix(runif(12),3,4)) #transforma de arma para NumericMatrix
exemplo5(matrix(runif(8),4,2)) #print e dimensao
exemplo6(3,4,4/5) #lista

Z=matrix(rnorm(6),2,3)
exemplo7(Z,1,2) #elemento (i+1,j+1) da matriz Z
exemplo8(Z,1)   #linha (i+1) da matriz
exemplo9(Z,2)   #coluna (i+1) da matriz

exemplo10(Z)  #realiza a soma Z+Z
exemplo11(Z)  #realiza a diferenca Z-Z
exemplo12(Z)  #multiplica elemento por elemento de Z
exemplo13(Z)  #calcula exponencial de Z
exemplo14(Z)  #calcula a transposta de Z
exemplo15(Z)  #multiplicacao
exemplo16(Z)  #matriz inversa da multiplicacao no exemplo 15

exemplo17(3)  # nao havera mensagem de erro (dimensao ok!)
exemplo17(2)  # aqui havera mensagens de erro (dimensao nao ok)

