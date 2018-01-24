##########################################################
## Script para execução do Bootstrap Usual e Bayesiano ###
##########################################################
set.seed(12345)

                                      ###########################
                                      #### Bootstrap Usual ######
                                      ###########################
#################################
### Bootstrap Não-Paramétrico ###
#################################
rm(list=ls(all=TRUE))
require(bootstrap)      

# EXEMPLO: Vamos supor que (y1, ..., yn) ~ U(0,theta), e que estamos interessados em corrigir o viés do EMV para theta.
# Além disso, vamos verificar se o vício relativo [(vicio/theta)*100] é menor para o estimador com ou sem correção.
# Vamos assumir: theta=10; n=20, B=1000

# Obs.: A correção Bootstrap para o vício de um estimador é dada por: theta_til = 2*ThetaHat - ThetaStar
# onde ThetaHat = max(yi) e ThetaStar = média das B amostras bootstrap

n<-20		  # tamanho da amostra
B<-1000		# nº de reptições bootstrap
theta<-10

ThetaHat<-NULL		
Theta_cor<-NULL		#estimativa do EMV corrigido

y<-runif(n,0,theta)

stat<-function(y){	
  n<-length(y)
  est<- max(y)
  return(est)
}
  
sampleBoot<-bootstrap(y,B,stat)			      # amostra bootstrap
ThetaStar<-mean(sampleBoot$thetastar)	    # média das B amostras bootstrap

ThetaHat<-max(y)				                #estimativa de MV sem a correção de viés
Theta_cor<-2*ThetaHat - ThetaStar		#estimativa MV com a correção bootstrap
					
# calculando o viés
vicio_ThetaHat = theta/(n+1)
vicio_Theta_cor = ((n-1)/(n+1))*theta - ThetaStar

vr.EMV<-abs((vicio_ThetaHat/theta)*100)          # viés relativo EMV
vr.Theta_cor<-abs((vicio_Theta_cor/theta)*100)   # viés relativo estimador com correção

### Construindo o IC percentílico com 95% de confiança
ic<- quantile(sampleBoot$t,probs=c(0.025,0.975))	# calcula o IC percentílico


#############################
### Bootstrap Paramétrico ###
#############################
rm(list=ls(all=TRUE))
require(boot)           # Faz tanto o boot não-paramétrico quanto o paramétrico


# Vamos supor que (y1, ..., yn) ~ N(0,1), e que estamos interessados em estimar a média populacional.

n<-100
B<-1000

y<-rnorm(n)
media<-mean(y)	#média para a amostra original. 

estat<-function(dados){mean(dados)}			# estatística de interesse na amostra bootstrap
# Gera v.as admitindo que a amostra vem de fato de uma Normal(0,1)
gera<-function(dados,mle){
  n<-length(dados)
  y.rep<-rnorm(n,mle,desvio)
  return(y.rep)
}

SampleBoot<-boot(y,statistic=estat,R=B,sim='parametric',ran.gen=gera,mle=media)

resultado<-as.vector(SampleBoot$t)
media1000<-mean(resultado)	#calcula a média das 1000 amostras bootstrap
ic2<-quantile(SampleBoot$t,probs=c(0.025,0.975))	# calcula o IC percentílico



                           ###############################
                           #### Bootstrap Bayesiano #####
                           ###############################
rm(list=ls(all=TRUE))
require(bayesboot)      

presidents <- read.csv("C://Users//laboratorio//Desktop//nivea//american_presidents.csv")

#Estimar a altura média a posteriori dos presidentes
b1 <- bayesboot(presidents$height_cm, mean)
summary(b1)
plot(b1)
b2<-bayesboot(presidents$height_cm, weighted.mean, use.weights = TRUE)
summary(b2)
plot(b2)

#Estimar a altura mediana a posteriori
n1 <- 3000  #(réplicas Bootstrap - tam da amostra a posteriori)
n2 <- 1000  #(tam da amostra)
n_data <- nrow(presidents)

weights <- rdirichlet(n1,rep(1,n_data)) # gerando os pesos
bb_median <- rep(NA, n1)
for(i in 1:n1) {
  data_sample <- sample(presidents$height_cm, size = n2, replace = TRUE, prob = weights[i,])
  bb_median[i] <- median(data_sample)
}
bbMedian<-mean(bb_median)
icred<-quantile(bb_median, c(0.025, 0.975)) # Int. de Credibilidade

#Modelo de Regressão Linear
blood.flow <- data.frame(
  dye = c(1.15, 1.7, 1.42, 1.38, 2.80, 4.7, 4.8, 1.41, 3.9),
  efp = c(1.38, 1.72, 1.59, 1.47, 1.66, 3.45, 3.87, 1.31, 3.75))

lm.coefs <- function(d, w) {
  coef( lm(efp ~ dye, data = d, weights = w) )
}
model <- bayesboot(blood.flow, lm.coefs, R = 1000, use.weights = TRUE)
summary(model)
