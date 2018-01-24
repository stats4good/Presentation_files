rm(list=ls())
library(spdep)
require(maptools)
require(spGoogle)
require(mapsBR)
require(data.table)
require(ggplot2)

cenario <- 1

if (cenario == 1) {
  eventos <- as.data.table(matrix(c(312520,0,
                                    313040,1,
                                    312700,0,
                                    314720,6,
                                    310620,1262),nrow = 5, ncol=2,byrow = TRUE))
} else if (cenario == 2) {
  eventos <- as.data.table(matrix(c(312520,1,
                                    313040,2,
                                    312700,0,
                                    314720,6,
                                    310620,1262),nrow = 5, ncol=2,byrow = TRUE))
}


colnames(eventos) <- c("CO_MUNICIPIO_IBGE","NU_OBITO")

pop <- as.data.table(matrix(c(312520,24,
                              313040,78,
                              312700,143,
                              314720,323,
                              310620,39591),nrow = 5, ncol=2,byrow = TRUE))

colnames(pop) <- c("CO_MUNICIPIO_IBGE","NU_NASC_VIVOS")


munic <- as.data.table(data.frame(CO_MUNICIPIO_IBGE = c(312520,
                                                        313040,
                                                        312700,
                                                        314720,
                                                        310620
                                                        ),NO_MUNICIPIO = c( "FAMA",
                                                                            "IJACI",
                                                                            "FRONTEIRA",
                                                                            "PARAGUACU",
                                                                            "BELO HORIZONTE"
                                                                          ),stringsAsFactors=FALSE))


####################################
# Estimador de M?xima Veroximilhan?a
####################################
emv <- as.data.table(cbind(eventos$CO_MUNICIPIO_IBGE,round(eventos$NU_OBITO / pop$NU_NASC_VIVOS,3)))
colnames(emv) <- c("CO_MUNICIPIO_IBGE","EMV")
emv <- merge(emv,munic,by="CO_MUNICIPIO_IBGE")
emv <- emv[order(NO_MUNICIPIO)]

###################################################################
# Estimador de Bayesiano : a priori gama(3,10), eventos ~ Poisson()
###################################################################
bayesiano <- as.data.table(cbind(eventos$CO_MUNICIPIO_IBGE,round((3 + eventos$NU_OBITO)/(100 + pop$NU_NASC_VIVOS),3)))
colnames(bayesiano) <- c("CO_MUNICIPIO_IBGE","Bayesiano")

################################################
# Estimador de Bayesiano Emp?rico - EBest(spdep)
################################################
bayesEmp <- as.data.table(cbind(eventos$CO_MUNICIPIO_IBGE,round(EBest(eventos$NU_OBITO,pop$NU_NASC_VIVOS),3)))
colnames(bayesEmp) <- c("CO_MUNICIPIO_IBGE","EMV_EBest","EBest")

# dados : serie historica
# pop : 

pop.total <- pop[,sum(NU_NASC_VIVOS)]  # calculo da populacao de referencia
pop.media <- pop[,mean(NU_NASC_VIVOS)]	

bemp <- function(p.eventos, p.pop, p.pop.total, p.pop.media){
  theta <- matrix(NA, nrow = nrow(p.eventos), ncol = 1)
  m <- as.numeric(sum(p.eventos[,2])/p.pop.total) # media brasileira
  r <- p.eventos[,2]/p.pop[,2]                 # media de cada municipio
  V <- as.numeric((sum(p.pop[,2]*(r-m)^2)/p.pop.total)-m/p.pop.media) # calculo da variancia
  w <- V/(V+(m/p.pop[,2]))                 # calculo dos pesos    
  theta <- w*r + (1-w)*m             # taxa bayesiana empirica por cidade
  return(theta)  
}

bayesEmpImp <- as.data.table(round(bemp(as.data.frame(eventos), as.data.frame(pop), as.data.frame(pop.total), as.data.frame(pop.media)),3))
bayesEmpImp <-  cbind(munic$CO_MUNICIPIO_IBGE,bayesEmpImp)
colnames(bayesEmpImp) <- c("CO_MUNICIPIO_IBGE","BEmp")

result <- as.data.table(merge(emv,bayesEmp))
result <- as.data.table(merge(result,bayesiano))
result <- as.data.table(merge(result,bayesEmpImp))
result <- result[order(NO_MUNICIPIO)]
result <- result[, .(CO_MUNICIPIO_IBGE, NO_MUNICIPIO,EMV,EMV_EBest,EBest,BEmp,Bayesiano)]

##################
# Desenha os Mapas
##################

data("regMun")

regMun$COD <- trunc(regMun$COD/10)

regMun <- merge(regMun, as.data.frame(result), by.x = "COD", by.y = "CO_MUNICIPIO_IBGE", all.x = T)
regMun <- subset(regMun, UF == 'MG' )

resultPlot <- melt(result, id.vars = 1:2, variable.name = "variable", value.name = "Taxa")
setnames(resultPlot, old = 'variable', new = 'Metodo')

resultPlot <- subset(resultPlot, Metodo == 'EMV' | Metodo == 'BEmp')

  
ggplot(resultPlot, aes(x=NO_MUNICIPIO, y=Taxa, group = Metodo, fill = Metodo)) +
  geom_bar(stat="identity", colour="black",position=position_dodge()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=70, hjust=1,size=10)) +
  labs(x = "Munic?pios") +
  ggtitle(paste("Taxa Bayesiana Emp?rica : ",cenario)) +
  scale_y_continuous("Taxa",limits=c(0,0.05)) + 
  scale_x_discrete("Municipios") +
  scale_fill_discrete()

spGplot(subset(regMun, UF == 'MG' ), var = 'EMV', cuts = 5,  
        description = list(title = 'EMV', var = c('EMV', 'NOME',"Bayesiano")), maptype = 'roadmap',
        col.pallete = list(col = rev(heat.colors(5)), alpha = 0.9))

