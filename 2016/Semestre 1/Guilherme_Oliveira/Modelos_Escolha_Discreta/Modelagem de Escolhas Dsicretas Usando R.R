############################################################################
#####              Seminário Stat4Good 20/05/2016                       ####
#####             Modelos logit para Escolhas Discretas                 ####
#####                     Guilherme Oliveira                            ####
############################################################################
rm(list=ls(all=TRUE))

###########################################################################
############# Modelo Logístico Multinomial Nominal ########################
if(!require(mlogit)){ install.packages("mlogit"); require(mlogit)}

dadosLMN <- read.table("dadosLMN.txt",sep="",header=TRUE)
head(dadosLMN)

dadosLMN$Disciplina <- as.factor(dadosLMN$Disciplina) #Categorizando "Disciplina"

## É necessário colocar os dados no formato da função "mlogit":
dadosLMN  <- mlogit.data(dadosLMN, choice="Disciplina", shape="wide") 
head(dadosLMN) 

modelo.lmn <- mlogit(Disciplina ~ 0 | ME + Idade, data = dadosLMN, reflevel="1")
summary(modelo.lmn)
mlogit.model$fitted.values


###########################################################################
############# Modelo Logístico Multinomial Ordinal ########################
if(!require(MASS)){ install.packages("mlogit"); require(MASS)}

dadosLMO <- read.table("dadosLMO.txt",sep="",header=TRUE)
head(dadosLMO)
dadosLMO$Sobrev <- as.factor(dadosLMO$Sobrev) #Categorizando "Sobrev"

modelo.lmo <- polr(Sobrev ~ Regiao + NivelToxico, data = dadosLMO,Hess=TRUE)
summary(modelo.lmo)
modelo.lmo$fitted.values


###########################################################################
############# Modelo Logístico Condicional Nominal ########################
if(!require(survival)){ install.packages("mlogit"); require(survival)}

dadosLCN <- read.table("dadosLCN.txt",sep="",header=TRUE)
head(dadosLCN)

modelo.lcn <- clogit(RES ~ D1+D2+D3+D4+D5+D6+strata(STR), data = dadosLCN)
summary(modelo.lcn)

#fim!
