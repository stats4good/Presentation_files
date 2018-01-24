#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#-----######-#######---###---#######-#-----#--######-#######-#######-######-----#
#----#----------#----#-----#----#----#-----#-#-------#-----#-#-----#-#-----#----#
#-----#####-----#----##-#-##----#----#######-#--####-#-----#-#-----#-#-----#----#
#----------#----#----#-----#----#----------#-#-----#-#-----#-#-----#-#-----#----#
#----######-----#----#-----#----#----------#--######-#######-#######-######-----#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

require(data.table)              # Manipulação de base de dados
require(ggplot2)                 # Visualização
require(evd)                     # Distribuição gumbel
require(skewt)                   # Distribuição t assimétrica

setwd('~/Dropbox/Projetos/Stat4Good/2016/Semestre 1/PopulacaoSUS/')
source('Scripts/functions.R', encoding = "UTF-8")
set.seed(1)

#------------------- Estimação da população SUS

#--- Leitura dos dados

DadosPNAD2008 <- fread(input = "Dados/DadosPNAD2008.csv", 
                       colClasses = c("factor", 
                                      "numeric", 
                                      "numeric", 
                                      "factor", 
                                      "numeric", 
                                      "character", 
                                      "factor"))

#--- Histograma da renda
ggplot() +
  geom_histogram(data = DadosPNAD2008, aes(x = Renda, y = ..density..), 
                 fill = "steelblue", 
                 colour = "black",
                 alpha = 0.5) +
  #geom_density(data = DadosPNAD2008, aes(x = Renda)) + 
  ylab("Contagem") + xlab("Renda") + 
  theme_bw()

summary(DadosPNAD2008$Renda)
quantile(DadosPNAD2008$Renda, probs = seq(0,1, by = 0.1))
#--- 
Renda99 <- quantile(x = DadosPNAD2008$Renda, probs = 0.99)
Renda99 <- data.frame(xmin = Renda99, xmax = Inf, ymin = -Inf, ymax = Inf)
#---

#------------------ Distribuicao empirica
quantile(DadosPNAD2008$Renda, probs = seq(0, 1, length.out = 200))
DadosPNAD2008 <- DadosPNAD2008[Renda < 10000]
DadosPNAD2008$"RendaCat" <- cut(x = DadosPNAD2008$"Renda", b = 100, na.rm = T, right = F)

Emp <- DadosPNAD2008[, .(Plano = sum(Plano),
                         Total = length(Plano), 
                         PtMedio = mean(Renda)), 
                     by = RendaCat]

Emp <- Emp[, Distr := Plano/Total]

ggplot() + geom_line(data = Emp,  aes(x = PtMedio, y = Distr),
                     colour = EmpiricaCol,
                     size = sizeEmp, 
                     alpha = 0.5) +
  xlab("Renda média") + ylab("P(Plano de saúde | Renda média)") +
  geom_rect(data = Renda99, aes(xmin = xmin, 
                                xmax = xmax, 
                                ymin = ymin, 
                                ymax = ymax), fill = "black", alpha = 0.15) +
  theme_bw()

#--- skew.glm 
Cov <- "Renda"                                      # Covariável do modelo
f <- formula(paste('Plano ~', Cov))                 # Fórmula do modelo

glm.logit <- glm(formula = f, data = DadosPNAD2008, 
                 family = binomial("logit"))                

glm.probit <- glm(formula = f, data = DadosPNAD2008, 
                  family = binomial("probit"))              

glm.cauchit <- glm(formula = f, data = DadosPNAD2008, 
                   family = binomial("cauchit"))            

glm.cloglog <- glm(formula = f, data = DadosPNAD2008, 
                   family = binomial("cloglog"))   

glm.robit <- skew.glm.robit(f = f, data = DadosPNAD2008, 
                            convMethod = "Nelder-Mead")    # Nelder-Mead funciona, os demais não. Ideias do pq?

parameters <- data.table("Parâmetro" = c("df", "skew", "intercept", "renda"),
                         "logit" = c(1, 1, coef(glm.logit)),
                         "probit" = c(1, 1, coef(glm.probit)),
                         "cauchit" = c(1, 1, coef(glm.cauchit)),
                         "cloglog" = c(1, 1, coef(glm.cloglog)),
                         "robit" = glm.robit$parameters)

ggplot(data = data.frame(x = c(-10, 20), y = c(0, 1)), aes(x = x, y = y)) +
  stat_function(fun = function(x) dskt(x = x,
                                       df = parameters$robit[1], 
                                       gamma = parameters$robit[2]), 
                aes(colour = "Logística"), size = sizeLine, n = nPoints) +
  ylab("Densidade") + xlab("Domínio") + 
  theme_bw() +
  Tema +
  theme(legend.position = 'none')

pred <- data.table(cov = glm.logit$data$Renda,
                   logit = glm.logit$fitted.values,
                   probit = glm.probit$fitted.values,
                   cauchit = glm.cauchit$fitted.values,
                   cloglog = glm.cloglog$fitted.values,
                   robit = glm.robit$fitted.values)

pred <- unique(pred)

pred <- melt(pred, id.vars = "cov", variable.name = "Modelo", value.name = "Prob")

ggplot() + 
  geom_line(data = pred, aes(x = cov, y = Prob, colour = Modelo), size = sizeLine) + 
  geom_line(data = Emp,  aes(x = PtMedio, y = Distr, colour = "empírica"),
            size = sizeEmp, 
            alpha = 0.5) +
  scale_colour_manual("Distribuições", values = c("logit" = LogitCol, 
                                                  "probit" = ProbitCol, 
                                                  "cauchit" = CauchitCol,
                                                  "cloglog" = CloglogCol,
                                                  "empírica" = EmpiricaCol,
                                                  "robit" = RobitCol)) +
  xlab("Renda") + ylab("P(Plano de saúde | Renda)") +
  geom_rect(data = Renda99, aes(xmin = xmin, 
                                xmax = xmax, 
                                ymin = ymin, 
                                ymax = ymax), fill = "black", alpha = 0.15) +
  theme_bw() +
  Tema

#--- AIC

AIC_glm <- data.table(logit = glm.logit$aic,
                      probit = glm.probit$aic,
                      cauchit = glm.cauchit$aic,
                      cloglog = glm.cloglog$aic,
                      robit = glm.robit$fit.measures$AIC)

BIC_glm <- data.table(logit = BIC(object = glm.logit),
                      probit = BIC(object = glm.probit),
                      cauchit = BIC(object = glm.cauchit),
                      cloglog = BIC(object = glm.cloglog),
                      robit = glm.robit$fit.measures$BIC) 

#--- Dados populacionais das cidades brasileiras

Pop <- fread(input = "Dados/POPULACAO_IBGE_2010.csv", 
             colClasses = c("character", 
                            "factor", 
                            "factor", 
                            "numeric", 
                            "numeric"),
             header = T, sep = ",", dec = ".", encoding = "UTF-8")

X <- model.matrix(~ RENDA_MEDIA, data = Pop)
param <- glm.robit$parameters
ProbSUS <- 1-robit.linkinv(X = X, parameters = param) 

Pop$"POP_GLM" <- ceiling(as.numeric(ProbSUS*Pop$"POP"))

PopCidades <- Pop[, .(POP = sum(POP), POP_GLM = sum(POP_GLM)), 
                  by = .(SIGLA_UF, CO_MUNICIPIO_IBGE)]

#--- Comparando o resultado com os dados da ANS

#---------------------Planos

NumeroPlanos <- fread(input = "Dados/PLANOS_ANS_2008.csv",
                      colClasses = c("factor", 
                                     "numeric", 
                                     "numeric", 
                                     "numeric"),
                      sep = ";")
names(NumeroPlanos) <- c("CO_MUNICIPIO_IBGE", "Assist_Medica", "Plano_Odontologico", "Total")

NumeroPlanos$"CO_MUNICIPIO_IBGE" <- toupper(iconv(NumeroPlanos$"CO_MUNICIPIO_IBGE", to = "ASCII//TRANSLIT"))
NumeroPlanos$"CO_MUNICIPIO_IBGE" <- gsub(pattern = "[A-Z]", replacement = "", x = NumeroPlanos$"CO_MUNICIPIO_IBGE")
NumeroPlanos$"CO_MUNICIPIO_IBGE" <- substr(x = NumeroPlanos$"CO_MUNICIPIO_IBGE", start = 1, stop = 6)
PopCidades$"CO_MUNICIPIO_IBGE" <- substr(x = PopCidades$"CO_MUNICIPIO_IBGE", start = 1, stop = 6)

setkey(NumeroPlanos, CO_MUNICIPIO_IBGE)
setkey(PopCidades, CO_MUNICIPIO_IBGE)

PopCidades <- merge(NumeroPlanos, PopCidades)

PopCidades[, POP_ANS := POP - Assist_Medica]
PopCidades <- dplyr::select(PopCidades, CO_MUNICIPIO_IBGE, SIGLA_UF, POP, POP_GLM, POP_ANS)

#--- Gráficos

PopCidades <- PopCidades[SIGLA_UF != "DF"]

ggplot(PopCidades, aes(x = POP_ANS, y = POP_GLM)) + 
  geom_point(col = "steelblue", alpha = 1, size = 2.5) +
  geom_abline(aes(intercept = 0, slope = 1), alpha = 0.8, size = 2, colour = "IndianRed") +
  ylim(c(0, 50000)) + xlim(c(0, 50000)) +
  xlab(label = "População SUS - ANS") + ylab(label = "População SUS - GLM") +
  facet_wrap(~SIGLA_UF) +
  theme_bw() +
  Tema

#--- Estamos subestimando a verdadeira população SUS. Provavelmente alguma covariável importante não está no modelo.
#--- Ideia: Corrigir linearmente a partir de um modelo linear.

f1 <- POP_ANS ~ -1 + POP_GLM
Reg <- lm(formula = f1, data = PopCidades)
Fator <- coef(Reg)

PopCidades <- PopCidades[, POP_SUS := ceiling(POP_GLM*Fator)]

ggplot(PopCidades, aes(x = POP_ANS, y = POP_SUS)) + 
  geom_point(col = "steelblue", alpha = 1, size = 2.5) +
  geom_abline(aes(intercept = 0, slope = 1), alpha = 0.8, size = 2, colour = "IndianRed") +
  ylim(c(0, 50000)) + xlim(c(0, 50000)) +
  xlab(label = "População SUS - ANS") + ylab(label = "População SUS - GLM") +
  facet_wrap(~SIGLA_UF) +
  theme_bw() +
  Tema

#--- Até o momento nós tinhamos a "verdade" para comparar. Porém, o objetio final é encontrar a população SUS em bairros.
#--- Basta aplicar a correção linear visto que cidades são formadas por conjuntos de bairros.
#--- Vejamos o caso de BH.

PopBH <- Pop[CO_MUNICIPIO_IBGE == "3106200"]
PopBH <- PopBH[, POP_SUS := ceiling(POP_GLM*Fator)]
PopBH$POP_SUS <- ifelse(PopBH$POP_SUS > PopBH$POP, PopBH$POP, PopBH$POP_SUS)

#--- Mapa

PopCidades$POP_SUS <- ifelse(PopCidades$POP_SUS > PopCidades$POP, PopCidades$POP, PopCidades$POP_SUS)
PopEstados <- PopCidades[, list(POP_SUS = sum(POP_SUS), POP = sum(POP)), by = SIGLA_UF]
PopEstados <- PopEstados[, Prop := POP_SUS/POP]

require(mapsBR)
require(RColorBrewer)

data(regUF)

regUF@data <- merge(regUF@data, PopEstados, 
                    by.x = "COD", by.y = "SIGLA_UF", all.x = T)

breaks <- quantile(regUF@data$Prop, probs = seq(0, 1, length.out = 10), na.rm = T)
regUF@data$breaks <- factor(round(breaks[findInterval(regUF@data$Prop, breaks)], 4))

spplot(regUF, "breaks", col.regions = colorRampPalette(c("blue", "red"))(10))
