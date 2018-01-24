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

#------------------- Distribuições: Logística, Normal, Cauchy, Log-Weibull (Gumbel)
#--- Densidade

ggplot(data = data.frame(x = c(-10, 10), y = c(0, 1)), aes(x = x, y = y)) +
  stat_function(fun = dlogis, aes(colour = "Logística"), size = sizeLine, n = nPoints) +
  stat_function(fun = dnorm, aes(colour = "Normal"), size = sizeLine, n = nPoints) +
  stat_function(fun = dcauchy, aes(colour = "Cauchy"), size = sizeLine, n = nPoints) + 
  stat_function(fun = dgumbel, aes(colour = "Gumbel"), size = sizeLine, n = nPoints) + 
  ylab("Densidade") + xlab("Domínio") +
  scale_colour_manual("Distribuições", values = c("Logística" = LogitCol, 
                                                  "Normal" = ProbitCol, 
                                                  "Cauchy" = CauchitCol,
                                                  "Gumbel" = CloglogCol)) +
  theme_bw() +
  Tema

#--- Acumulada

ggplot(data = data.frame(x = c(-20, 20), y = c(0, 1)), aes(x = x, y = y)) +
  stat_function(fun = plogis, aes(colour = "Logística"), size = sizeLine, n = nPoints) +
  stat_function(fun = pnorm, aes(colour = "Normal"), size = sizeLine, n = nPoints) +
  stat_function(fun = pcauchy, aes(colour = "Cauchy"), size = sizeLine, n = nPoints) + 
  stat_function(fun = pgumbel, aes(colour = "Gumbel"), size = sizeLine, n = nPoints) + 
  ylab("Densidade") + xlab("Domínio") +
  scale_colour_manual("Distribuições", values = c("Logística" = LogitCol, 
                                                  "Normal" = ProbitCol, 
                                                  "Cauchy" = CauchitCol,
                                                  "Gumbel" = CloglogCol)) +
  theme_bw() +
  Tema

#--- Gerando dados
df <- 2
gamma <- 1.5
n <- 1000
X <- cbind(1, rnorm(n = n))
betas <- c(1, 3)
Xbeta <- X%*%betas
prop <- pskt(x = Xbeta, df = df, gamma = gamma)
y <- rbinom(n = n, size = 1, prob = prop)

DadosGLM <- data.table(y = y, X)

#------------------- GLM com função de ligação simétrica
glm.logit <- glm(formula = y ~ V2, data = DadosGLM, 
                 family = binomial("logit"))                # Acumulada da logística

glm.probit <- glm(formula = y ~ V2, data = DadosGLM, 
                  family = binomial("probit"))              # Acumulada da normal

glm.cauchit <- glm(formula = y ~ V2, data = DadosGLM, 
                   family = binomial("cauchit"))            # Acumulada da cauchy

glm.cloglog <- glm(formula = y ~ V2, data = DadosGLM, 
                   family = binomial("cloglog"))            # Acumulada da distribuição de valores extremos (ou log-Weibull ou (Gumbel))

#--- Verificando o ajsute

pred <- data.table(cov = X[,2],
                   logit = glm.logit$fitted.values,
                   probit = glm.probit$fitted.values,
                   cauchit = glm.cauchit$fitted.values,
                   cloglog = glm.cloglog$fitted.values)

pred <- melt(pred, id.vars = "cov", variable.name = "Modelo", value.name = "Prob")

#--- Distribuição empírica

nPoint <- 20
DadosGLM$"Cat" <- cut(x = DadosGLM$"V2", b = nPoint, na.rm = T)

Emp <- DadosGLM[, .(y = sum(y),
                    Total = length(y),
                    PtMedio = mean(V2)), 
                by = Cat]

Emp <- Emp[, Distr := y/Total]

ggplot(data = pred, aes(x = cov, y = Prob, colour = Modelo)) + 
  geom_line(size = sizeLine) + 
  geom_line(data = Emp,  aes(x = PtMedio, y = Distr, colour = "empírica"),
            size = sizeEmp, 
            alpha = 0.5) +
  scale_colour_manual("Distribuições", values = c("logit" = LogitCol, 
                                                  "probit" = ProbitCol, 
                                                  "cauchit" = CauchitCol,
                                                  "cloglog" = CloglogCol,
                                                  "empírica" = EmpiricaCol)) +
  xlab("Covariável") + ylab("P(y = 1 | Covariável)") +
  theme_bw() +
  Tema

#--- AIC

AIC_glm <- data.table(logit = glm.logit$aic,
                      probit = glm.probit$aic,
                      cauchit = glm.cauchit$aic,
                      cloglog = glm.cloglog$aic)

BIC_glm <- data.table(logit = BIC(object = glm.logit),
                      probit = BIC(object = glm.probit),
                      cauchit = BIC(object = glm.cauchit),
                      cloglog = BIC(object = glm.cloglog)) 

#------------------- GLM com função de ligação assimétrica

#--- Montando uma função de ligação para usar no glm
#--- ?make.link

#               linkfun - Link function function(mu)
#               linkinv - Inverse link function function(eta)
#               mu.eta - Derivative function(eta) dmu/deta
#               valideta	- function(eta){ TRUE if eta is in the domain of linkinv }.
#               name	- A name to be used for the link

robit <- list()
robit$name <- "robit"
linkfun <- function(x) qskt(p = x, df = df, gamma = gamma)
linkinv <- function(x) pskt(x = x, df = df, gamma = gamma)
robit$linkfun <- function(x) qskt(p = x, df = df, gamma = gamma)
robit$linkinv <- function(x) pskt(x = x, df = df, gamma = gamma)
robit$mu.eta <- function(x) dskt(x = x, df = df, gamma = gamma)
robit$valideta <- function(eta) all(is.finite(eta))
class(x = robit) <- 'link-glm'

glm.robit.fix <- glm(formula = y ~ V2, data = DadosGLM, 
                     family = binomial(robit))

pred <- data.table(cov = X[,2],
                   cauchit = glm.cauchit$fitted.values,
                   robit.fix = glm.robit.fix$fitted.values)

pred <- melt(pred, id.vars = "cov", variable.name = "Modelo", value.name = "Prob")

ggplot(data = pred, aes(x = cov, y = Prob, colour = Modelo)) + 
  geom_line(size = sizeLine) + 
  geom_line(data = Emp,  aes(x = PtMedio, y = Distr, colour = "empírica"),
            size = sizeEmp, 
            alpha = 0.5) +
  scale_colour_manual("Distribuições", values = c("cauchit" = CauchitCol, 
                                                  "robit.fix" = RobitFixCol,
                                                  "empírica" = EmpiricaCol)) +
  xlab("Covariável") + ylab("P(y = 1 | Covariável)") +
  theme_bw() +
  Tema

AIC_glm$robit.fix <- glm.robit.fix$aic
BIC_glm$robit.fix <- BIC(glm.robit.fix)

#--- Montando uma função que estime parâmetros da função de ligação (criando um skew.glm)

f <- y ~ V2
data <- DadosGLM

convMethod <- "Nelder-Mead" # Opções: "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"

glm.robit <- skew.glm.robit(f = f, data = data, convMethod = convMethod)

AIC_glm$robit <- glm.robit$fit.measures$AIC                       # AIC_glm$robit.fix = AIC_glm$robit.fix + 2*2
BIC_glm$robit <- glm.robit$fit.measures$BIC                       # BIC_glm$robit.fix = BIC_glm$robit.fix + 2*log(length(y))

parameters <- data.table("Parâmetro" = c("df", "skew", "intercept", "covariavel"),
                         "Verdadeiro" = c(df, gamma, betas),
                         "logit" = c(1, 1, coef(glm.logit)),
                         "probit" = c(1, 1, coef(glm.probit)),
                         "cauchit" = c(1, 1, coef(glm.cauchit)),
                         "cloglog" = c(1, 1, coef(glm.cloglog)),
                         "robit" = glm.robit$parameters)

#--- Gráfico dos melhores ajustes

pred <- data.table(cov = X[,2],
                   cauchit = glm.cauchit$fitted.values,
                   robit.fix = glm.robit.fix$fitted.values,
                   robit = glm.robit$fitted.values)

pred <- melt(pred, id.vars = "cov", variable.name = "Modelo", value.name = "Prob")

ggplot(data = pred, aes(x = cov, y = Prob, colour = Modelo)) + 
  geom_line(size = sizeLine) + 
  geom_line(data = Emp,  aes(x = PtMedio, y = Distr, colour = "empírica"),
            size = sizeEmp, 
            alpha = 0.5) +
  scale_colour_manual("Distribuições", values = c("cauchit" = CauchitCol, 
                                                  "robit.fix" = RobitFixCol,
                                                  "robit" = RobitCol,
                                                  "empírica" = EmpiricaCol)) +
  xlab("Covariável") + ylab("P(y = 1 | Covariável)") +
  theme_bw() +
  Tema
