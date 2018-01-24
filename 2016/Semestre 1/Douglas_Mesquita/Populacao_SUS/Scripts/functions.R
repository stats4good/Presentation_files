#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#-----######-#######---###---#######-#-----#--######-#######-#######-######-----#
#----#----------#----#-----#----#----#-----#-#-------#-----#-#-----#-#-----#----#
#-----#####-----#----##-#-##----#----#######-#--####-#-----#-#-----#-#-----#----#
#----------#----#----#-----#----#----------#-#-----#-#-----#-#-----#-#-----#----#
#----######-----#----#-----#----#----------#--######-#######-#######-######-----#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#------------------- Funções auxiliares

#--- skew.glm
skew.glm.robit <- function(f, data, convMethod = 'Nelder-Mead'){
  
  Dados <- model.frame(formula = f, data = data)
  
  GLM_init <- glm(formula = f, data = data, 
                  family = binomial("cauchit")) 
  
  y <- Dados[,1]
  X <- cbind(1, as.matrix(Dados[,-1], ncol = (ncol(Dados)-1)))
  Dados <- cbind(y, X)
  
  par_init <- c(df = 1, skew = 1, coef(GLM_init))
  maxit <- 10000
  
  optimization <- optim(par = par_init, 
                        fn = max.log.likelihood,             
                        y = y,
                        X = X,
                        minimize = TRUE,
                        control = list(maxit = maxit),
                        method = convMethod)
  
  parameters <- optimization$par
  convergence <- ifelse(optimization$convergence == 0, "O método convergiu.", "O método não convergiu.")
  
  Probs <- robit.linkinv(X = X, parameters = parameters)
  log_likelihood <- log.likelihood(y = y, probs = Probs)
  
  aic <- -2*(log_likelihood) + 2*(length(parameters))
  bic <- -2*(log_likelihood) + length(parameters)*log(length(y))
  
  out <- list(parameters = parameters, 
              fit.measures = data.frame(AIC = aic, BIC = bic),
              convergence = convergence, 
              loglike = log_likelihood,
              fitted.values = Probs,
              data = Dados)
  
  return(out)
  
}

#--- Dado um conjunto de parâmetros retorna probabilidades
robit.linkinv <- function(X, parameters){
  df <- parameters[1]
  dfTest <- df > 0
  gamma <- parameters[2]
  gammaTest <- gamma > 0
  beta <- parameters[-c(1,2)]
  Xbeta <- X%*%beta
  
  if(dfTest & gammaTest){
    Probs <- pskt(x = Xbeta, df = df, gamma = gamma)
  } else{
    Probs <- rep(NA, length(Xbeta))
  }
  
  return(Probs)
}

#--- Dado os dados e as probabilidades retorna a log-verossimilhança
log.likelihood <- function(y, probs){
  
  if(sum(is.na(probs)) == 0){
    Dens <- dbinom(x = y, size = 1, prob = probs, log = T)  
    ifelse(!is.finite(Dens), 0, Dens)
    LogDens <- sum(Dens) 
  } else{
    LogDens <- 0
  } 
  
  return(LogDens)
}

#--- Função que gera as probabilidades e estima a verossimilhança (usada na otmização)
max.log.likelihood <- function(y, X, parameters, minimize = TRUE){
  Probs <- robit.linkinv(X = X, parameters = parameters)
  log.like <- log.likelihood(y = y, probs = Probs)
  
  if(minimize){
    return(-log.like)
  } else{
    return(log.like)
  } 
}

#--- Parâmetros gráficos
nPoints <- 1000                  # Número de pontos nos gráficos de densidade
sizeLine <- 1.7                  # Tamanho da linha para os gráficos
sizeEmp <- 4                     # Tamanho da linha para os gráficos (Empírica)
family <- "sans"                 # Tipo de letra
sizeLegendTitle <- 20            # Tamanho do título da legenda
sizeLegend <- 15                 # Tamanho do texto da legenda
tickSize <- 15                   # Tamanho dos números dos eixos
axisText <- 20                   # Tamanho do título dos eixos

PanelCol <- "grey97"             # Cor do painel do gráfico
FacetCol <- "LightBlue"          # Cor dos "facets"
GridCol <- "grey70"              # Cor das linhas no painel do gráfico
LogitCol <- "#B276B2"            # Cor para as curvas da logit
CauchitCol <- "#5DA5DA"          # Cor para as curvas da cauchit
ProbitCol <- "#F15854"           # Cor para as curvas da probit
CloglogCol <-  "#60BD68"         # Cor para as curvas da gumbel
RobitFixCol <- "#F15854"         # Cor para as curvas da robit.fix
RobitCol <- "grey30"             # Cor para as curvas da robit
EmpiricaCol <- "#4D4D4D"         # Cor para as curvas empíricas

Tema <- theme(legend.background = element_rect(fill = 'transparent'),
              legend.title = element_text(family = family, size = sizeLegendTitle),
              legend.text = element_text(family = family, size = sizeLegend),
              axis.text = element_text(family = family,size = tickSize),
              axis.title = element_text(family = family,size = axisText),
              strip.text = element_text(family = family,size = axisText),
              panel.background = element_rect(fill = PanelCol),
              strip.background = element_rect(fill = FacetCol),
              panel.grid.major = element_line(colour = GridCol))

gsubDM <- function(x, pattern, replacement){
  names(replacement) <- pattern
  x <- replacement[as.character(x)]
  return(x)
}