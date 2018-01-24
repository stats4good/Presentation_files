require(XML)
require(dicionariosIBGE)
require(data.table)

setwd('~/Dropbox/Projetos/Stat4Good/2016/Semestre 1/PopulacaoSUS/')
#------------------- Obtendo as bases usadas no trabalho e manipulando-as

# Dados: PNAD 2008
# Endereço: ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2009/PNAD_reponderado_2008.zip

url <- 'ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/microdados/reponderacao_2001_2009/PNAD_reponderado_2008.zip'
dir.create(path = "DadosNet/PNAD_2008")

download.file(url = url, destfile = "DadosNet/PNAD_2008/pnad2008.zip")
unzip(zipfile = "DadosNet/PNAD_2008/pnad2008.zip", exdir = "DadosNet/PNAD_2008/")

data(dicPNAD2008)                                               # Dados da PNAD 2008

#--- Adaptação da função feita pelo Elias - UFPR

read.fixed.width <- function(file.inp,                          # Arquivo de origem dos dados    
                             file.out,                          # Arquivo de destino dos dados selecionados
                             first,                             # Posicao de inicio da variavel
                             last,                              # Posicao de fim da variavel
                             buffer = 1000)                     # Número de linhas lidas a cada vez (meu pc não aguenta)
{
  nreg <- 0                                                     # 0 Registros lidos no inicio do processo
  con.inp <- file(file.inp, "r")                                # Abrir conexão com o arquivo para leitura
  repeat {                                                      # Repetir os passos até o processo ler todos os registros
    temp <- readLines(con.inp, buffer)                          # Ler buffer linhas do arquivo file.inp
    nreg <- nreg + length(temp)                                 # Incrementa o número de registros
    if (length(temp)>0) {                                       # Leu alguma coisa?                                   
      cat("Lidos:", length(temp), "registros.\n")       
      temp <- t(sapply(temp, substring,                         # Linha por linha pega determinadas posições do arquivo        
                       first, last))
      write.table(temp, file = file.out, append = TRUE,         # Salva o documento. append = T é para ir incrementando o documento
                  row.names = FALSE, col.names = FALSE)
    } else {
      break
    }
  }
  cat("TOTAL: Lidos:", nreg, "registros.\n")
  close(con.inp)
}

# ?read.fwf

read.fixed.width(file.inp = "DadosNet/PNAD_2008/2008/dados/PES2008.TXT", file.out = "DadosNet/PNAD_2008/PNAD_2008.txt", 
                 first = c(5, 1233, 757, 769, 755) , last = c(6, 1233, 768, 770, 756))

DadosPNAD2008 <- read.table(file = "DadosNet/PNAD_2008/PNAD_2008.txt", sep = " ")
names(DadosPNAD2008) <- c("CodUF", "Plano", "Renda", "RendaCat", "NumPessoas")

DadosPNAD2008$"SiglaUF" <- gsubDM(DadosPNAD2008$"CodUF",
                                  pattern = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 
                                              25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42,
                                              43, 50, 51, 52, 53),
                                  replacement = c("RO", "AC", "AC", "RR", "PA", "AP", 
                                                  "TO", "MA", "PI", "CE", "RN", "PB",
                                                  "PE", "AL", "SE", "BA", "MG", "ES", 
                                                  "RJ", "SP", "PR", "SC", "RS", "MS", 
                                                  "MT", "GO", "DF"))

#DadosPNAD2008$"Plano" <- gsub(pattern = 1, replacement = 1, x = DadosPNAD2008$"Plano")
DadosPNAD2008$"Plano" <- gsub(pattern = 3, replacement = 1, x = DadosPNAD2008$"Plano")
DadosPNAD2008$"Plano" <- gsub(pattern = 5, replacement = 0, x = DadosPNAD2008$"Plano")
DadosPNAD2008$"Renda" <- gsub(pattern = 999999999999, replacement = NA, x = DadosPNAD2008$"Renda")
DadosPNAD2008$"Plano" <- as.numeric(DadosPNAD2008$"Plano")
DadosPNAD2008$"Renda" <- as.numeric(DadosPNAD2008$"Renda")
DadosPNAD2008 <- DadosPNAD2008[which(is.na(DadosPNAD2008$"Renda") == F),]
DadosPNAD2008$"RendaDomicilio" <- DadosPNAD2008$"Renda"*DadosPNAD2008$"NumPessoas"

write.csv(DadosPNAD2008, file = "DadosNet/PNAD_2008/DadosPNAD2008.csv", row.names = F)
unlink("DadosNet/PNAD_2008/PNAD_2008.txt")

#-----------------------------------------------------------------------------------------------------------------------------

# Dados: População IBGE no censo 2010
# Endereço: ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios

setwd("DadosNet/PopIBGE/ArquivosSeparados/")
arqs <- list.files()

DadosIBGE = rbindlist(lapply(X = arqs, FUN = function(x) read.table(file = x,  
                                                                    sep = ";", 
                                                                    dec = ",", 
                                                                    header = FALSE, 
                                                                    skip = 1,                           # Coluna com os nomes bugada
                                                                    fileEncoding = "ISO-8859-15", 
                                                                    na.strings = c("", " ","NA", NA))))

ColumnNames <- read.table(file = arqs[1], sep = ";", dec = ",", 
                          nrows = 1, colClasses = rep("character", 34),
                          header = FALSE, fileEncoding = "ISO-8859-15", 
                          na.strings = c("", " ","NA", NA))

ColumnNames <- gsub(x = ColumnNames, pattern = " ", replacement = "")
names(DadosIBGE) <- ColumnNames[-length(ColumnNames)]
ColunasInteresse <- which(names(DadosIBGE) %in% c("Cod_UF", 
                                                  "Nome_da_UF", 
                                                  "Cod_municipio", 
                                                  "Nome_do_municipio", 
                                                  "Cod_bairro", 
                                                  "Nome_do_bairro", 
                                                  "V002", 
                                                  "V011"))
DadosIBGE <- DadosIBGE[, .(Cod_UF, Nome_da_UF, Cod_municipio, Nome_do_municipio, Cod_bairro, Nome_do_bairro, V002, V011)]

CodSiglaNomeUF <- read.csv2("../CodigoSiglaNomeEstado.csv", encoding = "Latin-1")
CodSiglaNomeUF <- CodSiglaNomeUF[, - which(names(CodSiglaNomeUF) == "ESTADO")]
CodSiglaNomeUF <- data.table(CodSiglaNomeUF)

DadosIBGE <- merge(DadosIBGE, CodSiglaNomeUF, by.x = "Cod_UF", by.y = "CODIGO")
names(DadosIBGE) <- c("CO_UF_IBGE", 
                      "UF", 
                      "CO_MUNICIPIO_IBGE", 
                      "NO_MUNICIPIO_IBGE", 
                      "CO_BAIRRO", 
                      "BAIRRO", 
                      "POP", 
                      "RendaMedia",
                      "SIGLA_UF")  

require(dplyr)
DadosIBGE <- DadosIBGE %>%
  group_by(SIGLA_UF,CO_MUNICIPIO_IBGE,BAIRRO) %>%
  mutate(RM = POP*RendaMedia/sum(POP, na.rm = T))

DadosIBGE <- DadosIBGE %>%
  group_by(SIGLA_UF,CO_MUNICIPIO_IBGE,BAIRRO) %>%
  summarize(RENDA_MEDIA = sum(RM, na.rm = T), POP = sum(POP, na.rm = T))

DadosIBGE <- subset(DadosIBGE, RENDA_MEDIA > 0 & !is.na(POP), na.rm = T)

write.csv(DadosIBGE, file = "../../PopIBGE/DadosIBGE2010.csv", row.names = F)

#-----------------------------------------------------------------------------------------------------------------------------

# Dados: População com plano de saúde ANS
# Endereço: http://www.ans.gov.br/csv/A09172610_22_1_2.csv

DadosANS_2008 <- data.table::fread(input = "http://www.ans.gov.br/csv/A09172610_22_1_2.csv", 
                                   skip = 3, header = T, encoding = "Latin-1", colClasses = c("character", "numeric", "numeric"))

names(DadosANS_2008) <- c("Municipio", "Assitencia_medica", "Plano_odontologico")

DadosANS_2008[, Total := Assitencia_medica + Plano_odontologico]

write.table(x = DadosANS_2008, file = "Dados/PLANOS_ANS_2008-2.csv", sep = ";", dec = ".")

