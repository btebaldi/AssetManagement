# Limpeza de variaveis
rm(list = ls())

# Bibliotecas utilizadas
library(readxl)
library(dplyr)
library(ggplot2)

# Funcao para calculo de covariancia
CovMatrix = function(M)
{
  n <- nrow(M) #number of subjects
  
  # Vector of ones
  O = matrix(1, n, 1)
  
  # Centering in means matrix (A)
  A = diag(n) - 1/n * O %*% t(O)
  
  # Centered matrix
  D = A %*% M  
  
  #creates the covariance matrix
  C <- (n-1)^-1 * t(D) %*% D
  
  return(C)
}


# Carrega banco de dados
Database <- readxl::read_excel("Database.xlsx", col_types = c("date", 
                                                      "text", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric"))
colnames(Database) = c("Date", "Ticker", "Open", "High", "Low", "Close", "Dividend", "AdjClose", "Volume")

# Ajusta coluna de fechamento
Database$AdjClose = Database$Close + Database$Dividend

# Passa o Ln nos precos de fechamento e fechamento ajustado
Database$Ln_AdjClose = log(Database$AdjClose)
Database$Ln_Close = log(Database$Close)

# calcula os retornos de cada um dos ativos
Db_Nord = Database %>% dplyr::filter(Ticker == "JWN") %>% dplyr::arrange(Date) %>% dplyr::mutate(ret = Ln_AdjClose - lag(Ln_Close)) %>% dplyr::filter(Date > "1995-02-01")
Db_Msft = Database %>% dplyr::filter(Ticker == "MSFT") %>% dplyr::arrange(Date) %>% dplyr::mutate(ret = Ln_AdjClose - lag(Ln_Close)) %>% dplyr::filter(Date > "1995-02-01")
Db_Sbux = Database %>% dplyr::filter(Ticker == "SBUX") %>% dplyr::arrange(Date) %>% dplyr::mutate(ret = Ln_AdjClose - lag(Ln_Close)) %>% dplyr::filter(Date > "1995-02-01")

# retira variaveis que nao serao mais utilizadas
rm(list = c("Database"))


ReturnData = cbind(Db_Msft$ret, Db_Nord$ret, Db_Sbux$ret)
colnames(ReturnData) = c("MSFT", "NORD", "SBUX")

CovMatrix = CovMatrix(ReturnData)

ggplot2::ggplot(tibble(M=colMeans(ReturnData), V=diag(CovMatrix^0.5))) + geom_point(aes(M,V))



