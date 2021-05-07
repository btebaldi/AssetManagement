# Carrega bibliotecas
library(ggplot2)
library(dplyr)
library(tibble)
library(readxl)


# Limpa variaveis
rm(list=ls())

# importa Dados
Data <- read_excel("./Listas/Monitoria1.xlsx", sheet = "Q6", 
                   range = "A2:C7")
risk_free = read_excel("./Listas/Monitoria1.xlsx", sheet = "Q6", 
                         range = "B9:B9", col_names = F)

# ----- Trabalha os Dados -----
# Constroi a matriz de variancia e covariancia
omega = matrix(NA, nrow = 2, ncol = 2)
omega[1,1] = as.numeric(Data[2,2])^2
omega[2,2] = as.numeric(Data[2,3])^2
omega[1,2] = as.numeric(Data[3,2])*as.numeric(Data[2,2])*as.numeric(Data[2,3])
omega[2,1] = omega[1,2]
  
# Constroi o vetor de retorno
ret = matrix(NA, nrow = 2, ncol = 1)
ret[1,1] = as.numeric(Data[1,2])
ret[2,1] = as.numeric(Data[1,3])
rownames(ret) = c("US", "JP")

# determina quantos pesos vamos calcular
N = 100

# constroi uma tabela com uma coluna de peso e retorno
dados = tibble::tibble(w = seq(from = 0, to = 1, length.out = N),
                        ret_Portifolio = NA,
                        sigma_Portifolio = NA)

# Para cada peso vamos calcular o retorno e a variancia
for (i in 1:N)
{
  # vetor de pessos, inicializado com alor NA
  weight_vector = matrix(NA, nrow = 2, ncol = 1)
  
  # Inicializo o vetor de peso com os pesos da tabela
  weight_vector[1,1] = dados$w[i]
  weight_vector[2,1] = 1-dados$w[i]
  
  # Calcula o retorno
  dados$ret_Portifolio[i] = t(weight_vector) %*% ret
  
  # Calcula o sigma
  dados$sigma_Portifolio[i] = (t(weight_vector) %*% as.matrix(omega) %*% weight_vector)^0.5
  
}

# realiza a impressao
ggplot(dados, aes(x=sigma_Portifolio, y=ret_Portifolio)) +
  geom_point(size=0.5) +
  labs(x="Sigma",
       y="Retorno",
       title = "Fronteira eficiente")
