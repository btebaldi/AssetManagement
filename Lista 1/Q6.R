# --- Carrega livrarias ----
library(ggplot2)
library(dplyr)
library(readxl)

# Limpa variaveis
rm(list=ls())

# ----- importa Dados -----
Data <- read_excel("./Lista 1/Monitoria1.xlsx", sheet = "Q6", 
                   range = "A2:C7")
risk_free = read_excel("./Lista 1/Monitoria1.xlsx", sheet = "Q6", 
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

# constroi o vetor de 1's
# one = as.matrix(rep(1, length(ret)))
one = rep(1, length(ret))


# ----- Fronteira Eficiente -----

# Constroi matrizes A B C 
A = as.numeric(t(one) %*% solve(omega) %*% one)
B = as.numeric(t(one) %*% solve(omega) %*% ret)
C = as.numeric(t(ret) %*% solve(omega) %*% ret)

# Determina os pontos de retorno que serao calculados
min = 0.10
max = 0.115

# inicializa o vetor de respostas
N = 100
result = matrix(NA, nrow = N, ncol = 2)
colnames(result) = c("ret", "sigma")

result[,1] = seq(from = min, to = max, length.out = N)

for (i in 1:N)
{
  mu = result[i,"ret"]
  
  # Constroi Lambda1 e lambda2
  L1 = as.numeric((C-B*mu)/(A*C-B^2))
  L2 = as.numeric((A*mu-B)/(A*C-B^2))
  
  # Calcula os pesos
  w = L1* solve(omega) %*% one + L2 * solve(omega) %*% ret
  
  # Calcula o sigma
  sigma = (t(w) %*% as.matrix(omega) %*% w)^0.5
  
  result[i,"sigma"] = sigma
}

# transforma em data frame para imprimir
data_fronteira = as.data.frame(result)


# ----- Impressao do grafico -----
d = arrange(data_fronteira, ret)

# realiza a impressao
ggplot(d, aes(x=sigma, y=ret)) + geom_point(size=0.5) 

