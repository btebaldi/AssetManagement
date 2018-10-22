# --- Carrega livrarias ----
library(ggplot2)
library(dplyr)
library(readxl)

# Limpa variaveis
rm(list=ls())

# ----- importa Dados -----
Data <- read_excel("./Lista 1/Monitoria1.xlsx", sheet = "Q7", 
                   range = "B2:C7")
Correlation = read_excel("./Lista 1/Monitoria1.xlsx", sheet = "Q7", 
                         range = "F2:J7")

# ----- Trabalha os Dados -----
# Constroi a matriz de variancia e covariancia
omega = as.matrix(Data["Std"]) %*% t(as.matrix(Data["Std"])) * Correlation

# Constroi o vetor de retorno
ret = as.matrix(Data["Mean"])

# constroi o vetor de 1's
one = rep(1, length(ret))


# ----- Fronteira Eficiente -----

# Constroi matrizes A B C 
A = as.numeric(t(one) %*% solve(omega) %*% one)
B = as.numeric(t(one) %*% solve(omega) %*% ret)
C = as.numeric(t(ret) %*% solve(omega) %*% ret)

# Determina os pontos de retorno que serao calculados
min = 10
max = 12.5

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

# ----- Minima Variancia -----

w_2 = as.numeric(solve(t(one) %*% solve(omega) %*% one)) * solve(omega) %*% one

ret_min = t(ret) %*% w_2
var_min = t(w_2) %*% as.matrix(omega) %*% w_2
std_min = var_min^0.5

data_minVar = data.frame("ret" = ret_min, "std" = std_min)


# ----- CAL LINE -----

w_cal = as.numeric(solve(t(one) %*% solve(omega) %*% ret)) * solve(omega) %*% ret
ret_CAL = t(ret) %*% w_cal
var_CAL = t(w_cal) %*% as.matrix(omega) %*% w_cal
std_CAL = var_CAL^0.5

data_CAL = data.frame("ret" = ret_CAL, "std" = std_CAL)
colnames(data_CAL) = c("ret", "std")
slope_CAL = ret_CAL/std_CAL

# ----- Retorno especifico -----
mu = 12

A = as.numeric(t(one) %*% solve(omega) %*% one)
B = as.numeric(t(one) %*% solve(omega) %*% ret)
C = as.numeric(t(ret) %*% solve(omega) %*% ret)

lambda_1 = (C-B*mu)/(A*C - B*B)
lambda_2 = (A*mu-B)/(A*C - B*B)

w_retEspec =  lambda_1 * solve(omega) %*% one + lambda_2 * solve(omega) %*% ret

ret_retEspec = t(ret) %*% w_retEspec
var_retEspec = t(w_retEspec) %*% as.matrix(omega) %*% w_retEspec
std_retEspec = var_retEspec^0.5

data_retEspec = data.frame("ret" = ret_retEspec, "std" = std_retEspec)
colnames(data_retEspec) = c("ret", "std")


# ----- Impressao do grafico -----
d = arrange(data_fronteira, ret)

# realiza a impressao
ggplot(d, aes(x=sigma, y=ret)) + geom_point(size=0.5) +
  geom_point(data = data_minVar, aes(x=std, y=ret), shape = 16, colour="red", size = 2) +
  geom_abline(intercept = 0, slope = as.numeric(slope_CAL)) +
  geom_point(data = data_CAL, aes(x=std, y=ret), shape = 16, colour="blue", size = 2) +
  geom_point(data = data_retEspec, aes(x=std, y=ret), shape = 16, colour="green", size = 2)  
# ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point() + ylim(c(5,15)) + xlim(c(0,40))


