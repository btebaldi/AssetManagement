# Limpa variaveis
rm(list = ls())

# Carrega bibliotecas
library(readxl)
library(lubridate)
library(ggplot2)

# Carrega dados Monitoria2.xlsx
data <- read_excel("./Listas/Monitoria2.xlsx", range = "A1:C30", sheet = "Q6")
colnames(data) = c("DatePrice", "r_year", "DateMaturity")

# Determina quanto tempo (em dias) para o maturity
data$Maturity_days = as.numeric(data$DateMaturity - data$DatePrice)

# Determina quanto tempo (em anos) para o maturity (assumindo ano 252)
data$Maturity_years = data$Maturity_days / 252
head(data)

# Plota os pontos
ggplot(data, aes(x=Maturity_years, y=r_year)) + geom_point()

# Determina a funcao de Nelson-Svensson-Siegel
NSS = function(x, Data)
{
  t=Data$Maturity_years
  beta0 = x[1]
  beta1 = x[2]
  beta2 = x[3]
  beta3 = x[4]
  lambda1 = x[5]
  lambda2 = x[6]
  
  y <- beta0 +
    beta1*(1-exp(-t/lambda1))/(t/lambda1) +
    beta2*((1-exp(-t/lambda1))/(t/lambda1) -exp(-t/lambda1)) +
    beta3*((1-exp(-t/lambda2))/(t/lambda2) -exp(-t/lambda2))

  return(y)
}

# Determina a funcao de erro quadratico
NSS.SSE = function(x, Data)
{
  y <- NSS(x, Data)
  
  error <- y - Data$r_year 
  
  SSE <- t(error) %*% error
  
  return(SSE)
}

# Chute inicial para os coeficientes
x=c(0.09, 0.01, 0.01, 0.01, 1, 1)

# A titulo de curiosidade vamos verificar o quao longe estamos dos valores
data$prev0 = NSS(x, data)

ggplot(data, aes(x=Maturity_years, y=r_year)) +
  geom_point(aes(colour="Original")) +
  geom_line(aes(y=prev0, colour="Guess")) +
  scale_colour_manual(values = c("black", "red")) + 
  labs(x = "Maturity [Years]", 
       y="Ret.")

# Calcula o modelo irrestrito
mdl.unrestrict <- optim(x, NSS.SSE, Data=data, method="L-BFGS-B", lower = c(0,-Inf,-Inf,-Inf,0,0), upper = c(Inf, Inf, Inf, Inf, Inf, Inf) )
data$prev1 = NSS(mdl.unrestrict$par, data)

# Calcula o modelo restrito
mdl.restrict <- constrOptim(x, NSS.SSE, grad=NULL, ui = rbind(c(1,0,0,0,0,0), c(1,1,0,0,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1)), ci=c(0, 0, 0, 0), Data=data)
data$prev2 = NSS(mdl.restrict$par, data)


ggplot(data, aes(x=Maturity_years, y=r_year)) +
  geom_point(aes(colour="Original")) +
  geom_line(aes(y=prev1, colour="Unrestricted")) +
  geom_line(aes(y=prev2, colour="Restricted")) +
  scale_colour_manual(values = c("red", "blue", "black")) +
  theme(legend.position = c(0.4, 0.95),
        legend.direction = "horizontal")

