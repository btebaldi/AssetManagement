# Carrega bibliotecas
library(ggplot2)
library(dplyr)
library(tibble)

# Limpa variaveis
rm(list=ls())

# Determina a funcao utilidade
U = function(gamma, r, s){
  ret = r - gamma/2*s
}

# Valores informados do problemas
  
  # Valores do eixo x (gamma) que vamos utilizar para tracar o grafico
  x = 1:50
  
  # Ativo de risco
  r = 0.1
  
  # Ativo livre de risco
  rf = 0.04
  
  # Sigma
  s = 0.0036

# constroi uma tabela que deve Contem os valores
dados = tibble(gamma = x, Utilidade = U(x, r, s))

# Busca os valores de 10 30 e 50 na tabela
dados.selecao = dplyr::filter(dados, gamma %in% c(10,30,50)) 

print(dados.selecao)

# Imprime os graficos
ggplot(dados, aes(x=x)) + 
  geom_line(aes(y=Utilidade, colour = "Risk Asset")) +
  geom_line(aes(y=rf, colour = "Risk Free")) +
  geom_point(data = dados.selecao, aes(x=gamma,  y=Utilidade))

             