library(ggplot2)

# Limpa variaveis
rm(list=ls())

# Carrega dados do exercicio
load("./ExercicioSite/ExercicioSite1.RData")

# define um vetor de 1's
one = as.matrix(rep(1, length(ret)))

gamma = 1

lambda = solve((t(one) %*% solve(omega) %*% one)) %*% (
  t(ret) %*% solve(omega) %*% one - gamma)

weights =  (solve(omega) %*% ret )* gamma^(-1) +
          -as.numeric(lambda)*gamma^(-1) * solve(omega) %*% one

cat(sprintf('weights:\n'))
for(i in 1:length(ret)){
  cat(sprintf('%1.3f\n', weights[i]))
}

plot()
