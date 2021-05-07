# Limpa variaveis
rm(list=ls())

# Carrega dados do exercicio
load("./ExercicioSite/ExercicioSite1.RData")

# define um vetor de 1's
one = as.matrix(rep(1, length(ret)))

# define o retorno esperado
mu = 14

A = as.numeric(t(one) %*% solve(omega) %*% one)
B = as.numeric(t(one) %*% solve(omega) %*% ret)
C = as.numeric(t(ret) %*% solve(omega) %*% ret)

lambda_1 = (C-B*mu)/(A*C - B*B)
lambda_2 = (A*mu-B)/(A*C - B*B)

weights =  lambda_1 * solve(omega) %*% one + lambda_2 * solve(omega) %*% ret

cat(sprintf('------------------------------------\n'))
cat(sprintf('Análise de Minima Variancia com retorno especifico\n'))
cat(sprintf('------------------------------------\n'))
cat(sprintf('weights:\n'))
for(i in 1:length(ret)){
  cat(sprintf('Ativo %d: %1.3f\n', i, weights[i]))
}
cat(sprintf('------------------------------------\n'))
cat(sprintf('Retorno carteira: %2.2f\n', t(ret)%*%weights))
cat(sprintf('Variancia carteira: %2.2f\n', t(weights)%*%omega%*%weights))
cat(sprintf('====================================\n'))

