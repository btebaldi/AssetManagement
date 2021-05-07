# Limpa variaveis
rm(list=ls())

# Carrega dados do exercicio
load("./ExercicioSite/ExercicioSite1.RData")

# define um vetor de 1's
one = as.matrix(rep(1, length(ret)))

gamma = as.numeric(t(one) %*% solve(omega) %*% ret)

lambda = solve((t(one) %*% solve(omega) %*% one)) %*% (t(one) %*% solve(omega) %*% ret - gamma)

weights =  (solve(omega) %*% ret )* gamma^(-1) -  as.numeric(lambda)*gamma^(-1) * solve(omega) %*% one

cat(sprintf('------------------------------------\n'))
cat(sprintf('Análise da CAL\n'))
cat(sprintf('------------------------------------\n'))
cat(sprintf('weights:\n'))
for(i in 1:length(ret)){
  cat(sprintf('Ativo %d: %1.3f\n', i, weights[i]))
}
cat(sprintf('------------------------------------\n'))
cat(sprintf('Retorno carteira: %2.2f\n', t(ret)%*%weights))
cat(sprintf('Variancia carteira: %2.2f\n', t(weights)%*%omega%*%weights))
cat(sprintf('====================================\n'))
