library(ggplot2)
library(tibble)

# Limpa variaveis
rm(list=ls())

# gera retorno para dois ativos com risco
R_p = c(10.33, 11.12)

# Gera o retorno de Renda Fixa
rf = 10

# Calcula o retorno líquido
R_net = R_p - rf

# Determina a matrix de Variancia e covariancia para os ativos de risco
var = matrix(NA, ncol=2, nrow=2)
var[1,] = c(245.862, 120.148)
var[2,] = c(120.148, 469.590)

sampleSize = 1e4
weights = matrix(NA, ncol = 2, nrow=sampleSize)
weights[,1] = rnorm(sampleSize, mean = 0, sd = 1)
weights[,2] = rnorm(sampleSize, mean = 0, sd = 1)

n=1e3
weights[1:n,2] = 1-weights[1:n,1]
weights[n+1:n+n,2] = -1-weights[n+1:n+n,1]


y = matrix(NA, ncol = 1, nrow=sampleSize)
x = matrix(NA, ncol = 1, nrow=sampleSize)

for (i in 1:sampleSize){
  w_row = as.matrix(weights[i,])

  y[i,1] = t(w_row) %*% R_net;
  x[i,1] = t(w_row) %*% var %*% w_row
}

# Faz a soma dos pesos para cada cenario (linha)
RS = rowSums(weights)


# Classifica os pesos
data = data.frame("Sdv"=x^0.5,"Ret"=y, "Type" = NA)
data = as.tibble(data)
data[RS>1,3]= "[1, +]"
data[RS==1,3]= "=1"
data[RS<1,3]= "[0, 1]"
data[RS<0,3]= "[-1, 0)"
data[RS==-1,3]= "=-1"
data[RS< -1,3]= "[-, -1]"

rm(list = c("RS", "w_row"))

s1 = (data[,"Type"] == "[1, +]")
s2 = (data[,"Type"] == "=1")
s3 = (data[,"Type"] == "[0, 1]")
s4 = (data[,"Type"] == "[-1, 0)")
s5 = (data[,"Type"] == "=-1")
s6 = (data[,"Type"] == "[-, -1]")

# head(data)
print( ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point() )
# ggplot(data[data[,"Type"] != "+1",], aes(x=Sdv, y=Ret, color=Type)) + geom_point(size=0.5) + ylim(c(-2,2))
print(ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point( size=0.5) + ylim(c(-2,2)) + xlim(c(0,40)) )

ggplot(data[s1|s6|s2|s5,], aes(x=Sdv, y=Ret, color=Type)) + geom_point(size=0.5) + ylim(c(-2,2))  + xlim(c(0,40))

ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point() + ylim(c(8,12)) + xlim(c(0,40))

