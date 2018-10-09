library(ggplot2)

# Limpa variaveis
rm(list=ls())

# Vamos geral dois retornos
ret = c(10.33, 11.12)

var = matrix(NA, ncol=2, nrow=2)

var[1,] = c(245.862, 120.148)
var[2,] = c(120.148, 466.560)


sampleSize = 10000
weights = matrix(NA, ncol = 2, nrow=sampleSize)
weights[,1] = rnorm(sampleSize, mean = 0, sd = 1)
weights[,2] = rnorm(sampleSize, mean = 0, sd = 1)

n=100
weights[1:n,2] = 1-weights[1:n,1]

y = matrix(NA, ncol = 1, nrow=sampleSize)
x = matrix(NA, ncol = 1, nrow=sampleSize)

for (i in 1:sampleSize){
  y[i,1] = weights[i,]%*%ret;
  s = as.matrix(weights[i,])
  x[i,1] = weights[i,]%*%var%*%s
}

# x = x^0.5;
RS = rowSums(weights)
# summary(RS)
# plot(x[RS==1],y[RS==1],type = 'p', col="red")
# points(x[RS>1],y[RS>1],col="green")
# points(x[RS<1],y[RS<1],col="blue")

data = data.frame("Sdv"=x^0.5,"Ret"=y, "Type" = NA)
data[RS==1,3]= "1"
data[RS>1,3]= "+1"
data[RS<1,3]= "-1"
# head(data)
ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point()
ggplot(data, aes(x=Sdv, y=Ret, color=Type)) + geom_point() + ylim(c(5,15)) + xlim(c(0,40))
