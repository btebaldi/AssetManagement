rm(list=ls())

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

NSS = function(x, maturity)
{
  t=maturity
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

NSS.SSE = function(x, data)
{
  y <- NSS(x, data$Maturity)
  
  error <- as.vector(y - data$Yeild)
  
  SSE <- t(error) %*% error
  
  return(SSE^2)
}


Pasta1 <- read_excel("C:/Users/Tebaldi/Downloads/Pasta1.xlsx", 
                     range = "A1:E12", sheet = 2)

for (i in 2:ncol(Pasta1))
{
  print(sprintf("Round %d", i))
  x=c(2, -1, 1, 1, 1, 1)
  
  coluna = colnames(Pasta1)[i]
  
  data = Pasta1 %>% select(Maturity, coluna)
  colnames(data) = c("Maturity", "Yeild")

  mdl.unrestrict <- optim(x, NSS.SSE, method="L-BFGS-B", lower = c(0,-Inf,-Inf,-Inf,0,0), upper = c(Inf, Inf, Inf, Inf, Inf, Inf), data=data)
  mdl.restrict <- constrOptim(x, NSS.SSE, grad=NULL, ui = rbind(c(1,0,0,0,0,0), c(1,1,0,0,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1)), ci=c(0, 0, 0, 0), data=data)
  
  Pasta1[,paste(coluna, "prevRes", sep = ".")] = NSS(mdl.unrestrict$par, data$Maturity)
  Pasta1[,paste(coluna, "prevUnres", sep = ".")] = NSS(mdl.restrict$par, data$Maturity)
  
}

ggplot(Pasta1) +
  geom_point(aes(x=Maturity, y=D2016_12, colour="2016")) +
  geom_line(aes(x=Maturity, y=D2016_12.prevUnres, colour="2016")) +
  geom_point(aes(x=Maturity, y=D2017_12, colour="2017")) +
  geom_line(aes(x=Maturity, y=D2017_12.prevUnres, colour="2017")) +
  geom_point(aes(x=Maturity, y=D2018_12, colour="2018")) +
  geom_line(aes(x=Maturity, y=D2018_12.prevUnres, colour="2018")) +
  geom_point(aes(x=Maturity, y=D2019_08, colour="2019")) +
  geom_line(aes(x=Maturity, y=D2019_08.prevUnres, colour="2019")) +
  theme(legend.position = c(0.5, 0.95),
        legend.direction = "horizontal")


ggplot(Pasta1) +
  geom_point(aes(x=Maturity, y=D2016_12, colour="2016")) +
  geom_line(aes(x=Maturity, y=D2016_12.prevRes, colour="2016")) +
  geom_point(aes(x=Maturity, y=D2017_12, colour="2017")) +
  geom_line(aes(x=Maturity, y=D2017_12.prevRes, colour="2017")) +
  geom_point(aes(x=Maturity, y=D2018_12, colour="2018")) +
  geom_line(aes(x=Maturity, y=D2018_12.prevRes, colour="2018")) +
  geom_point(aes(x=Maturity, y=D2019_08, colour="2019")) +
  geom_line(aes(x=Maturity, y=D2019_08.prevRes, colour="2019")) +
  theme(legend.position = c(0.5, 0.95),
        legend.direction = "horizontal")
