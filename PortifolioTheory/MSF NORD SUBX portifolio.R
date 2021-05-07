
# Setup -------------------------------------------------------------------
# Limpeza de variaveis
rm(list = ls())

# Bibliotecas utilizadas
library(readxl)
library(dplyr)
library(ggplot2)


# User defined functions --------------------------------------------------
# 
# # Funcao para calculo de covariancia
# CovMatrix = function(M)
# {
#   n <- nrow(M) #number of subjects
#   
#   # Vector of ones
#   O = matrix(1, n, 1)
#   
#   # Centering in means matrix (A)
#   A = diag(n) - 1/n * O %*% t(O)
#   
#   # Centered matrix
#   D = A %*% M  
#   
#   #creates the covariance matrix
#   C <- (n-1)^-1 * t(D) %*% D
#   
#   return(C)
# }


D.ln <- function(x) {
  ret <- log(x) - lag(log(x))
  return(ret)
  
}

r.ln <- function(x) {
  ret <- exp(D.ln(x)) -1
  return(ret)
  
}

# Dataload ----------------------------------------------------------------


# Carrega banco de dados
Database <- readxl::read_excel("./databases/MSFT_NORD_SBUX_Database.xlsx", col_types = c("date", 
                                                      "text", "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric", "numeric"))

# Data preparation --------------------------------------------------------

# calcula os retornos de cada um dos ativos

Db_Msft <- Database %>% 
  dplyr::filter(Ticker == "MSFT") %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(ret = D.ln(AdjClose)) %>%
  dplyr::filter(Date > "1995-01-01")

Db_Nord <- Database %>% 
  dplyr::filter(Ticker == "JWN") %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(ret = D.ln(AdjClose)) %>%
  dplyr::filter(Date > "1995-01-01")

Db_Sbux <- Database %>% 
  dplyr::filter(Ticker == "SBUX") %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(ret = D.ln(AdjClose)) %>%
  dplyr::filter(Date > "1995-01-01")


# Portfolio Characteristics Using Matrix Notation -------------------------

# Busca os retornos
ReturnData = cbind(Db_Msft$ret, Db_Nord$ret, Db_Sbux$ret)
colnames(ReturnData) = c("MSFT", "NORD", "SBUX")

# Matrix de variancia e covariancia
sigma.mat = cov(ReturnData)

# Matrix dos retornos
mu.vec = apply(ReturnData, 2, mean, na.rm=TRUE)

mean_var.tbl <- tibble(mean=mu.vec, sigma=diag(sigma.mat^0.5), names=colnames(ReturnData))
mean_var.tbl %>% 
ggplot() +
  geom_point(aes(x=sigma, y = mean)) + 
  geom_text(aes(x=sigma, y=mean-0.001, label = names)) +
  labs(x="variance",
       y="return")


# Example 3 Portfolio computations in R -----------------------------------
asset.names <- colnames(ReturnData)

x.vec = rep(1,3)/3
names(x.vec) = asset.names
mu.p.x = crossprod(x.vec,mu.vec)
sig2.p.x = t(x.vec)%*%sigma.mat%*%x.vec
sig.p.x = sqrt(sig2.p.x)

mean_var.tbl %>% 
  ggplot() +
  geom_point(aes(x=sigma, y = mean)) + 
  geom_text(aes(x=sigma, y=mean-0.001, label = names)) +
  geom_point(aes(x=sig.p.x, y=mu.p.x), colour="red") +
  labs(x="variance",
       y="return")

# Next, consider another portfolio with weight vector y = c(0.8, 0.4, -0.2) and
# return R_{py} The covariance between R_{py} and R_{px} is
y.vec = c(0.8, 0.4, -0.2)
names(x.vec) = asset.names
sig.xy = t(x.vec)%*%sigma.mat%*%y.vec



# Finding the Global Minimum Variance Portfolio ---------------------------

top.mat = cbind(2*sigma.mat, rep(1, 3))
bot.vec = c(rep(1, 3), 0)
Am.mat = rbind(top.mat, bot.vec)
b.vec = c(rep(0, 3), 1)
z.m.mat = solve(Am.mat)%*%b.vec
m.vec = z.m.mat[1:3,1]
m.vec


mu.gmin = as.numeric(crossprod(m.vec, mu.vec))
mu.gmin


sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)
sig.gmin = sqrt(sig2.gmin)
sig2.gmin
sig.gmin


mean_var.tbl %>% 
  ggplot() +
  geom_point(aes(x=sigma, y = mean)) + 
  geom_text(aes(x=sigma, y=mean-0.001, label = names)) +
  geom_point(aes(x=sig.p.x, y=mu.p.x), colour="red") +
  geom_point(aes(x=sig.gmin, y=mu.gmin), colour="blue") +
  labs(x="variance",
       y="return")


one.vec = rep(1, 3)
sigma.inv.mat = solve(sigma.mat)
top.mat = sigma.inv.mat%*%one.vec
bot.val = as.numeric((t(one.vec)%*%sigma.inv.mat%*%one.vec))
m.mat = top.mat/bot.val
m.mat[,1]


# Example 7 Efficient portfolio with the same expected return as MSFT --------

top.mat = cbind(2*sigma.mat, mu.vec, rep(1, 3))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, 3), 0, 0)
A.mat = rbind(top.mat, mid.vec, bot.vec)
bmsft.vec = c(rep(0, 3), mu.vec["MSFT"], 1)

z.mat = solve(A.mat)%*%bmsft.vec
x.vec = z.mat[1:3,]
x.vec

mu.MSFT = as.numeric(crossprod(x.vec, mu.vec))
mu.MSFT


sig2.MSFT = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)
sig.MSFT = sqrt(sig2.MSFT)


mean_var.tbl %>% 
  ggplot() +
  geom_point(aes(x=sigma, y = mean)) + 
  geom_text(aes(x=sigma, y=mean-0.001, label = names)) +
  geom_point(aes(x=sig.MSFT, y=mu.MSFT), colour="red") +
  geom_point(aes(x=sig.gmin, y=mu.gmin), colour="blue") +
  geom_hline(yintercept =  mu.vec["MSFT"], linetype="dashed") +
  labs(x="variance",
       y="return")


# Creating an arbitrary frontier portfolio --------------------------------


# ESSA PARTE PARECE ESTAR ERRADA (PRECISA CORRIGIR)

m <- m.mat[,1]
mu.px <- mu.vec["MSFT"]
sig2.px <- sigma.mat[1,1]

a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, 3)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m)%*%sigma.mat%*%x.vec
i=1
for (i in 1:n.a) {
   z.mat[i, ] = a[i]*m + (1-a[i])*x.vec
   mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
   sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}

tbl <- tibble(s=sqrt(sig2.z), mu.z)

mean_var.tbl %>% 
  ggplot() +
  geom_point(aes(x=sigma, y = mean)) + 
  geom_text(aes(x=sigma, y=mean-0.001, label = names)) +
  geom_point(aes(x=sig.MSFT, y=mu.MSFT), colour="red") +
  geom_point(aes(x=sig.gmin, y=mu.gmin), colour="blue") +
  geom_hline(yintercept =  mu.vec["MSFT"], linetype="dashed") +
  geom_path(aes(x=s, y= mu.z), data = tbl) +
  labs(x="variance",
       y="return")


