# Limpa variaveis
rm(list = ls())

#  carrega livrarias
library(readxl)
AssetNames=c('US Bonds', 'Int Bonds', 'US Large Growth', 'US Large Value', 
             'US Small Growth', 'US Small Value', 'Int Dev Equity',
             'Int Emerg Equity')

ret <- read_excel("Data.xlsx", sheet = "Planilha2", 
                   col_names = FALSE, range = "B1:E8" )
rownames(ret) = AssetNames

omega <- read_excel("Data.xlsx", sheet = "Sigma", 
                   col_names = FALSE, range="B1:I8")
colnames(omega) = AssetNames

# Weights
W_Sigma = matrix(data = NA, nrow = length(AssetClass), ncol=length(AssetClass))

W_Sigma[,1] = c(0.001005,2,3,4,5,6,7,8)

df <- data.frame(AssetClass, ret_Hist, ret_CAPM_GSMI, ret_CAPM_Port, eqVec)
