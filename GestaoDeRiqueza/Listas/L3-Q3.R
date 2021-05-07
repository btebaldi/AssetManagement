# Carrega bibliotecas
library(readxl)
library(lmtest)

# Limpa variaveis
rm(list=ls())

# importa Dados e acerta nome das colunas
Dados <- read_excel("./Listas/Monitoria3.xlsx", sheet = "Q3")
colnames(Dados) = c("DATA", "SPY_ETF", "SPYG_ETF", "SPYV_ETF", "FLGEX", "LCEAX", "TRMCX", "VEIPX")

head(Dados)


# Faz analise econometrica dos modelos
mdl.FLGEX = lm(FLGEX ~ SPY_ETF + SPYG_ETF + SPYV_ETF, data = Dados)
summary(mdl.FLGEX)

mdl.LCEAX = lm(LCEAX ~ SPY_ETF + SPYG_ETF + SPYV_ETF, data = Dados)
summary(mdl.LCEAX)

mdl.TRMCX = lm(TRMCX ~ SPY_ETF + SPYG_ETF + SPYV_ETF, data = Dados)
summary(mdl.TRMCX)

mdl.VEIPX = lm(VEIPX ~ SPY_ETF + SPYG_ETF + SPYV_ETF, data = Dados)
summary(mdl.VEIPX)
