# Carrega bibliotecas
library(dplyr)
library(readxl)

# Limpa variaveis
rm(list=ls())


# importa Dados e acerta nome das colunas
Dados <- read_excel("./Listas/L3-Q6.xlsx",
                    sheet = "Planilha1",
                    range = "A3:P206")
head(Dados)

# Arrumando o banco de dados
Dados$Juros_1_aa = 1 + Dados$Juros_1/100
Dados$Juros_2_aa = 1 + Dados$Juros_2/100
Dados$Selic_aa = 1 + Dados$Selic/100
Dados$IPCA_am = 1 + Dados$IPCA/100

Dados$ln_Juros_1 = log(Dados$Juros_1_aa)
Dados$ln_Juros_2 = log(Dados$Juros_2_aa)
Dados$ln_Selic = log(Dados$Selic_aa)
Dados$ln_IPCA = log(Dados$IPCA_am)

Dados$Dln_Juros_1 = Dados$ln_Juros_1 - dplyr::lag(Dados$ln_Juros_1)
Dados$Dln_Juros_2 = Dados$ln_Juros_2 - dplyr::lag(Dados$ln_Juros_2)
Dados$Dln_Selic = Dados$ln_Selic - dplyr::lag(Dados$ln_Selic)
Dados$Dln_IPCA = Dados$ln_IPCA - dplyr::lag(Dados$ln_IPCA)

Dados$ln_USD = log(Dados$USD)
Dados$Dln_USD = Dados$ln_USD - dplyr::lag(Dados$ln_USD)

Dados$ln_Verde = log(Dados$Verde)
Dados$Dln_Verde = Dados$ln_Verde - dplyr::lag(Dados$ln_Verde)

colnames(Dados)

# Faz analise econometrica dos modelos

# Define a formula da regressao
f = formula("Dln_Verde ~ MKT + SMB + HML + IML + WML + Dln_USD + 
                    Dln_Juros_1 + Dln_Juros_2 + Dln_Selic + Dln_IPCA")

# Modelo sem restricao temporal
mdl_1 = lm(f, data = Dados)
summary(mdl_1)

# Mandado Lula
mdl_2 = lm(f, data = dplyr::filter(Dados, Lula == 1))
summary(mdl_2)

# Mandado Dilma
mdl_3 = lm(f, data = dplyr::filter(Dados, Dilma == 1))
summary(mdl_3)

# Periodo Pre-Subprime
mdl_4 = lm(f, data = dplyr::filter(Dados, Pre_Subprime == 1))
summary(mdl_4)

# Periodo Pos-Subprime
mdl_5 = lm(f, data = dplyr::filter(Dados, Pos_Subprime == 1))
summary(mdl_5)

