# Carrega bibliotecas
library(dplyr)
library(readxl)
library(tibble)

# Limpa variaveis
rm(list=ls())


# importa Dados e acerta nome das colunas
Dados <- read_excel("./Listas/L4-Q1.xlsx",
                    sheet = "Planilha1",
                    range = "A3:U206")
colnames(Dados)

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

# Calcula retorno dos fundos
Dados$ln_Murano = log(Dados$Murano)
Dados$Dln_Murano = Dados$ln_Murano - dplyr::lag(Dados$ln_Murano)

Dados$ln_Maua = log(Dados$Maua)
Dados$Dln_Maua = Dados$ln_Maua - dplyr::lag(Dados$ln_Maua)

Dados$ln_SafraGalileo = log(Dados$SafraGalileo)
Dados$Dln_SafraGalileo = Dados$ln_SafraGalileo - dplyr::lag(Dados$SafraGalileo)

Dados$ln_KapitaloZeta = log(Dados$KapitaloZeta)
Dados$Dln_KapitaloZeta = Dados$ln_KapitaloZeta - dplyr::lag(Dados$KapitaloZeta)

Dados$ln_GardeDArtagnan = log(Dados$GardeDArtagnan)
Dados$Dln_GardeDArtagnan = Dados$ln_GardeDArtagnan - dplyr::lag(Dados$GardeDArtagnan)

Dados$ln_CHSG_Verde = log(Dados$CHSG_Verde)
Dados$Dln_CHSG_Verde = Dados$ln_CHSG_Verde - dplyr::lag(Dados$CHSG_Verde)

Dados$ln_Modal_Tatico = log(Dados$Modal_Tatico)
Dados$Dln_Modal_Tatico = Dados$ln_Modal_Tatico - dplyr::lag(Dados$Modal_Tatico)

Dados$ln_SPX_Raptor = log(Dados$SPX_Raptor)
Dados$Dln_SPX_Raptor = Dados$ln_SPX_Raptor - dplyr::lag(Dados$SPX_Raptor)

Dados$ln_JGP_Max = log(Dados$JGP_Max)
Dados$Dln_JGP_Max = Dados$ln_JGP_Max - dplyr::lag(Dados$JGP_Max)

Dados$ln_Bahia_Marau = log(Dados$Bahia_Marau)
Dados$Dln_Bahia_Marau = Dados$ln_Bahia_Marau - dplyr::lag(Dados$Bahia_Marau)

# Faz analise econometrica dos modelos

# Define a formula da regressao
regressores = "MKT + SMB + HML + IML + WML + Dln_USD + Dln_Juros_1 + Dln_Juros_2 + Dln_Selic + Dln_IPCA"


varDependente = c("Dln_Murano",
                  "Dln_Maua",
                  "Dln_SafraGalileo",
                  "Dln_KapitaloZeta",
                  "Dln_GardeDArtagnan",
                  "Dln_CHSG_Verde",
                  "Dln_Modal_Tatico",
                  "Dln_SPX_Raptor",
                  "Dln_JGP_Max",
                  "Dln_Bahia_Marau")

# Inicializa uma tabela que vai conter os resultados das regressoes
regressData = tibble::tibble(Fundo = varDependente, alpha = NA)


for(i in 1:nrow(regressData)){
  
  formula = paste(regressData[i,1], regressores, sep = " ~ ")
  print(formula)

  mdl = lm(formula, data = Dados)
  print(summary(mdl))
  
  regressData[i,2] = mdl$coefficients["(Intercept)"]
}

regressData %>% dplyr::arrange(desc(alpha))
