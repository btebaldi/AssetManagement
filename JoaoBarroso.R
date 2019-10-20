# limpa variaveis antigas
rm(list = ls())

# bibliotecas utilizadas.
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)

# carrega base de dados
database <- read_excel("C:/Users/Tebaldi/Downloads/trabalho 1 2019/vale_occam2019.xlsx")

head(database)

# Modelando Ação da Vale com Pço Minério de Ferro
# Vide http://www.valor.com.br/valor-investe/o-estrategista/2926704/acoes-da-vale-sob-navalha-de-ockham
# e https://www.valor.com.br/financas/4942060/vale-descola-do-preco-de-minerio
# Fonte dos dados: vide vale_occam2019.xlsx



# 2009-05 a 2017-12
# arrumando datas 
database$data = lubridate::ymd(database$ano *10000 + database$mes*100 + 01)

# definindo time series
# tsset Data

# Criando variaveis em logs
database$l_ore =log(database$iron_ore)
database$l_vale=log(database$vale_adr)
database$l_dja =log(database$DJA)
database$l_ibov=log(database$IBOV)



# Criando as variÃ¡veis em log diferenÃ§as
database$dl_ore =database$l_ore - dplyr::lag(database$l_ore)
database$dl_vale=database$l_vale - dplyr::lag(database$l_vale)
database$dl_dja =database$l_dja - dplyr::lag(database$l_dja)

# Ajuste sua amostra
# drop if data<2009.01

database.sample = dplyr::filter(database, data >= "2009-05-01")



# Olhando os dados
# twoway (line l_ore Data, sort) (line l_vale Data, yaxis(2)), scheme(s2mono)



ggplot(database.sample, aes(x=data)) + 
  geom_line(aes(y=l_ore, colour = "Iron Ore")) +
  geom_line(aes(y=l_vale+2, colour = "Vale")) +
  scale_y_continuous(sec.axis = sec_axis(~.-2, name = "ln(vale)")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "ln(IronOre)",
         x = "Date",
         colour = NULL) +
  theme(legend.position = c(0.3, 0.15))

# Correlograma VALE
# corrgram XXXXXX , yw

acf(database.sample$l_vale)
pacf(database.sample$l_vale)

acf(database.sample$dl_vale)
pacf(database.sample$dl_vale)

acf(database.sample$l_ore)
pacf(database.sample$l_ore)

acf(database.sample$dl_ore)
pacf(database.sample$dl_ore)


# Testes raiz unitária
varsoc l_vale
dfuller l_vale, regress lags(XXXXX)
dfgls l_vale, ers


******* Modelo ARIMA(p,d,q) previsÃ£o VALE
arima l_vale if data<=2017.12, arima(XX,XX,XX)
predict l_vale110, y
predict e_l_vale110, res
estat ic


label variable l_vale110 "predict arima110"
label variable l_vale111 "predict arima111"


twoway (line l_vale Data if tin(2014m1,), sort) (line l_vale110 Data if tin(2014m1,)) (line l_vale111 Data if tin(2014m1,)), scheme(s2mono)



******Comparando modelos

* Modelo com explicativas
reg l_vale l.l_dja l.l_ore if tin( ,2017m12)
estat bgodfrey, lags(1 2 3 4) small
predict l_valeEST, xb
predict e_l_valeEST, res
estat ic

* Modelo ADL-SW


label variable l_valeEST "predict EST"
label variable l_valeADL "predict ADL"

twoway (line l_vale Data if tin(2014m1,), sort) (line l_vale110 Data if tin(2014m1,)) ///
  (line l_valeEST Data if tin(2014m1,)) (line l_valeADL Data if tin(2014m1,)), scheme(s2mono)

twoway (line e_l_vale110 Data if tin(2014m1,)) (line e_l_valeEST Data if tin(2014m1,)) ///
  (line e_l_valeADL Data if tin(2014m1,)), scheme(s2mono)



**** Comparando previsÃµes fora da amostra
sum e_l_vale110 if tin(2017m1, )
display "MSPE=" r(sd)^2 + r(mean)^2
display "RMSPE=" sqrt( r(sd)^2 + r(mean)^2)

sum e_l_valeEST if tin(2017m1, )
display "MSPE=" r(sd)^2 + r(mean)^2
display "RMSPE=" sqrt( r(sd)^2 + r(mean)^2)

sum e_l_valeADL if tin(2017m1, )
display "MSPE=" r(sd)^2 + r(mean)^2
display "RMSPE=" sqrt( r(sd)^2 + r(mean)^2)







****** Modelo EconomÃ©trico: AnÃ¡lise de CointegraÃ§Ã£o
reg dl_vale dl_dja dl_ore if tin( ,2017m12)
estat bgodfrey, lags(1 2 3 4) small

reg l_vale l_dja l_ore if tin( ,2017m12)
estat bgodfrey, lags(1 2 3 4) small
predict Res, r


*Teste cointegraÃ§Ã£o - EG
varsoc Res if tin( ,2017m12)
egranger XXXXXXX if tin( ,2017m12), reg lags(XXXXXX)


* Modelo ECM 
reg d.l_vale XXXXXX if tin( ,2017m12)
estat bgodfrey, lags(1 2 3 4) small



