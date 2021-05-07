#
# Leitura da serie historica da bovespa em formato de larguras fixas
# para data.frame, csv, feather
# 2019-07  
# Bruno Barbosa

# Limpeza de variaveis antigas
rm(list=ls())

# biblioteca utilizadas
library(tibble)
library(readr)
library(dplyr)
source("./getBovDicDados.R")

# Diretorio do arquivo de serie historica
dir  <- "../../BaseHist/"
file <- list.files(path = dir)

# Define nome completo do arquivo
fileFullPath <- paste0(dir, file[16])

# Declara o dicionario de dados do arquivo da bovespa
DicionarioColunas = getBovDicDados()
col_specs = getBovColSpecs()

# Calcula o tamanho das colunas
col_widths = DicionarioColunas$end - DicionarioColunas$begin + 1

# Faz a leitura do arquivo
data = read_fwf(fileFullPath, fwf_widths(col_widths, DicionarioColunas$col_names), col_types = col_specs, skip = 1)

# Lista problemas de importacao
problems(data)
nrow(data)

# Como o trailer e diferente do resto do arquivo retiramos ele manualmente.
data = data %>% dplyr::filter(TIPREG == 1)

# Arruma preco das bases
data$PREABE = data$PREABE/100
data$PREEXE = data$PREEXE/100
data$PREMAX = data$PREMAX/100
data$PREMIN = data$PREMIN/100
data$PREMED = data$PREMED/100
data$PREULT = data$PREULT/100
