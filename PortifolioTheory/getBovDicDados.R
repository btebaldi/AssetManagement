#' Title
#'
#' @return
#' @export
#'
#' @examples
getBovDicDados = function()
{
  if(!require("tibble"))
  {
    stop("tibble not installed")
  }
  
  DicionarioColunas = tibble::tibble(col_names = rep(NA,26), col_desc=NA, begin=0, end=0)
  
  DicionarioColunas[1,  c("col_names", "col_desc")] = c("TIPREG", "TIPO DE REGISTRO")
  DicionarioColunas[1,  c("begin", "end")] = c(1, 2)
  
  DicionarioColunas[2,  c("col_names", "col_desc")] = c("DATA", "DATA DO PREGAO")
  DicionarioColunas[2,  c("begin", "end")] = c(3, 10)
  
  DicionarioColunas[3,  c("col_names", "col_desc")] = c("CODBDI", "CODIGO BDI")
  DicionarioColunas[3,  c("begin", "end")] = c(11, 12)
  
  DicionarioColunas[4,  c("col_names", "col_desc")] = c("CODNEG", "CODIGO DE NEGOCIA??O DO PAPEL")
  DicionarioColunas[4,  c("begin", "end")] = c(13, 24)
  
  DicionarioColunas[5,  c("col_names", "col_desc")] = c("TPMERC", "TIPO DE MERCADO")
  DicionarioColunas[5,  c("begin", "end")] = c(25, 27)
  
  DicionarioColunas[6,  c("col_names", "col_desc")] = c("NOMRES", "NOME RESUMIDO DA EMPRESA EMISSORA DO PAPEL")
  DicionarioColunas[6,  c("begin", "end")] = c(28, 39)
  
  DicionarioColunas[7,  c("col_names", "col_desc")] = c("ESPECI", "ESPECIFICACAO DO PAPEL")
  DicionarioColunas[7,  c("begin", "end")] = c(40, 49)
  
  DicionarioColunas[8,  c("col_names", "col_desc")] = c("PRAZOT", "PRAZO EM DIAS DO MERCADO A TERMO")
  DicionarioColunas[8,  c("begin", "end")] = c(50, 52)
  
  DicionarioColunas[9,  c("col_names", "col_desc")] = c("MODREF", "MOEDA DE REFERENCIA")
  DicionarioColunas[9,  c("begin", "end")] = c(53, 56)
  
  DicionarioColunas[10, c("col_names", "col_desc")] = c("PREABE", "PRECO DE ABERTURA")
  DicionarioColunas[10,  c("begin", "end")] = c(57, 69)
  
  DicionarioColunas[11, c("col_names", "col_desc")] = c("PREMAX", "PRECO MAXIMO")
  DicionarioColunas[11,  c("begin", "end")] = c(70, 82)
  
  DicionarioColunas[12, c("col_names", "col_desc")] = c("PREMIN", "PRECO MINIMO")
  DicionarioColunas[12,  c("begin", "end")] = c(83, 95)
  
  DicionarioColunas[13, c("col_names", "col_desc")] = c("PREMED", "PRECO MEDIO")
  DicionarioColunas[13,  c("begin", "end")] = c(96, 108)
  
  DicionarioColunas[14, c("col_names", "col_desc")] = c("PREULT", "PRECO FECHAMENTO")
  DicionarioColunas[14,  c("begin", "end")] = c(109, 121)
  
  DicionarioColunas[15, c("col_names", "col_desc")] = c("PREOFC", "PRECO DA MELHOR OFERTA DE COMPRA")
  DicionarioColunas[15,  c("begin", "end")] = c(122, 134)
  
  DicionarioColunas[16, c("col_names", "col_desc")] = c("PREOFV", "PRECO DA MELHOR OFERTA DE VENDA")
  DicionarioColunas[16,  c("begin", "end")] = c(135, 147)
  
  DicionarioColunas[17, c("col_names", "col_desc")] = c("TOTNEG", "NUMERO DE NEGOCIOS EFETUADOS")
  DicionarioColunas[17,  c("begin", "end")] = c(148, 152)
  
  DicionarioColunas[18, c("col_names", "col_desc")] = c("QUATOT", "QUANTIDADE TOTAL DE TITULOS NEGOCIADOS")
  DicionarioColunas[18,  c("begin", "end")] = c(153, 170)
  
  DicionarioColunas[19, c("col_names", "col_desc")] = c("VOLTOT", "VOLUME TOTAL DE TITULOS NEGOCIADOS")
  DicionarioColunas[19,  c("begin", "end")] = c(171, 188)
  
  DicionarioColunas[20, c("col_names", "col_desc")] = c("PREEXE", "PRECO DE EXERCICIO")
  DicionarioColunas[20,  c("begin", "end")] = c(189, 201)
  
  DicionarioColunas[21, c("col_names", "col_desc")] = c("INDOPC", "INDICADOR DE CORRECAO")
  DicionarioColunas[21,  c("begin", "end")] = c(202, 202)
  
  DicionarioColunas[22, c("col_names", "col_desc")] = c("DATVEN", "DATA DO VENCIMENTO")
  DicionarioColunas[22,  c("begin", "end")] = c(203, 210)
  
  DicionarioColunas[23, c("col_names", "col_desc")] = c("FATCOT", "FATOR DE COTACAO")
  DicionarioColunas[23,  c("begin", "end")] = c(211, 217)
  
  DicionarioColunas[24, c("col_names", "col_desc")] = c("PTOEXE", "PRECO DE EXERCICIO EM PONTOS")
  DicionarioColunas[24,  c("begin", "end")] = c(218, 230)
  
  DicionarioColunas[25, c("col_names", "col_desc")] = c("CODISI", "CODIGO DO PAPEL NO SISTEMA ISIN")
  DicionarioColunas[25,  c("begin", "end")] = c(231, 242)
  
  DicionarioColunas[26, c("col_names", "col_desc")] = c("DISMES", "NUMERO DE DISTRIBUICAO")
  DicionarioColunas[26,  c("begin", "end")] = c(243, 245)
  
  
  return(DicionarioColunas)
}

getBovColSpecs = function()
{
  
  if(!require("readr"))
  {
    stop("tibble not installed")
  }
  
  col_specs = readr::cols(
    TIPREG = col_integer(),
    DATA   = col_integer(),
    CODBDI = col_character(),
    CODNEG = col_character(),
    TPMERC = col_integer(),
    NOMRES = col_character(),
    ESPECI = col_character(),
    PRAZOT = col_integer(),
    MODREF = col_character(),
    PREABE = col_double(),
    PREMAX = col_double(),
    PREMIN = col_double(),
    PREMED = col_double(),
    PREULT = col_double(),
    PREOFC = col_double(),
    PREOFV = col_double(),
    TOTNEG = col_integer(),
    QUATOT = col_double(),
    VOLTOT = col_double(),
    PREEXE = col_double(),
    INDOPC = col_integer(),
    DATVEN = col_integer(),
    FATCOT = col_integer(),
    PTOEXE = col_double(),
    CODISI = col_character(),
    DISMES = col_character()
  )
  
  return(col_specs)
}