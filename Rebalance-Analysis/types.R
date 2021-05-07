# define um ativo
GetPosicoes = function(nDias)
{
  retorno = matrix(data = NA,
                   nrow = nDias,
                   ncol = 8)
  colnames(retorno) = c(
    "Data",
    "Rv.Un",
    "Rv.MktVal",
    "Rf.Un",
    "Rf.MktVal",
    "TotalVal",
    "Rv.Aloc",
    "Rf.Aloc"
  )
  
  retorno[, "Data"] = 1:nDias
  
  
  return(retorno)
}

GetMarket = function(nDias)
{
  retorno = matrix(data = NA,
                   nrow = nDias,
                   ncol = 5)
  colnames(retorno) = c("Data", "Rv.Val", "Rv.Ret", "Rf.Val", "Rf.Ret")
  
  retorno[, "Data"] = 1:nDias
  
  
  return(retorno)
}
