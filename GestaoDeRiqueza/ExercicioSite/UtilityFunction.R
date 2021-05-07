Ut = function (R, rf, gamma, variance, level=0)
{
  Util  = as.vector(R-rf)
  Util = Util - gamma * 0.5 * variance
  
  return ( Util - as.vector(level)) 
}

x = c(0.04, 0.05, 0.06, 0.07, 0.08)
-2*(0.06 -0.10)/x^2
gmm = function(x, func, rf, gamma, variance, level)
{
  ret = Ut(x, rf, gamma, variance, level)

  erro = t(ret) %*% ret

  return(erro)
}
