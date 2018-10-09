

AssetClass=c('US Bonds', 'Int Bonds', 'US Large Growth', 'US Large Value', 
             'US Small Growth', 'US Small Value', 'Int Dev Equity',
             'Int Emerg Equity')
ret_Hist=c(3.15, 1.75,-6.39,-2.86, -6.75,-0.54, -6.75, -5.26)
ret_CAPM_GSMI=c(0.02, 0.18, 5.57, 3.39, 6.59, 3.16, 3.92, 5.60)
ret_CAPM_Port=c(0.08, 0.67, 6.41, 4.08, 7.43, 3.70, 4.80, 6.60)
eqVec=c(0.08, 0.67, 6.41, 4.08, 7.43, 3.70, 4.80, 6.60)

# Weights
W_Sigma = matrix(data = NA, nrow = length(AssetClass), ncol=length(AssetClass))

W_Sigma[,1] = c(0.001005,2,3,4,5,6,7,8)

df <- data.frame(AssetClass, ret_Hist, ret_CAPM_GSMI, ret_CAPM_Port, eqVec)
