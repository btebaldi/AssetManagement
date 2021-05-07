# limpeza
rm(list = ls())


source(file = "./types.R")

# Cria uma simulacao de mercado.

DU = 252
AnosDeAnalise = 5

TotalDias =AnosDeAnalise*DU

# Informacoes da Renda Variavel
RV.media=1.0002667
RV.dp=0.018039369

# Informacoes da Renda Fixa
RF.media=(1+0.1)^(1/DU)
RV.var=0


# indice de dias

market = GetMarket(TotalDias);

market[,"Rv.Ret"] = rnorm(TotalDias, mean = RV.media, sd = RV.dp);
market[,"Rf.Ret"] = RendaFixa.Rendimento = rep(RF.media, TotalDias);

for (dia in market[,"Data"])
{
   if (dia == 1){
     market[dia, "Rv.Val"] = 1
     market[dia, "Rf.Val"] = 1
   }
  else
  {
    market[dia, "Rv.Val"] = market[dia-1, "Rv.Val"]  * market[dia, "Rv.Ret"] ;
    market[dia, "Rf.Val"] = market[dia-1, "Rf.Val"]  * market[dia, "Rf.Ret"] ;
  }
}

tail (market)
tail (market)


# Contrucao do portifolio
portifolio = GetPosicoes(TotalDias)


# Alucacao inicial do portifolio
portifolio[1, "Rv.MktVal"] = 0.5;
portifolio[1, "Rf.MktVal"] = 0.5;

portifolio[1, "Rv.Un"] = portifolio[1, "Rv.MktVal"] / market[1, "Rv.Val"];
portifolio[1, "Rf.Un"] = portifolio[1, "Rf.MktVal"] / market[1, "Rf.Val"];

portifolio[1, "TotalVal"] = portifolio[1, "Rv.MktVal"] + portifolio[1, "Rf.MktVal"];

portifolio[1, "Rv.Aloc"] = portifolio[1, "Rv.MktVal"] / portifolio[1, "TotalVal"];
portifolio[1, "Rf.Aloc"] = portifolio[1, "Rf.MktVal"] / portifolio[1, "TotalVal"];


# Calculo da carteira no tempo


for (dia in portifolio[-1,"Data"])
{
  # As unidades do dia sao as mesmas que as unidades do dia anterior
  portifolio[dia, "Rv.Un"] = portifolio[dia-1, "Rv.Un"];
  portifolio[dia, "Rf.Un"] = portifolio[dia-1, "Rf.Un"];
  
  portifolio[dia, "Rv.MktVal"] = portifolio[dia, "Rv.Un"] * market[dia, "Rv.Val"];
  portifolio[dia, "Rf.MktVal"] = portifolio[dia, "Rf.Un"] * market[dia, "Rf.Val"];
  
  portifolio[dia, "TotalVal"] = portifolio[dia, "Rv.MktVal"] + portifolio[dia, "Rf.MktVal"];
  
  portifolio[dia, "Rv.Aloc"] = portifolio[dia, "Rv.MktVal"] / portifolio[dia, "TotalVal"];
  portifolio[dia, "Rf.Aloc"] = portifolio[dia, "Rf.MktVal"] / portifolio[dia, "TotalVal"];

}


plot(portifolio[, "TotalVal"])
