
# Primeras 4 funciones del paper, podria ser algo parecido a esta formula.

ValueofAyL <- function(AssetoLiability,Rd_f,n){
  Aux=0
  for (i in 1:n) {
    Aux=Aux+AssetoLiability[i,1]*((1+Rd_f)^-i)
  }
  return(Aux)
}

# Funciones de la 5 a la 8.

MacaulayDuration <- function(AssetoLiability,Rd_f,n){
  Aux=0
  for (i in 1:n) {
    Aux=Aux+i*AssetoLiability[i,1]*((1+Rd_f)^-i)
  }
  Aux=Aux/ValueofAyL(AssetoLiability,Rd_f,n)
  return(Aux)
}

# Funciones de la 9 a las 12.

Convexity <- function(AssetoLiability,Rd_f,n){
  Aux=0
  for (i in 1:n) {
    Aux=Aux+i*(i+1)*AssetoLiability[i,1]*((1+Rd_f)^-i)
  }
  Aux=(0.5*Aux)/((ValueofAyL(AssetoLiability,Rd_f,n))*((1+Rd_f)^2))
  return(Aux)
}

# Funciones 13 y 14.

OverallDur <- function(AssetoLiabilityD,AssetoLiabilityF,Rd,Rf,n,AoL,S){
  Aux=0
  Aux= (ValueofAyL(AssetoLiabilityD,Rd,n)*MacaulayDuration(AssetoLiabilityD,Rd,n))/AoL + 
    (S*ValueofAyL(AssetoLiabilityF,Rf,n)*MacaulayDuration(AssetoLiabilityF,Rf,n))/AoL
  return(Aux)
}

# STD Standard Error por los loles.

std <- function(x) sd(x)/sqrt(length(x))

# Cuota de Bono Installment.

Cuota <- function(MontoCredito,r,Meses){
  Aux=0
  Aux=MontoCredito*((1+r)^Meses)*r/((1+r)^Meses-1)*matrix(1,Meses,1)
  return(Aux)
}

# Buscador en el Data Frame.

SearchValue <- function(DataFrame,Row_Name,Col_Name){
  Aux=0
  Aux=DataFrame[which(rownames(DataFrame)==Row_Name),which(colnames(DataFrame)==Col_Name)]
  return(Aux)
}

# Liabilitys Present Value.
LiaValue <- function(Actual_Value,Years_Matrix,Rates_Matirx){
  Aux=matrix(0,1,length(Years_Matrix))
  for (i in 1:length(Years_Matrix)) {
    if (i==1) {
      Aux[i]<-Actual_Value/((1+Rates_Matirx[i])^Years_Matrix[i])
    }
    else {
      Aux[i]<-Aux[i-1]/((1+Rates_Matirx[i])^Years_Matrix[i])
    }
  }
  return(Aux)
}