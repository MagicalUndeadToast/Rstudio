
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

SearchValue <- function(DataFrame,RowName,ColName){
  Aux=0
  Aux=DataFrame[which(rownames(DataFrame)==Fecha),which(colnames(DataFrame)==Tenor)]
  return(Aux)
}
