library(modeest)
library(moments)
library(ggplot2)

df <- read.csv(file = "C:\\Users\\diego\\OneDrive\\Desktop\\Cato\\Codigos de prueba\\csv R\\CasosTotalesCumulativo_T.csv", fileEncoding="utf-8")
Datos <- data.frame(Csv=df)
Fecha <- df$Region

ciclo <- function(x){
AricaDiario <- c()
for (j in  seq(1,length(x)-1)){
  anterior=x[j]
  if (j==1){
    AricaDiario <- c(AricaDiario, anterior)
  }
  
  contador= j+1
  nuevo = x[contador] - anterior
  if (nuevo < -1){
    nuevo=0
  }
  AricaDiario <- c(AricaDiario, nuevo)
  anterior=nuevo
  
}
  return(AricaDiario)
}

estadigrafo <- function(y){
  
  media <- mean(y) #promedio
  mediana <- median(y) #mediana
  moda <- mlv(y, method = "mfv") # most frequent value  [moda]
  varian <- var(y) #varianza
  destd <- sd(y) #desviacion std
  rango <- IQR(y) #rango intercuartilico
  asimetria <- skewness(y) #asimetria
  curtosis <- kurtosis(y) #curtosis
  data <- data.frame(media, mediana, moda, varian, destd, rango, asimetria, curtosis)
  return(data)
}

Arica <- ciclo(Datos$Csv.Arica.y.Parinacota)
Datos$AricaDiario <- Arica
estadigrafo(Arica)
quantile(Arica) #cuantiles


Tarapaca <- ciclo(Datos$Csv.Tarapacá)
Datos$TarapacaDiario <- Tarapaca
estadigrafo(Tarapaca)
quantile(Tarapaca) #cuantiles


Antofagasta <- ciclo(Datos$Csv.Antofagasta)
Datos$AntofagastaDiario <- Antofagasta
estadigrafo(Antofagasta)
quantile(Antofagasta) #cuantiles


Atacama <- ciclo(Datos$Csv.Atacama)
Datos$AtacamaDiario <- Atacama
estadigrafo(Atacama)
quantile(Atacama) #cuantiles


Coquimbo <- ciclo(Datos$Csv.Coquimbo)
Datos$CoquimboDiario <- Coquimbo
estadigrafo(Coquimbo)
quantile(Coquimbo) #cuantiles


Valparaiso <- ciclo(Datos$Csv.Valparaíso)
Datos$ValparaisoDiario <- Valparaiso
estadigrafo(Valparaiso)
quantile(Valparaiso) #cuantiles


Metropolitana <- ciclo(Datos$Csv.Metropolitana)
Datos$MetropolitanaDiario <- Metropolitana
estadigrafo(Metropolitana)
quantile(Metropolitana) #cuantiles


OHiggins <- ciclo(Datos$Csv.O.Higgins)
Datos$OHigginsDiario <- OHiggins
estadigrafo(OHiggins)
quantile(OHiggins) #cuantiles


Maule <- ciclo(Datos$Csv.Maule)
Datos$MauleDiario <- Maule
estadigrafo(Maule)
quantile(Maule) #cuantiles


Ñuble <- ciclo(Datos$Csv.Ñuble)
Datos$ÑubleDiario <- Ñuble
estadigrafo(Ñuble)
quantile(Ñuble) #cuantiles


BioBio <- ciclo(Datos$Csv.Biobío)
Datos$BioBioDiario <- BioBio
estadigrafo(BioBio)
quantile(BioBio) #cuantiles


Araucania <- ciclo(Datos$Csv.Araucanía)
Datos$AraucaniaDiario <- Araucania
estadigrafo(Araucania)
quantile(Araucania) #cuantiles


Rios <- ciclo(Datos$Csv.Los.Ríos)
Datos$RiosDiario <- Rios
estadigrafo(Rios)
quantile(Rios) #cuantiles


Lagos <- ciclo(Datos$Csv.Los.Lagos)
Datos$LagosDiario <- Lagos
estadigrafo(Lagos)
quantile(Lagos) #cuantiles


Aysen <- ciclo(Datos$Csv.Aysén)
Datos$AysenDiario <- Aysen
estadigrafo(Aysen)
quantile(Aysen) #cuantiles


Magallanes <- ciclo(Datos$Csv.Magallanes)
Magallanes
Datos$MagallanesDiario <- Magallanes
estadigrafo(Magallanes)
quantile(Magallanes) #cuantiles


write.csv(Datos, file="Data.csv", row.names = F)


grafico3 <- ggplot(Datos, aes(x=Fecha, y=MagallanesDiario)) + geom_bar(stat="identity") 
grafico3  + xlab("Fechas") + ylab ("Cantidad de Contagios")



