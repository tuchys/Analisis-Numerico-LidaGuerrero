library (pracma)
library(readxl)
#Se buscan los datos de la base de Quixeramobim por lo cual la base más cercana es la de Quixada 
araripe <- read_excel("Abril 2013 (1 em 1 hora) (2).xls", sheet = "Araripe")
araripeOriginal <- read_excel("Abril 2013 (1 em 1 hora) (2).xls", sheet = "Araripe")
datosOriginales = araripeOriginal$`Pressão Atmosférica(hPa)`
datosTemp = araripe$`Pressão Atmosférica(hPa)`
tam = length(datosTemp)
porcentaje = length(datosTemp)*0.8
toma = sort(sample(1:length(datosTemp),porcentaje, replace = F))
plot(1:tam, datosOriginales , type="l",main = "Presion atmosferica original")
datosTemp = datosTemp[toma]
#Se saca la interpolación de la muestra de Quixada y se aplica spline 
inter = spline(toma,datosTemp,n=length(datosOriginales))
length(inter$y)
plot(1:length(datosOriginales),datosOriginales,type = "l",main = "Presion atmosferica Spline")
lines(1:length(datosOriginales),inter$y,col="yellow")
legend(555,46, legend=c("Original", "Spline"),
       col=c("black", "yellow"),lty=1:2, cex=0.8)
#------------------------
c <- splinefun(toma, datosTemp )
c<-c(1:tam)
plot(1:length(datosOriginales),datosOriginales,type = "l",main = "Presion atmosferica Splinefun")
lines(1:length(datosOriginales),c,col="blue")
legend(555,46, legend=c("Original", "Splinefun"),
       col=c("black", "blue"), lty=1:2, cex=0.8)
#----------------
l<-approx(toma, datosTemp, method = "linear", n=length(datosOriginales))
plot(1:length(datosOriginales),datosOriginales,type = "l",main = "Presion atmosferica lineal")
lines(1:length(datosOriginales),l$y,col="pink")
legend(555,46, legend=c("Original", "lineal"),
       col=c("black", "pink"), lty=1:2, cex=0.8)
#Error
eSpline<- c()
ec <- c()
el <- c()

for(i in 1:tam){
  #error spline
  e = 0
  e = abs(datosOriginales[i]-inter$y[i])
  eSpline<- c(eSpline,e)
  #error de la costante
  e = 0
  e = abs(datosOriginales[i]-c[i])
  ec<- c(ec,e)
  #error lineal
  e = 0
  e = abs(datosOriginales[i]-l$y[i])
  el<- c(el,e)
  
} 
emcS = sqrt( (sum(eSpline^2))/tam)
emcC = sqrt( (sum(ec^2))/tam)
emcL = sqrt( (sum(el^2))/tam)
cat("METODO DE SPLINE \n")
cat("error EMC: ",emcS,'\n')
cat("error medio: ",(sum(eSpline))/tam,'\n')
cat("error minimo: ",min(eSpline),'\n')
cat("error maximo: ",max(eSpline),'\n')
(1 - (sum(eSpline)/sum(datosOriginales)))*100
#-----------
cat("METODO DE HERMITE \n")
cat("error EMC: ",emcC,'\n')
cat("error medio: ",(sum(ec))/tam,'\n')
cat("error minimo: ",min(ec),'\n')
cat("error maximo: ",max(ec),'\n')
(1 - (sum(ec)/sum(datosOriginales)))*100
#-----------
cat("METODO LINEAL \n")
cat("error EMC: ",emcL,'\n')
cat("error medio: ",(sum(el))/tam,'\n')
cat("error minimo: ",min(el),'\n')
cat("error maximo: ",max(el),'\n')
(1 - (sum(el)/sum(datosOriginales)))*100