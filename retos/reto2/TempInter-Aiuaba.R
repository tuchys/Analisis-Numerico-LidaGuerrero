library(readxl)
library(PolynomF)
library(pracma)

#datos de aiuaba
aiuaba <- read_excel("Abril 2013 (1 em 1 hora) (2).xls", sheet = "Aiuaba")
tempInter <-aiuaba$`Temp. Interna (ºC)`
datosOriginales <-aiuaba$`Temp. Interna (ºC)`
tam<-length(tempInter)
por<-tam*0.8
sorteado<-sort(sample(1:tam,por, replace = F))
faltantes<-1:tam
faltantes <- faltantes[-sorteado]
plot(1:tam, tempInter , type="l",xlab = "numeracion de dias",ylab = "Temp. interna relativa",main = "Temp. interna original")
datos<-tempInter[sorteado]

#interpolacion por spline
spli<-spline(1:por, datos, n=tam)
print(spli)
length(spli$y)
plot(1:tam, tempInter , type="l",xlab = "numeracion de dias",ylab = "Temp. interna relativa",main = "Temp. interna Spline")
lines(1:tam, spli$y, col="red")
legend(630, 60, legend=c("Original", "Spline"),
       col=c("black", "red"), lty=1:2, cex=0.8)
#interpolacion 
c <- splinefun(sorteado, datos )
c<-c(1:tam)
#c<-approx(1:por, datos, method = "constant", n=tam)
l <-approx(1:por, datos, method = "linear", n=tam)
plot(1:tam, tempInter , type="l",xlab = "numeracion de dias",ylab = "Temp. interna relativa",main = "Temp. interna Lineal")
lines(1:tam, c, col="pink")
legend(630, 60, legend=c("Original", "Splinefun"),
       col=c("black", "pink"), lty=1:2, cex=0.8)
plot(1:tam, tempInter , type="l",xlab = "numeracion de dias",ylab = "Temp. interna relativa",main = "Temp. interna Splinefun")
lines(1:tam, l$y, col="yellow")
legend(630, 60, legend=c("Original", "lineal"),
       col=c("black", "yellow"), lty=1:2, cex=0.8)

#Error
eSpline<- c()
ec <- c()
el <- c()

for(i in 1:tam){
  #error spline
  e = 0
  e = abs(tempInter[i]-spli$y[i])
  eSpline<- c(eSpline,e)
  #error de la costante
  e = 0
  e = abs(tempInter[i]-c[i])
  ec<- c(ec,e)
  #error lineal
  e = 0
  e = abs(tempInter[i]-l$y[i])
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
