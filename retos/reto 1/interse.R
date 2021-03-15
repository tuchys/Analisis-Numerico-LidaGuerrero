library(ggplot2)
intersc = function( va,vb){
  valA=va
  valB=vb
  num=0

  repeat{
    x=valB
    fb=(sqrt(19)/sqrt(x))-((10-x^2)/x)
    x=valA
    fa=(sqrt(19)/sqrt(x))-((10-x^2)/x)
    fpf1=valB-(fb*(valB-valA))/(fb-fa)
    if(abs(fpf1-valA) < 10^(-16)){
      break;
    }else{
      cat ("Resultado:",fpf1, " iteración:",num,"\n")
      valA=fpf1
      num=num+1
    }
  }
  x=valA
  corteY=((10-x^2)/x)
  cat ("Las funciones cortan en X:",valA," Y:",corteY, "\n")
  
  #union graficas con punto de corte
  x1 <- seq(from=0.1,to=3,by=0.1)
  datos <- data.frame(x = x1, y = ((10-x1^2)/x1), z =(sqrt(19)/sqrt(x1)))  # se crea el data frame con los datos
  ggplot(datos) +
    geom_line(aes(x=x1, y=y),col="blue") +
    geom_line(aes(x=x1, y=z),col="red") + 
    geom_point(aes(x=valA, y=corteY),col="green")  
}
x0 = 0.1
xf=4
intersc (x0,xf)




