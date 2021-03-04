intersec = function( inicio,limite,expo){
  
  num=0
  resultado=0
  x=1
  #####
  plot(x = 1,                 
       xlab = "X", 
       ylab = "Resultado",
       xlim = c(0, 5), 
       ylim = c(0, 5),
       main = "Sumatoria al cuadrado",
       type = "n")
  
  #####
  resultado=cos(1)+1
  cat("Valor de cos(1)+1: ",resultado ,"\n")
  resultado=cos(2)+1 
  cat("Valor de cos(2)+1: ",resultado ,"\n")
  repeat{
    resultado=x-(x^2)*(x-(x-1))/((x)^2-(x-1)^2)

    cat ("Resultado:",resultado,"iteraciones",num,"%\n")
    points(x = num, y = resultado)

    if(num>=1)
      break;
    num=num+1
    inicio=limite
    if(abs(resultado-x) < 10^(-expo))
      break;
    x=resultado
    
  }
}
intersec (1,2,9)
