sumatoria = function( limite,error){
  
  num=0
  resultado=0
  #####
  plot(x = 1,                 
       xlab = "X", 
       ylab = "Resultado",
       xlim = c(0, 10), 
       ylim = c(0, 60),
       main = "Sumatoria al cuadrado",
       type = "n")
  
  #####
  repeat{
    resultado=resultado+(num^2)
    errorPorc=(error/resultado)*100
    cat ("Resultado:",resultado,"iteraciones",num,"Error relativo porcentual ",errorPorc,"%\n")
    points(x = num, y = resultado)
    

    if(num>=limite)
      break;
    num=num+1
  }
}
sumatoria (4,0.1)