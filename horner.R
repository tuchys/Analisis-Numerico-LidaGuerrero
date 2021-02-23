rm(list = ls())

metodoHorner = function (polinomio, x0){
  resultado = polinomio[1]
  sumas = 0
  multiplicaciones = 0
  
  for(k in polinomio[2:length(polinomio)]){
    
    resultado = x0 * resultado + k
    sumas = sumas + 1
    multiplicaciones = multiplicaciones + 1
    
  }
  cat ("Resultado:",resultado,"\n")
  cat ("Numero de sumas:",sumas, "\n")
  cat ("Numero de Multiplicaciones:", multiplicaciones,"\n")
}

x0 = 1
polinomio <- c(2,0,-3,3,-4)
metodoHorner (polinomio, x0)
