newton_rapshon = function( valor,tol){
  
  aux=valor
  num=0
  derivada=0
  #####
  plot(x = 1,                 
       xlab = "X", 
       ylab = "Resultado",
       xlim = c(0, 2), 
       ylim = c(0, 2),
       main = "Newton Rapshon",
       type = "n")
  
  #####

  repeat{
    #resultado=aux-((cos(aux)^2-aux^2)/(-2*aux-2*cos(aux)*sin(aux)))
    #resultado=aux-((cos(aux)-aux)/(sin(aux)+1))
    resultado=aux-((aux^3 -aux-1)/(3*aux^2 -1))
    
    
    num=num+1
    #funcion=cos(aux)^2-aux^2
    #funcion=cos(aux)-aux
    funcion=aux^3 -aux-1
    if(funcion==0){
      break;
    }else{
      #derivada=-2*aux-2*cos(aux)*sin(aux)
      #derivada=sin(aux)+1
      derivada=3*aux^2 -1
      if(derivada==0)
        break;
    }
    if(abs(resultado-aux) < 10^(tol)){
      break;
    }else{
      cat ("Resultado:",resultado, "valor de x",aux,"valor de f(x)",funcion,"valor f'(x)",derivada,"iteraciones",num,"\n")
      #cat ("Resultado:",resultado, "valor de x",aux,"\n")
      points(x = aux, y = resultado)
    }
    if(num>100)
      break;
    aux=resultado
  }
}
# 10???8; 10???16; 10???32; 10???56;
x0 = 2
tol=-56
#f x^3-x-1
#f' 3x^2-1 
newton_rapshon (x0,tol)


