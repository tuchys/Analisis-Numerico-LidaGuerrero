taylor = function(cifras, dato)
{
  taylor = 0
  flag = TRUE
  contador = 1
  grado = 1
  while (grado<=cifras)
  {
    factorial = 1
    aux = grado
    while(aux >= 1)
    {
      factorial = factorial * aux
      aux = aux - 1
    }
    taylor2 = taylor + (1/factorial)* ( sqrt(dato)^grado )
    grado = grado + 1
  
    taylor = taylor2
  }
  return (taylor2)
}
