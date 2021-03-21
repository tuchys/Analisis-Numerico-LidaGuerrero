#PROGRAMA METODO DE GAUSS-SIDEL
#SOLUCION DEL PROBLEMA ANTERIOR
# Elaborado por Msc. Jesus A. Suniaga Q.
# datos del problema:
# tol = margen de tolerancia
# N=numero máximo de iteracciones
#f1, f2,f3 filas de la matriz A
# b=vector de terminos independientes
# ex, ey, ez errores absolutos de x,y y z respectivamente
tol=0.0001
N=20
# FILAS DE LA MATRIZ

f1=c(1,3,-2)
f2=c(4,-1,1)
f3=c(1,1,7)

# VECTOR DE TERMINOS INDEPENDIENTES

b=c(18.666,27.3,3.1416)

# matriz A

a=rbind(f1,f2,f3)

print(a)
#valores iniciales
x=0; y=0; z=0
# ciclo iterativo

for ( i in 1:N) {
  
  xnew=(b[1]-(a[1,2]*y+a[1,3]*z))/a[1,1]
  
  ynew=(b[2]-(a[2,1]*xnew+a[2,3]*z))/a[2,2]
  
  znew=(b[3]-(a[3,1]*xnew+a[3,2]*ynew))/a[3,3]
  
  cat("\n","calculo de xnew,ynew,znew, etapa " ,i)
  
  cat("\n", c(xnew,ynew,znew))
  ex=abs(x-xnew)
  ey=abs(y-ynew)
  ez=abs(z-znew)
  
  cat("\n Erores en la etapa, ",i,"ex,ey,ez", c(ex,ey,ez))
  if (ex < tol & ey < tol & ez< tol)
    
  {
    cat("\n,convergencia alcanzada en ", i, "iteraciones, resultado: ( x, y, z) =",
          c(xnew,ynew,znew))
    cat("\n________________________\n")
    
    stop()
  }
  x=xnew
  y=ynew
  z=znew
  cat("\n,valores iniciales iteracion ", i+1,c(x,y,z))
}
