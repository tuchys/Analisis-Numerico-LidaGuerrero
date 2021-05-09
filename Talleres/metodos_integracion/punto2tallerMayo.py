import numpy as np
from scipy.interpolate import lagrange
import matplotlib.pyplot as plt
from scipy import integrate

#Integral por el metodo de trapecios
def trapecios(polinomio,a,b,m):
    h = (b-a)/m
    s = 0
    for i in range(1,m):
        s = s+polinomio(a+i*h)
    r = h/2*(polinomio(a)+2*s+polinomio(b))

    return r

#Integral por el metodo de simpson
def simpson(polinomio,a,b,m):
    h = (b-a)/m
    s = 0
    x = a
    for i in  range(1, m):      
        s = s+2*(i%2+1)* polinomio(x+i*h)
    s = h/3 * (polinomio(a)+s+polinomio(b))
    return s
        
#Llenar los arreglos y definir datos
x=np.array([0 , 0.2, 0.4, 0.6, 0.8])
y=np.array([3.592, 3.110, 3.017, 2.865, 2.658])
inferior = 0
superior = 0.8
n = 20
rombergEx = 2
#Interpolacion para hallar polinomio
polinomio=lagrange(x,y)
print('x:',x)
print('y:',y)
print('El polinomio es: ')
print(polinomio)

xs=np.linspace(x.min(),x.max())
ys=polinomio(xs)

#Grafica de la funcion
plt.plot(x,y,'o')
plt.ylabel('y')
plt.xlabel('x')
plt.axis([0,1,0,4])
plt.plot(xs,ys,':')
plt.grid(True)
plt.text(0.3, 0.5, polinomio)
plt.show()

#Llamado a funciones de integrales 
print('\n Trapecios \n')
for i in range(1, n+1):
    print( i,'  ', trapecios(polinomio,inferior, superior,i))
print('\n \n Simpson \n ')
for j in range(1, n+1):
    print( j,'  ', simpson(polinomio, inferior, superior, j))
#Llamado a la integral de romberg para evaluar hasta con grado 2
print('\n \n Romberg \n \n', integrate.romberg(polinomio, inferior, superior, divmax=rombergEx), '\n \n')
