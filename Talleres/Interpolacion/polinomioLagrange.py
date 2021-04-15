
import numpy as np
from scipy.interpolate import lagrange
import matplotlib.pyplot as plt

x=np.array([4410000 , 4830000, 5250000])
y=np.array([1165978, 1329190, 1501474])

polinomio=lagrange(x,y)
xi=5000000
yi=polinomio(xi)
print(yi)

xs=np.linspace(x.min(),x.max())
ys=polinomio(xs)

plt.plot(x,y,'o')
plt.plot(xi,yi,'sr')
plt.plot(xs,ys,':')

plt.grid()
plt.show()


