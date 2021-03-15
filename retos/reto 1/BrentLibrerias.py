# -*- coding: utf-8 -*-


def f(x):
    return (x**3 - 2*x**2+(4*x)/3- (8/27))

from scipy import optimize

root = optimize.brentq(f, 0, 1,)
print("La raiz encontrada es: ",root)
