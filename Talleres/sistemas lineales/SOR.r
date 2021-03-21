#install.packages("Rlinsolve")

#Para cualquier w ??? R tenemos que ??(B(w)) ??? |1 ??? w|; por lo tanto, el m´etodo SOR converge solo si w ??? (0, 2) 


#cuando SOR w = 1 (col 1) = metodo Gauss Sidel  



require(Rlinsolve)


A = matrix(c(4,-1,1, 3,7,3, 1,1,7), 3, 3)
A
x = matrix(c(27.3, 18.666, 3.14),3,1)
x
b = A%*%x
b

out1 = lsolve.sor(A,b)
out2 = lsolve.sor(A,b,w=0.5)
out3 = lsolve.sor(A,b,w=1.5)
matout = cbind(matrix(x),out1$x, out2$x, out3$x);
colnames(matout) = c("true x","SOR 1 = GS", "SOR w=0.5", "SOR w=1.5")
print(matout)
