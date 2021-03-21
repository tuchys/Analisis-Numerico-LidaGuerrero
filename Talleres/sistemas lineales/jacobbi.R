# NOT RUN {
install.packages('JacobiEigen')


#V <- crossprod(matrix(c(4,-1,1, 3,7,3, 1,1,7), 3, 3))
V <- crossprod(matrix(c(1,3,-2, 4,-1,1, 1,1,7 ), 3, 3))
#Jacobi(V)
eigjacobi(V)
# }
