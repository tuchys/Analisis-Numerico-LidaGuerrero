f <- function(x){
  return(1+sin(3*x^3))
}
x <- seq(-2, 2, by=0.1)
y <- f(x)
plot(x,y)
curve(exp(1+sin(3*x^3)), -2, 2, xname = "x",col="blue")

trapezoid <- function(f, a, b, n) {
  intval <- integrate(f,a,b)
  val2 = intval$value
  h <- (b-a)/n
  x <- seq(a, b, by=h)
  y <- f(x)
  s <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  suma <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  cat("Integral trapecio: ",suma,"\n")
  error = abs(val2-suma)
  if (error == 0) {
    cat("Error trapecio: 0 ","\n")
  } else {
    cat("Error trapecio: ",error,"\n")
  }
  
}
#Parámetros

a <--1          # Limite Inferior. 
b <- 1      # Limite Superior. 
tol <- 1e-8    # Error permitido 
n<- 5
trapezoid(f,-1,1,10)


simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
  
  return(s)
}

simpsons.rule(f, -1, 1)