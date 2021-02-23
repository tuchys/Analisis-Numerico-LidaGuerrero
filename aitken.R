aitken <- function(f, x0, nmax = 12, tol = 1e-8, ...) {
  if (!is.numeric(x0) || length(x0) != 1)
    stop("Argumento no validos")
  fun <- match.fun(f)
  f <- function(x) fun(x, ...)
  x <- x0
  diff <- 1 + tol
  niter <- 0
  num=0
  while (diff > tol && niter <= nmax) {
    num=num+1
    gx <- f(x)
    ggx <- f(gx)
    xnew <- (x*ggx - gx^2) / (ggx - 2*gx + x)
    diff <- abs(x - xnew)
    x <- xnew
    niter <- niter + 1
    cat("Iteración:", num, "Resultado:",x,"\n")
  }
  if (niter > nmax)
    warning("No es posible calcular.")
  return(x)
}
# Funcion   f(x) = cos(x)^2 - x*exp(2)
# Punto con  phi(x) = x + (cos(x) - x*exp(x))/2
phi <- function(x) x + (cos(x)^2 - x^(2))
aitken(phi, 0)  #=> 0.5177574