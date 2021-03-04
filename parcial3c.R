library(pracma)

f <- function(x) log(1+x)
p <- taylor(f, 0, 4)
p           

x <- seq(-0.5, 0.5, length.out=100)
yf <- f(x)
yp <- polyval(p, x)
plot(x, yf, type = "l", col = "gray", lwd = 3)
lines(x, yp, col = "red")
grid()