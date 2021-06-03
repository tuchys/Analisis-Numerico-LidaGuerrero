#Lotka-Volterra

#El efecto Volterra.
#La siguiente tabla muestra el índice de capturas (en miles) de linces y conejos elaborada por la compañía Hudson Bay entre los años 1901 y 1922.

#conejos = [30, 47.2, 70.2, 77.3 , 36.3, 20.6, 18.1, 21.4, 22, 25.4, 27.1, 40.3, 57, 76.5 , 52.3, 19.5, 11.2, 7.6, 14.6, 16.2, 24.7];
#linces = [4, 6.1, 9.9, 35.2, 59.4, 41.7, 19, 13, 8.4, 9.1, 7.4, 8, 12.3, 19.5, 45.7, 51.1, 29.7, 15.8, 9.6, 10.1, 8.6]; 

#dN/dt = r * N - a * P * N
#dP/dt = f * P * N - b * P
#donde
#N: presa
#P: depredador
#r: tasa de crecimiento de N
#a: tasa de pérdida de N
#f: tasa de crecimiento de P
#b: tasa de pérdida de P

library(tidyverse)
library(deSolve)


# parameters
pars <- c(alpha = 1, beta = 0.2, delta = 0.5, gamma = 0.2)
# initial state 
init <- c(x = 1, y = 2)
# times
times <- seq(0, 100, by = 1)


deriv <- function(t, state, pars) {
  with(as.list(c(state, pars)), {
    d_x <- alpha * x - beta * x * y
    d_y <- delta * beta * x * y - gamma * y
    return(list(c(x = d_x, y = d_y)))
  })
}
lv_results <- ode(init, times, deriv, pars)

lv_model <- function(pars, times = seq(0, 50, by = 1)) {
  # initial state 
  state <- c(x = 1, y = 2)
  # derivative
  deriv <- function(t, state, pars) {
    with(as.list(c(state, pars)), {
      d_x <- alpha * x - beta * x * y
      d_y <- delta * beta * x * y - gamma * y
      return(list(c(x = d_x, y = d_y)))
    })
  }
  # solve
  ode(y = state, times = times, func = deriv, parms = pars)
}
lv_results <- lv_model(pars = pars, times = seq(0, 50, by = 0.25))

lv_results %>% 
  data.frame() %>% 
  gather(var, pop, -time) %>% 
  mutate(var = if_else(var == "x", "Presa", "Depredador")) %>% 
  ggplot(aes(x = time, y = pop)) +
  geom_line(aes(color = var)) +
  scale_color_brewer(NULL, palette = "Set1") +
  labs(title = "Modelo Lotka-Volterra depredador presa",
       subtitle = paste(names(pars), pars, sep = " = ", collapse = "; "),
       x = "Tiempo", y = "Densidad poblacional")