
require(deSolve)
##################################################################

###MODELO SIR MEGA CARGADO

##################################################################

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    b = beta*(1-I/N)^n 
    dJ <-  b * J * (N-J)
    dR <-  gamma * I
    dI <-  dJ-dR
    
    return(list(c(dJ, dI,dR)))
  })
}

init       <- c(J = 2200, I = 2200, R = 0)
parameters <- c(beta = .8/7e6, gamma = 0.003, N = 7e6, n = 3)

times      <- seq(0, 70, by = 1)

##metodo 1

## Solve using ode (General Solver for Ordinary Differential Equations)
out1 = ode(y = init, times = times, func = sir, parms = parameters, method = "rk4")
out1 <- as.data.frame(out1)
out1$time <- NULL
head(out1, 25)

#plot de infectados metodo 1
plot(times,out1$J,type = "l",col="blue",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de infectados")

#plot de infecciosos metodo 1
plot(times,out1$I,type = "l",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de infecciosos")

#plot de recuperados metodo 1
plot(times,out1$R,type = "l",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de recuperados")

#plot grafica general metodo 1
plot(times, out1$J,
     main="Modelo SIR Ebola en Sierra Leona (Noviembre 2014) por metodo de rk4",
     ylab="Poblacion",
     type="l",
     col="blue")
lines(times,out1$I, col="red")
lines(times,out1$R, col="green")

legend("topleft",
       c("Infected","Ifecciosos","Recuperados"),
       fill=c("blue","red","green")
)

##metodo 2

## Solve using ode (General Solver for Ordinary Differential Equations)
out2 = ode(y = init, times = times, func = sir, parms = parameters, method = "euler")
out2 <- as.data.frame(out2)
out2$time <- NULL
head(out2, 25)

#plot de infectados metodo 2
plot(times,out2$J,type = "l",col="blue",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de infectados")

#plot de infecciosos metodo 2
plot(times,out2$I,type = "l",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de infecciosos")

#plot de recuperados metodo 2
plot(times,out2$R,type = "l",lwd = 2,main="Modelo SIR (Susceptible-infectados-Recuperados)\nCRv2(Code red 1)\nJulio 19 de 2001",xlab= "t(horas)",ylab = "# de recuperados")

#plot grafica general metodo 2
plot(times, out2$J,
     main="Modelo SIR Ebola en Sierra leona (Noviembre 2014) por metodo de Euler",
     ylab="Población",
     type="l",
     col="blue")
lines(times,out2$I, col="red")
lines(times,out2$R, col="green")

legend("topleft",
       c("Infected","Ifecciosos","Recuperados"),
       fill=c("blue","red","green")
)