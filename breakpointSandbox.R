library("deSolve")
library("cumSeg")

lvPredPreyModel <- function(time, initState, params){
  # function for ordinary differential equations (ODE)
  lvPredPreyEqs <-function(time, initState, params){
    with(as.list(c(initState, params)),{

      # lotka-Volterra predator-prey model
      dx <- (alpha * prey) - (beta * prey * predator)
      dy <- (gamma * prey * predator) - (delta * predator)

      # alpha = the growth rate of prey
      # beta = the rate at which predators kill prey
      # delta = the death rate of predators
      # gamma = the rate at which predators increase by consuming prey

      list(c(dx, dy))
    })
  }

  # deSolve method to solve initial value problems (IVP)
  output <- data.frame(ode(y=initState, times=time, func=lvPredPreyEqs, parms=params)[,-1])

  return(output)
}

## Test-values
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
initState <- c(prey=500, predator=10)
time <- seq(1, 1000, by=1)

## Function-call
data <- lvPredPreyModel(time, initState, params)

plot(time, data[[1]], type="l")

m <- jumpoints(y=data[[1]], output=1)
abline(v=m$psi, col="blue")
n <- jumpoints(y=data[[2]], output=1)
abline(v=n$psi, col="green")

foo <- mean(data[[1]])
abline(h=foo, col="blue")
