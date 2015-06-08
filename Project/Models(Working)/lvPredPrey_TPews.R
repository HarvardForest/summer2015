# Load dependencies
library(deSolve)
library(breakpoint)
library(earlywarnings)
library(ggplot2)

lotVpredPrey <- function(time, initState, params){
  # Function for ordinary differential equations (ODE)
  lotVPPeqs <-function(time, initState, params){
    with(as.list(c(initState, params)),{

      # Lotka-Volterra predator-prey model
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
  output <- data.frame(ode(y=initState, times=time, func=lotVPPeqs, parms=params)[,-1])
  print(output)
  return(output)
}

## Test-values
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
initState <- c(prey=500, predator=10)
time <- seq(1, 200, by=1)

## Function call
data <- lotVpredPrey(time, initState, params)
