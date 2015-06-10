### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Latest Edit: 6/1/15

# Load dependencies
library(deSolve)
library(breakpoint)

lotVpredPrey <- function(time, initState, params){
  # function for ordinary differential equations (ODE)
  lotVPPeqs <-function(time, initState, params){
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
  output <- data.frame(ode(y=initState, times=time, func=lotVPPeqs, parms=params)[,-1])

  return(output)
}

## Test-values
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
initState <- c(prey=5, predator=10)
time <- seq(0, 100, by=1)

## Function-call
data <- lotVpredPrey(time, initState, params)
