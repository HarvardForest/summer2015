# Load dependencies
library(deSolve)
library(breakpoint)
library(earlywarnings)
library(formatR)
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
time <- seq(1, 100, by=1)

## Function call
data <- lotVpredPrey(time, initState, params)

ewsInfo <- qda_ews(data[[1]][2:length(data[[1]])], param=NULL, winsize=50, detrending='gaussian', bandwidth=NULL,
                   boots=50, s_level=0.05, cutoff=0.05, detection.threshold=0.002, grid.size=50,
                   logtransform=FALSE, interpolate=FALSE)

ewsInfo2 <- movpotential_ews(data[[1]][2:length(data[[1]])], param=data[[1]][2:length(data[[1]])])

