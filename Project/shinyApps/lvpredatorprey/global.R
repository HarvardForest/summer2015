#### Lotka-Volterra Predator Prey Model
### Nathan Justice
## Last edited: 06July2015

# load dependencies
library(shiny)
library(shinyapps)
library(shinythemes)
library(shinyAce)
library(deSolve)
library(breakpoint)
library(ggplot2)
library(earlywarnings)

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

# transform the values of x so that the range of x is equal to the range of y
rescale <- function(x, y){
    x.range <- range(x)
    y.range <- range(y)
    x <- ((x - x.range[1]) * (diff(y.range))) / diff(x.range) + y.range[1]
    if(any(range(x) != range(y))){
        warning('Ranges do not match.')
    }
    else{
        return(x)
    }
}
