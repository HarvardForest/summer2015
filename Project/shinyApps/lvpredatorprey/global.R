################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 20July2015 ##################################
################################################################################
################################################################################

###### Global dependencies and function(s) ######

# load dependencies
library(shiny)
library(shinyapps)
library(shinythemes)
library(shinyAce)
library(deSolve)
library(breakpoint)
library(earlywarnings)
library(plotrix)

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
