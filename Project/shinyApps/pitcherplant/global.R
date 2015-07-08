#### Pitcher plant O2 simulation - stack algorithm
### Nathan Justice
## Last edited: 07July2015

# load dependencies
library(shiny)
library(shinyapps)
library(shinythemes)
library(shinyAce)
library(deSolve)
library(breakpoint)
library(ggplot2)
library(earlywarnings)

## Functions ##

# transform the values of x so that the range of x is equal to the range of y
rescale <- function(x,y){
    x.range <- range(x)
    y.range <- range(y)
    x <- ((x - x.range[1])* (diff(y.range))) / diff(x.range) + y.range[1]
    if (any(range(x) != range(y))){
        warning('Ranges do not match.')
    }else{
        return(x)
    }
}

light <- function(days){
  out <- sin(2*pi*(1:720)/1440)
  out[out < 0] <- 0
  out <- c(rep(0,720/2), out, rep(0,720/2))
  rep(out, days)
}

PAR <- function(days, rise=6, set=18){
  out <- rep(0, 1440)
  out[(rise*60):(set*60)] <- 1
  rep(out, days)
}

pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=5, beta=0.005, k=1, Bscaler=1,
                            aMax=10, aMin=1, s=10, d=0.5, c=100) {

  minute <- vector(mode="numeric") # t/time variable
  x <- vector(mode="numeric") # amount of o2
  a <- vector(mode="numeric") # augmentation function
  P <- vector(mode="numeric") # photosynthesis
  B <- vector(mode="numeric") # biological o2 demand
  n <- vector(mode="numeric") # amount of nutrients
  w <- vector(mode="numeric") # amount of food


  ## Initialization ##

  # simulate photosynthesis as fixed values
  P <- light(days)*PAR(days=days)

  # initial nutrient value
  n <- 0

  # initial augmentation value
  a <- ((aMax-aMin)/(1+exp((-s*n)-d)))+aMin

  # initial biological o2 demand
  B <- 0/(k+0)

  # o2 at minute=0, P=0 b/c unable to index at minute=0
  x <- (a*0)-B

  # simulate until food is first added
  # loop runs until feedingTime-2 b/c food is added AT the minute
  for(i in 1:(feedingTime-2)){
    # augmentation function - default value
    a <- c(a, ((aMax-aMin)/(1+exp((-s*n[i])-d)))+aMin)

    # biological oxygen demand - default value (no food = no microbes)
    B <- c(B, 0/(k+0))

    # calculate o2 amount - product of photosynthesis alone (no food)
    x <- c(x, (a[i]*P[i])-B[i])

    # amount of food - no food
    w <- c(w, 0)

    # amount of nutrients - no nutrients
    n <- c(n, 0)

    # adjust minute
    minute <- c(minute, i)
  }

  # adjust minute
  minute <- c(minute, length(minute)+1)

  # adjust amount of food
  w <- c(w, w[length(w)])

  for(z in 1:days){
    # add food
    w <- c(w, w[length(w)]+foodWeight)

    # run simulation for a full day
    for(j in 1:1440){
      # adjust minute
      minute <- c(minute, length(minute)+1)

      # adjust biological o2 demand
      B <- c(B, (w[length(minute)]/(k+w[length(minute)]))*Bscaler)

      # adjust amount of nutrients
      n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

      # adjust augmentation value
      a <- c(a, ((aMax-aMin)/(1+exp((-s*n[length(minute)])-d)))+aMin)

      # adjust o2 amount
      tempO2 <- (a[length(minute)]*P[length(minute)])-B[length(minute)]
      if(is.na(tempO2) == FALSE && tempO2 > 0){
        x <- c(x, tempO2)
      }
      else{
        x <- c(x, 0)
      }

      if(j < 1440){
          ## adjust amount of food
          w <- c(w, w[length(w)]*exp(-beta*(1)))
      }
    }
  }

  # trim objects to appropriate time
    # omitted values aren't relevant
  minute <- minute[1:length(P)]
  B <- B[1:length(P)]
  n <- n[1:length(P)]
  a <- a[1:length(P)]
  x <- x[1:length(P)]
  w <- w[1:length(P)]

  data <- data.frame(minute, x, P[1:length(x)], B, n, a, w)
  colnames(data) <- c("Minute", "Oxygen", "Photosynthesis",
                      "Biological Oxygen Demand", "Nutrients",
                      "Augmentation Value", "Food Amount")
  return(data)

}
