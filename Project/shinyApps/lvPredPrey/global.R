### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Latest Edit: 6/1/15

# Load dependencies
library(deSolve)
library(breakpoint)

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

  return(output)
}

## Test-values
#params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
#initState <- c(prey=5, predator=1)
#time <- seq(0, 10, by=1)

## Function-call
#data <- lotVpredPrey(time, initState, params)

#### Simulated data example ###
blah <- function(){segs <- 6 # Number of segements
M <- c(1500, 2200, 800, 2500, 1000, 2000) # Segment width
#true.locations <- c(1501, 3701, 4501, 7001, 8001)  # True break-point locations
seg <- NULL
p <- c(0.45, 0.25, 0.4, 0.2, 0.3, 0.6) # Specification of p's for each segment
for(j in 1:segs){
  seg <- c(seg, rnbinom(M[j], size =10, prob = p[j]))
}
simdata <- as.data.frame(seg)
rm(p, M, seg, segs, j)
#plot(data[, 1])

## Not run:
## CE with the four parameter beta distribution ##

obj1 <- CE.NB(simdata, distyp = 1, parallel = TRUE) # Parallel computation

return(obj1)
}
