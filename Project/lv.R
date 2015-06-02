# Load dependencies
library(deSolve)

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
  output <- ode(y=initState, times=time, func=lotVPPeqs, parms=params)
  # Plot matrix
  matplot(output[,-1], type="l", xlab="Time", ylab="Population")
  legend("topleft", c("Prey", "Predator"), lty=c(1, 2), col=c(1, 2),
         bty="n")
}

# Test-values
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
initState <- c(prey=500, predator=10)
time <- seq(0, 100, by=1)

# Function-call
lotVpredPrey(time, initState, params)
