library(deSolve)

lotVmodel <-function(time, initState, params){
  with(as.list(c(initState, params)),{

    # Lotka-Volterra equations
    dx <- (alpha * prey) - (beta * prey * predator)
    dy <- (gamma * prey * predator) - (delta * predator)

    # alpha = the growth rate of prey
    # beta = the rate at which predators kill prey
    # delta = the death rate of predators
    # gamma = the rate at which predators increase by consuming prey

    list(c(dx, dy))
  })
}

params <- c(alpha=1.15, beta=0.01, delta=0.4, gamma=0.001)
initState <- c(prey=500, predator=10)
time <- seq(0, 100, by=.1)

output <- ode(y=initState, times=time, func=lotVmodel, parms=params)
matplot(output[,-1], type="l", xlab="Time", ylab="Population")
legend("topright", c("Prey", "Predator"), lty = c(1, 2), col = c(1, 2), box.lwd = 0)
