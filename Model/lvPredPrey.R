################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 18July2015 ##################################
################################################################################
################################################################################

##### Model Simulation ######

# Load dependencies
library(deSolve)

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
  output <- data.frame(ode(y=initState, times=time, func=lvPredPreyEqs,
                           parms=params)[,-1])

  return(output)
}

## Test-values ##
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
initState <- c(prey=500, predator=10)
time <- seq(1, 100, by=1)

## Function-call ##
data <- lvPredPreyModel(time, initState, params)

library(changepoint)

bp <- cpt.meanvar(data=data[[1]], penalty="None", method="BinSeg", Q=10)

plot(x=time, y=data[[1]], type="l")

abline(v=bp@cpts, col="blue")

decomp <- decompose(ts(data[[1]], frequency=2))

plot(decomp)

bp2 <- cpt.meanvar(data=decomp$random[3:99], penalty="None", method="BinSeg", Q=10)

plot(decomp$random)

abline(v=bp2@cpts, col="blue")

plot(x=time, y=data[[1]], type="l")

abline(v=bp2@cpts, col="blue")

bp3 <- cpt.meanvar(data=decomp$random[3:99], penalty="None", method="AMOC")

plot(x=time, y=data[[1]], type="l")

abline(v=bp3@cpts, col="blue")

bp4 <- cpt.meanvar(data=decomp$random[3:99], penalty="None", method="PELT", Q=10)

plot(x=time, y=data[[1]], type="l")

abline(v=bp4@cpts, col="blue")

bp5 <- cpt.meanvar(data=decomp$random[3:99], penalty="None", method="SegNeigh", Q=10)

plot(x=time, y=data[[1]], type="l")

abline(v=bp5@cpts, col="blue")

bp <- cpt.meanvar(data=data[[1]], penalty="None", method="BinSeg", Q=10)
bp6 <- cpt.meanvar(data=data[[1]], penalty="None", method="AMOC", Q=10)
bp7 <- cpt.meanvar(data=data[[1]], penalty="None", method="PELT", Q=10)
bp8 <- cpt.meanvar(data=data[[1]], penalty="None", method="SegNeigh", Q=10)

plot(x=time, y=data[[1]], type="l")

abline(v=bp@cpts, col="blue")
abline(v=bp6@cpts, col="red")
abline(v=bp7@cpts, col="green")
abline(v=bp8@cpts, col="pink")

library(cpm)

bp9 <- processStream(x=data[[1]], cpmType="GLR")

plot(x=time, y=data[[1]], type="l")

abline(v=bp9$detectionTimes, col="blue")
abline(v=bp9$changePoints, col="red")

# detectionTimes: A vector containing the points in the sequence at which changes were detected,
  # defined as the first observation after which Dt exceeded the test threshold.

# changePoints A vector containing the best estimates of the change point locations, for each
  # detecting change point. If a change is detected after the t
  # the observation, then the
  # change estimate is the value of k which maximises Dk,t.

bp10 <- processStream(x=decomp$random[3:99], cpmType="GLR")

plot(decomp$random)

abline(v=bp10$detectionTimes, col="blue")
abline(v=bp10$changePoints, col="red")

plot(x=time, y=data[[1]], type="l")

abline(v=bp10$detectionTimes, col="blue")
abline(v=bp10$changePoints, col="red")

#########################################
plot(x=time, y=data[[1]], type="l")

abline(v=bp2@cpts, col="red")
abline(v=bp10$detectionTimes, col="blue")

print(bp2@cpts)
print(bp10$detectionTimes)

#########################################

## Test-values ##
params2 <- c(alpha=1.5, beta=0.02, delta=0.04, gamma=0.01)
initState2 <- c(prey=500, predator=10)
time2 <- seq(1, 100, by=1)

## Function-call ##
data2 <- lvPredPreyModel(time2, initState2, params2)

plot(x=time2, y=data2[[1]], type="l")

bp10 <- cpt.meanvar(data=data2[[1]], penalty="None", method="BinSeg", Q=10)

plot(x=time2, y=data2[[1]], type="l")

abline(v=bp10@cpts, col="blue")

decomp2 <- decompose(ts(data2[[1]], frequency=2))

plot(decomp2)

bp11 <- cpt.meanvar(data=data2[[1]], penalty="None", method="BinSeg", Q=10)

plot(decomp2$random)

abline(v=bp11@cpts, col="blue")

plot(x=time2, y=data2[[1]], type="l")

abline(v=bp11@cpts, col="blue")

bp12 <- processStream(x=decomp2$random[3:99], cpmType="GLR")

plot(decomp2$random)

abline(v=bp12$detectionTimes, col="blue")
abline(v=bp12$changePoints, col="red")

plot(x=time, y=data2[[1]], type="l")

abline(v=bp12$detectionTimes, col="blue")
abline(v=bp12$changePoints, col="red")

bp13 <- processStream(x=data2[[1]], cpmType="GLR")

plot(x=time2, y=data2[[1]], type="l")

abline(v=bp13$detectionTimes, col="blue")
abline(v=bp13$changePoints, col="red")
