#### Pitcher plant O2 simulation - stack algorithm
### Nathan Justice
## Last edited: 08June2015

# 6:00 sunrise = 360
# 12:00 noon = 720
# 18:00 sunset = 1080

## Variables ##

minute <- 0 # t/time variable
days <- 10 # total number of days
totalMinutes <- days*1440 # total number of minutes
k <- 1 # carrying capacity
food <- FALSE # presence of food
feedingTime <- 720 # time at which food is added
aMax <- 10 # maximum value of augmentation
aMin <- 1 # minimum value of augmentation
beta <- 0.0005 # constant
s <- 1 # constant
d <- 0.5 # constant
c <- 1 # constant
x <- vector(mode="numeric") # amount of o2
a <- vector(mode="numeric") # augmentation function
P <- vector(mode="numeric") # photosynthesis
B <- vector(mode="numeric") # biological o2 demand
n <- vector(mode="numeric") # amount of nutrients
w <- vector(mode="numeric") # amount of food

## Functions ##

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

## Initialization ##

# simulate photosynthesis as fixed values
P <- light(days)*PAR(days=days)

# initial nutrient value
n <- 0

# initial augmentation value
a <- (aMax-aMin)/(1+exp(s*n*d))

# initial amount of food
w <- 0

# initial biological o2 demand
B <- 0/(k+0)

# o2 at minute=0, P=0 b/c unable to index at minute=0
x <- (a*0)-B

# simulate until food is first added (feedingTime=720)
for(i in 1:length(feedingTime)){

}
