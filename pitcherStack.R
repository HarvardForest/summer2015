#### Pitcher plant O2 simulation - stack algorithm
### Nathan Justice
## Last edited: 08June2015

# 6:00 sunrise = 360
# 12:00 noon = 720
# 18:00 sunset = 1080

## Variables ##

days <- 10 # total number of days
totalMinutes <- days*1440 # total number of minutes
k <- 1 # carrying capacity
food <- FALSE # presence of food
feedingTime <- 720 # time at which food is added
foodWeight <- 5 # weight of food
aMax <- 10 # maximum value of augmentation
aMin <- 1 # minimum value of augmentation
beta <- 0.0005 # constant
s <- 10 # constant
d <- 0.5 # constant
c <- 100 # constant
minute <- vector(mode="numeric") # t/time variable
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

# initial biological o2 demand
B <- 0/(k+0)

# o2 at minute=0, P=0 b/c unable to index at minute=0
x <- (a*0)-B

# simulate until food is first added (feedingTime=720)
# loop runs until feedingTime-2 b/c food is added AT minute=720
for(i in 1:(feedingTime-2)){
  # augmentation function - default value
  a <- c(a, (aMax-aMin)/(1+exp(-s*n[i]*d)))

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

# add food
food <- TRUE
w <- c(w, foodWeight)

# run simulation for a full day
for(j in 1:1439){
  # adjust minute
  minute <- c(minute, length(minute)+1)

  # adjust biological o2 demand
  B <- c(B, w[length(minute)]/(k+w[length(minute)]))

  # adjust amount of nutrients
  n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

  # adjust augmentation value
  a <- c(a, ((aMax-aMin)/(1+exp(-(s*n[length(minute)]-d))+aMin)))

  x <- c(x, (a[length(minute)]*P[length(minute)])-B[length(minute)])

  if(j < 1439){
    # adjust amount of food
    w <- c(w, w[length(minute)]*exp(-beta*length(minute)))
  }
}

data <- data.frame(minute, x, P[1:length(x)], B, n, a, w)
pairs(data)
