#### Pitcher plant O2 simulation - stack-based
### Nathan Justice
## Last edited: 03June2015

## Variables and functions
time = 0
time_max = 5
x <- vector(mode="numeric")
B <- vector(mode="numeric")
P <- vector(mode="numeric")
k <- 1
w <- vector(mode="numeric")
food <- FALSE
beta <- 0.0005
a <- vector(mode="numeric")
n <- vector(mode="numeric")
aMax <- 10
aMin <- 1
s <- 1
d <- 0.5
C <- 1

light <- function(){
  out <- sin(2*pi*(1:720)/1440)
  out[out < 0] <- 0
  out <- c(rep(0,720/2),out,rep(0,720/2))
  rep(out)
}

PAR <- function(rise=6,set=18){
  out <- rep(0,1440)
  out[(rise*60):(set*60)] <- 1
  rep(out)
}

P <- light() * PAR()

update_x_NF <- function(){
  get_B()
  x <<- c(x, (-(B[time]) + P[time]))
}

get_B <- function(){
  get_w()
  B <<- c(B, w[time]/(k + w[time]))
}

get_w <- function(){
  if (food == FALSE){
    w <<- c(w, 0)
  }
  else {
    w <<- c(w, w[time-1] * exp(beta * time))
  }
}

update_x <- function(){
  get_B()
  get_a()
  x <<- c(x, (-(B[time]) + (P[time] * a[time])))
}

get_a <- function(){
  get_n()
  a <<- c(a, ((aMax - aMin)/1 + exp(s * d * n[time])))
}

get_n <- function(){
  n <<- c(n, ((w[time] * x[time - 1]) / C))
}

## Initialization

update_x_NF()
time <- time + 1
update_x_NF()
time <- time + 1
update_x_NF()
time <- time + 1
print(time)
food <- TRUE
print(food)
w <- c(w, 1)

update_x()
#update_w()
time <- time + 1
update_x()
#update_w()
time <- time + 1
update_x()
#update_w()

## Write single update_x() function, a is 1 (not 0), until food is added, then it is changed
# a is initialized to 1 to reflect solely photosynthesis until food is added
