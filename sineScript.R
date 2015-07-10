library("breakpoint")
library("strucchange")

x <- sin(seq(0, 5*pi, 0.01))

x <- c(x, sin(seq(0, 5*pi, 0.01)) * 3)

x <- c(x, sin(seq(0, 10*pi, 0.01) *2))

plot(x, type="l")

frame_x <- data.frame(x)

tp_x <- CE.Normal(frame_x, Nmax=3)

matplot(frame_x, type="l")
abline(v=tp_x[[2]], col="blue")

dx <- diff(x)

frame_dx <- data.frame(dx)

tp_dx <- CE.Normal(frame_dx, Nmax=3)

matplot(frame_dx, type="l")
abline(v=tp_dx[[2]], col="green")
abline(v=tp_x[[2]], col="blue")

ddx <- diff(dx)

frame_ddx <- data.frame(ddx)

tp_ddx <- CE.Normal(frame_ddx, Nmax=10)

matplot(frame_x, type="l")
abline(v=tp_dx[[2]], col="green")
abline(v=tp_x[[2]], col="blue")
abline(v=tp_ddx[[2]], col="red")

data("Nile")
plot(Nile)

fs.nile <- Fstats(Nile ~ 1)
plot(fs.nile)
lines(breakpoints(fs.nile))

bp.nile <- breakpoints(Nile ~ 1)
summary(bp.nile)
lines(bp.nile)

temp <- Fstats(x ~ 1)
plot(x, type="l")
lines(breakpoints(temp))

test <- seq(50, 500, 1)
test <- c(test, seq(1, 25, 1))

plot(test, type="l")
frame_test <- data.frame(test)

test_BP <- CE.Normal(frame_test, Nmax=10)
abline(v=test_BP[[1]], col="blue")
