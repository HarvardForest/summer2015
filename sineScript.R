x <- sin(seq(0, 5*pi, 0.01))

x <- c(x, sin(seq(0, 5*pi, 0.01)) * 3)

x <- c(x, sin(seq(0, 10*pi, 0.01) *2))

plot(x, type="l")

timeseries <- ts(x, frequency=2)

decomp <- decompose(timeseries)

plot(decomp)

y <- decomp$random

ydf <- data.frame(y)

bp <- CE.Normal(ydf[1])
