library("breakpoint")

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
