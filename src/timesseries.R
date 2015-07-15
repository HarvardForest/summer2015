### Time Series analyses

source('~/projects/HF/summer2015/Project/Models(Done)/pme.R')

sim0 <- pitcherPlantSim(10,Bscaler=1,foodWeight=0)
sim1 <- pitcherPlantSim(10,Bscaler=1)
sim10 <- pitcherPlantSim(10,Bscaler=10,foodWeight=c(rep(0,5),rep(5,5)))
o2.0 <- sim0$Oxygen
o2.1 <- sim1$Oxygen
o2.10 <- sim10$Oxygen

dec.0 <- decompose(ts(o2.0,freq=1440))
dec.1 <- decompose(ts(o2.1,freq=1440))
dec.10 <- decompose(ts(o2.10,freq=1440))

plot(dec.0)
plot(dec.1)
plot(dec.10)


