source('pme.R')

days <- 15
foodWeight <- c(rep(5,5),rep(0,5),rep(5,5))
plot(foodWeight)

sim0 <- pitcherPlantSim(days,foodWeight=foodWeight*0,Bscaler=10,aMax=2)
sim1 <- pitcherPlantSim(days,foodWeight=foodWeight,beta=0.01,Bscaler=1,aMax=2)
sim1.det <- sim1$Oxygen-sim0$Oxygen

k.lag <- 90

par(mfrow=c(1,2))
plot(sim0$Oxygen,type='p',ylim=range(c(sim0$Oxygen,sim1$Oxygen,sim1.det)),col='darkgrey',cex=0.25)
points(sim1$Oxygen,col='dodgerblue',cex=0.5)
points(sim1.det,cex=0.5,col=rainbow(length(sim0$Oxygen)))
plot(sim1.det[k.lag:length(sim1.det)]~sim1.det[1:(length(sim1.det)-(k.lag-1))],cex=0.50,col=rainbow(length(sim1.det)))
