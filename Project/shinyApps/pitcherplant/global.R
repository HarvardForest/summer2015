#### Pitcher plant O2 simulation - stack algorithm
### Nathan Justice
## Last edited: 30June2015

# load dependencies
library(shiny)
library(shinyapps)
library(shinyjs)
library(deSolve)
library(breakpoint)
library(ggplot2)
#library(earlywarnings)

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

pitcherPlantSim <- function(days, feedingTime, foodWeight, beta, k, Bscaler,
                            aMax, aMin, s, d, c) {

  ## Function Variables ##

  # days <- 3 # total number of days
  # k <- 1 # carrying capacity
  # feedingTime <- 720 # time at which food is added
  # foodWeight <- 5 # weight of food
  # aMax <- 10 # maximum value of augmentation
  # aMin <- 1 # minimum value of augmentation
  # beta <- 0.0005 # constant
  # s <- 10 # constant
  # d <- 0.5 # constant
  # c <- 100 # constant
  # Bscaler <- 10 # scales biological oxygen demand values
  minute <- vector(mode="numeric") # t/time variable
  x <- vector(mode="numeric") # amount of o2
  a <- vector(mode="numeric") # augmentation function
  P <- vector(mode="numeric") # photosynthesis
  B <- vector(mode="numeric") # biological o2 demand
  n <- vector(mode="numeric") # amount of nutrients
  w <- vector(mode="numeric") # amount of food

  ## Initialization ##

  # simulate photosynthesis as fixed values
  P <- light(days)*PAR(days=days)

  # initial nutrient value
  n <- 0

  # initial augmentation value
  a <- ((aMax-aMin)/(1+exp((-s*n)-d)))+aMin

  # initial biological o2 demand
  B <- 0/(k+0)

  # o2 at minute=0, P=0 b/c unable to index at minute=0
  x <- (a*0)-B

  # simulate until food is first added
  # loop runs until feedingTime-2 b/c food is added AT the minute
  for(i in 1:(feedingTime-2)){
    # augmentation function - default value
    a <- c(a, ((aMax-aMin)/(1+exp((-s*n[i])-d)))+aMin)

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

  for(z in 1:days){
    # add food
    w <- c(w, foodWeight)

    # run simulation for a full day
    for(j in 1:1440){
      # adjust minute
      minute <- c(minute, length(minute)+1)

      # adjust biological o2 demand
      B <- c(B, (w[length(minute)]/(k+w[length(minute)]))*Bscaler)

      # adjust amount of nutrients
      n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

      # adjust augmentation value
      a <- c(a, ((aMax-aMin)/(1+exp((-s*n[length(minute)])-d)))+aMin)

      # adjust o2 amount
      tempO2 <- (a[length(minute)]*P[length(minute)])-B[length(minute)]
      if(is.na(tempO2) == FALSE && tempO2 > 0){
        x <- c(x, tempO2)
      }
      else{
        x <- c(x, 0)
      }

      if(j < 1440){
        # adjust amount of food
        w <- c(w, w[length(minute)]*exp(-beta*(length(minute)+1)))
      }
    }
  }

  # trim objects to appropriate time
  # omitted values aren't relevant
  minute <- minute[1:length(P)]
  B <- B[1:length(P)]
  n <- n[1:length(P)]
  a <- a[1:length(P)]
  x <- x[1:length(P)]
  w <- w[1:length(P)]

  data <- data.frame(minute, x, P[1:length(x)], B, n, a, w)

  colnames(data) <- c("Minute", "Oxygen", "Photosynthesis",
                      "Biological Oxygen Demand", "Nutrients",
                      "Augmentation Value", "Food Amount")

  return(data)
}

################################################################################

    ### Everything below is a local copy of the earlywarnings package ###

# Citation:

# Vasilis Dakos et al. Methods for detecting early warnings of critical
# transitions in time series illustrated using simulated ecological dataPLoS
# One 7(7):e41010, 2012. See
# http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0041010

################################################################################

#' Description: Generic Early Warning Signals
#'
#' \code{generic_ews} is used to estimate statistical moments within rolling windows along a timeserie
#'
#' @author Vasilis Dakos \email{vasilis.dakos@@gmail.com}
#' Author: Vasilis Dakos, January 2, 2012

generic_ews <- function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                 "loess", "linear", "first-diff"), bandwidth = NULL, span = NULL, degree = NULL,
                        logtransform = FALSE, interpolate = FALSE, AR_n = FALSE, powerspectrum = FALSE) {

  # timeseries<-ts(timeseries)
  timeseries <- data.matrix(timeseries)  #strict data-types the input data as tseries object for use in later steps
  if (dim(timeseries)[2] == 1) {
    Y = timeseries
    timeindex = 1:dim(timeseries)[1]
  } else if (dim(timeseries)[2] == 2) {
    Y <- timeseries[, 2]
    timeindex <- timeseries[, 1]
  } else {
    warning("not right format of timeseries input")
  }

  # Interpolation
  if (interpolate) {
    YY <- approx(timeindex, Y, n = length(Y), method = "linear")
    Y <- YY$y
  } else {
    Y <- Y
  }

  # Log-transformation
  if (logtransform) {
    Y <- log(Y + 1)
  }

  # Detrending
  detrending <- match.arg(detrending)
  if (detrending == "gaussian") {
    if (is.null(bandwidth)) {
      bw <- round(bw.nrd0(timeindex))
    } else {
      bw <- round(length(Y) * bandwidth/100)
    }
    smYY <- ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw, range.x = range(timeindex),
                    x.points = timeindex)
    nsmY <- Y - smYY$y
    smY <- smYY$y
  } else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
  } else if (detrending == "loess") {
    if (is.null(span)) {
      span <- 25/100
    } else {
      span <- span/100
    }
    if (is.null(degree)) {
      degree <- 2
    } else {
      degree <- degree
    }
    smYY <- loess(Y ~ timeindex, span = span, degree = degree, normalize = FALSE,
                  family = "gaussian")
    smY <- predict(smYY, data.frame(x = timeindex), se = FALSE)
    nsmY <- Y - smY
  } else if (detrending == "first-diff") {
    nsmY <- diff(Y)
    timeindexdiff <- timeindex[1:(length(timeindex) - 1)]
  } else if (detrending == "no") {
    smY <- Y
    nsmY <- Y
  }

  # Rearrange data for indicator calculation
  mw <- round(length(Y) * winsize/100)
  omw <- length(nsmY) - mw + 1  ##number of moving windows
  low <- 6
  high <- omw
  nMR <- matrix(data = NA, nrow = mw, ncol = omw)
  x1 <- 1:mw
  for (i in 1:omw) {
    Ytw <- nsmY[i:(i + mw - 1)]
    nMR[, i] <- Ytw
  }

  # Calculate indicators
  nARR <- numeric()
  nSD <- numeric()
  nSK <- numeric()
  nKURT <- numeric()
  nACF <- numeric()
  nDENSITYRATIO <- numeric()
  nSPECT <- matrix(0, nrow = omw, ncol = ncol(nMR))
  nCV <- numeric()
  smARall <- numeric()
  smARmaxeig <- numeric()
  detB <- numeric()
  ARn <- numeric()

  nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  for (i in 1:ncol(nMR)) {
    nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
    nARR[i] <- nYR$ar
    # nSD[i]<-sapply(nMR[,i], sd, na.rm = TRUE)#sd(nMR[,i], na.rm = TRUE)
    nSK[i] <- abs(moments::skewness(nMR[, i], na.rm = TRUE))
    nKURT[i] <- moments::kurtosis(nMR[, i], na.rm = TRUE)
    nCV[i] <- nSD[i]/mean(nMR[, i])
    ACF <- acf(nMR[, i], lag.max = 1, type = c("correlation"), plot = FALSE)
    nACF[i] <- ACF$acf[2]
    spectfft <- spec.ar(nMR[, i], n.freq = omw, plot = FALSE, order = 1)
    nSPECT[, i] <- spectfft$spec
    nDENSITYRATIO[i] <- spectfft$spec[low]/spectfft$spec[high]

    if (AR_n) {
      ## RESILIENCE IVES 2003 Indicators based on AR(n)
      ARall <- ar.ols(nMR[, i], aic = TRUE, order.max = 6, demean = F, intercept = F)
      smARall[i] <- ARall$ar[1]
      ARn[i] <- ARall$order
      roots <- Mod(polyroot(c(rev(-ARall$ar), 1)))
      smARmaxeig[i] <- max(roots)
      detB[i] <- (prod(roots))^(2/ARn[i])
    }
  }

  nRETURNRATE = 1/nARR

  # Estimate Kendall trend statistic for indicators
  timevec <- seq(1, length(nARR))
  KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtACF <- cor.test(timevec, nACF, alternative = c("two.sided"), method = c("kendall"),
                    conf.level = 0.95)
  KtSD <- cor.test(timevec, nSD, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtSK <- cor.test(timevec, nSK, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtKU <- cor.test(timevec, nKURT, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtDENSITYRATIO <- cor.test(timevec, nDENSITYRATIO, alternative = c("two.sided"),
                             method = c("kendall"), conf.level = 0.95)
  KtRETURNRATE <- cor.test(timevec, nRETURNRATE, alternative = c("two.sided"),
                           method = c("kendall"), conf.level = 0.95)
  KtCV <- cor.test(timevec, nCV, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)

  # Output
  out <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD, nSK, nKURT, nCV, nRETURNRATE,
                    nDENSITYRATIO, nACF)
  colnames(out) <- c("timeindex", "ar1", "sd", "sk", "kurt", "cv", "returnrate",
                     "densratio", "acf1")
  return(out)
}

plot_generic_ews <- function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                       "loess", "linear", "first-diff"), bandwidth = NULL, span = NULL, degree = NULL,
                              logtransform = FALSE, interpolate = FALSE, AR_n = FALSE, powerspectrum = FALSE) {

  # timeseries<-ts(timeseries)
  timeseries <- data.matrix(timeseries)  #strict data-types the input data as tseries object for use in later steps
  if (dim(timeseries)[2] == 1) {
    Y = timeseries
    timeindex = 1:dim(timeseries)[1]
  } else if (dim(timeseries)[2] == 2) {
    Y <- timeseries[, 2]
    timeindex <- timeseries[, 1]
  } else {
    warning("not right format of timeseries input")
  }

  # Interpolation
  if (interpolate) {
    YY <- approx(timeindex, Y, n = length(Y), method = "linear")
    Y <- YY$y
  } else {
    Y <- Y
  }

  # Log-transformation
  if (logtransform) {
    Y <- log(Y + 1)
  }

  # Detrending
  detrending <- match.arg(detrending)
  if (detrending == "gaussian") {
    if (is.null(bandwidth)) {
      bw <- round(bw.nrd0(timeindex))
    } else {
      bw <- round(length(Y) * bandwidth/100)
    }
    smYY <- ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw, range.x = range(timeindex),
                    x.points = timeindex)
    nsmY <- Y - smYY$y
    smY <- smYY$y
  } else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
  } else if (detrending == "loess") {
    if (is.null(span)) {
      span <- 25/100
    } else {
      span <- span/100
    }
    if (is.null(degree)) {
      degree <- 2
    } else {
      degree <- degree
    }
    smYY <- loess(Y ~ timeindex, span = span, degree = degree, normalize = FALSE,
                  family = "gaussian")
    smY <- predict(smYY, data.frame(x = timeindex), se = FALSE)
    nsmY <- Y - smY
  } else if (detrending == "first-diff") {
    nsmY <- diff(Y)
    timeindexdiff <- timeindex[1:(length(timeindex) - 1)]
  } else if (detrending == "no") {
    smY <- Y
    nsmY <- Y
  }

  # Rearrange data for indicator calculation
  mw <- round(length(Y) * winsize/100)
  omw <- length(nsmY) - mw + 1  ##number of moving windows
  low <- 6
  high <- omw
  nMR <- matrix(data = NA, nrow = mw, ncol = omw)
  x1 <- 1:mw
  for (i in 1:omw) {
    Ytw <- nsmY[i:(i + mw - 1)]
    nMR[, i] <- Ytw
  }

  # Calculate indicators
  nARR <- numeric()
  nSD <- numeric()
  nSK <- numeric()
  nKURT <- numeric()
  nACF <- numeric()
  nDENSITYRATIO <- numeric()
  nSPECT <- matrix(0, nrow = omw, ncol = ncol(nMR))
  nCV <- numeric()
  smARall <- numeric()
  smARmaxeig <- numeric()
  detB <- numeric()
  ARn <- numeric()

  nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  for (i in 1:ncol(nMR)) {
    nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
    nARR[i] <- nYR$ar
    # nSD[i]<-sapply(nMR[,i], sd, na.rm = TRUE)#sd(nMR[,i], na.rm = TRUE)
    nSK[i] <- abs(moments::skewness(nMR[, i], na.rm = TRUE))
    nKURT[i] <- moments::kurtosis(nMR[, i], na.rm = TRUE)
    nCV[i] <- nSD[i]/mean(nMR[, i])
    ACF <- acf(nMR[, i], lag.max = 1, type = c("correlation"), plot = FALSE)
    nACF[i] <- ACF$acf[2]
    spectfft <- spec.ar(nMR[, i], n.freq = omw, plot = FALSE, order = 1)
    nSPECT[, i] <- spectfft$spec
    nDENSITYRATIO[i] <- spectfft$spec[low]/spectfft$spec[high]

    if (AR_n) {
      ## RESILIENCE IVES 2003 Indicators based on AR(n)
      ARall <- ar.ols(nMR[, i], aic = TRUE, order.max = 6, demean = F, intercept = F)
      smARall[i] <- ARall$ar[1]
      ARn[i] <- ARall$order
      roots <- Mod(polyroot(c(rev(-ARall$ar), 1)))
      smARmaxeig[i] <- max(roots)
      detB[i] <- (prod(roots))^(2/ARn[i])
    }
  }

  nRETURNRATE = 1/nARR

  # Estimate Kendall trend statistic for indicators
  timevec <- seq(1, length(nARR))
  KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtACF <- cor.test(timevec, nACF, alternative = c("two.sided"), method = c("kendall"),
                    conf.level = 0.95)
  KtSD <- cor.test(timevec, nSD, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtSK <- cor.test(timevec, nSK, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtKU <- cor.test(timevec, nKURT, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtDENSITYRATIO <- cor.test(timevec, nDENSITYRATIO, alternative = c("two.sided"),
                             method = c("kendall"), conf.level = 0.95)
  KtRETURNRATE <- cor.test(timevec, nRETURNRATE, alternative = c("two.sided"),
                           method = c("kendall"), conf.level = 0.95)
  KtCV <- cor.test(timevec, nCV, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)

  # Plotting Generic Early-Warnings
  #dev.new()
  par(mar = (c(0, 2, 0, 1) + 0), oma = c(7, 2, 3, 1), mfrow = c(5, 2))
  plot(timeindex, Y, type = "l", ylab = "", xlab = "", xaxt = "n", las = 1, xlim = c(timeindex[1],
                                                                                     timeindex[length(timeindex)]))
  if (detrending == "gaussian") {
    lines(timeindex, smY, type = "l", ylab = "", xlab = "", xaxt = "n", col = 2,
          las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  }
  if (detrending == "loess") {
    lines(timeindex, smY, type = "l", ylab = "", xlab = "", xaxt = "n", col = 2,
          las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  }
  if (detrending == "no") {
    plot(c(0, 1), c(0, 1), ylab = "", xlab = "", yaxt = "n", xaxt = "n", type = "n",
         las = 1)
    text(0.5, 0.5, "no residuals - no detrending")
  } else if (detrending == "first-diff") {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindexdiff, nsmY, ylab = "", xlab = "", type = "l", xaxt = "n", las = 1,
         ylim = c(-limit, limit), xlim = c(timeindexdiff[1], timeindexdiff[length(timeindexdiff)]))
    legend("topleft", "first-differenced", bty = "n")
  } else {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindex, nsmY, ylab = "", xlab = "", type = "h", xaxt = "n", las = 1,
         ylim = c(-limit, limit), xlim = c(timeindex[1], timeindex[length(timeindex)]))
    legend("topleft", "residuals", bty = "n")
  }
  plot(timeindex[mw:length(nsmY)], nARR, ylab = "", xlab = "", type = "l", xaxt = "n",
       las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))  #3
  legend("bottomleft", paste("Kendall tau=", round(KtAR$estimate, digits = 3)),
         bty = "n")
  legend("topleft", "ar(1)", bty = "n")
  plot(timeindex[mw:length(nsmY)], nACF, ylab = "", xlab = "", type = "l", xaxt = "n",
       las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))  #4
  legend("bottomleft", paste("Kendall tau=", round(KtACF$estimate, digits = 3)),
         bty = "n")
  legend("topleft", "acf(1)", bty = "n")
  plot(timeindex[mw:length(nsmY)], nRETURNRATE, ylab = "", xlab = "", type = "l",
       xaxt = "n", las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomleft", paste("Kendall tau=", round(KtRETURNRATE$estimate, digits = 3)),
         bty = "n")
  legend("topleft", "return rate", bty = "n")
  plot(timeindex[mw:length(nsmY)], nDENSITYRATIO, ylab = "", xlab = "", type = "l",
       xaxt = "n", las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomleft", paste("Kendall tau=", round(KtDENSITYRATIO$estimate, digits = 3)),
         bty = "n")
  legend("topleft", "density ratio", bty = "n")
  plot(timeindex[mw:length(nsmY)], nSD, ylab = "", xlab = "", type = "l", xaxt = "n",
       las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomleft", paste("Kendall tau=", round(KtSD$estimate, digits = 3)),
         bty = "n")
  legend("topleft", "standard deviation", bty = "n")
  if (detrending == "no") {
    plot(timeindex[mw:length(nsmY)], nCV, ylab = "", xlab = "", type = "l", xaxt = "n",
         las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
    legend("bottomleft", paste("Kendall tau=", round(KtCV$estimate, digits = 3)),
           bty = "n")
    legend("topleft", "coefficient of variation", bty = "n")
  } else {
    plot(0, 0, ylab = "", xlab = "", type = "n", xaxt = "n", yaxt = "n", xlim = c(0,
                                                                                  1), ylim = c(0, 1))
    text(0.5, 0.5, "no coeff var estimated - data detrended")
  }
  plot(timeindex[mw:length(nsmY)], nSK, type = "l", ylab = "", xlab = "", las = 1,
       cex.lab = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("topleft", "skewness", bty = "n")
  legend("bottomleft", paste("Kendall tau=", round(KtSK$estimate, digits = 3)),
         bty = "n")
  mtext("time", side = 1, line = 2, cex = 0.8)
  plot(timeindex[mw:length(nsmY)], nKURT, type = "l", ylab = "", xlab = "", las = 1,
       cex.lab = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("topleft", "kurtosis", bty = "n")
  legend("bottomleft", paste("Kendall tau=", round(KtKU$estimate, digits = 3)),
         bty = "n")
  mtext("time", side = 1, line = 2, cex = 0.8)
  mtext("Generic Early-Warnings", side = 3, line = 0.2, outer = TRUE)  #outer=TRUE print on the outer margin

  # Resilience Estimators based on AR(n)
  if (AR_n) {
    #dev.new()
    par(mar = (c(1, 2, 0, 1) + 0.2), oma = c(4, 2, 3, 1), mfrow = c(2, 2))
    plot(timeindex[mw:length(nsmY)], ARn, type = "p", ylab = "", xlab = "", xaxt = "n",
         cex = 0.1, las = 1, cex.axis = 0.8, xlim = c(timeindex[1], timeindex[length(timeindex)]))  #10
    legend("topleft", "AR(n)", bty = "n")
    plot(timeindex[mw:length(nsmY)], smARmaxeig, type = "l", ylab = "", xlab = "",
         xaxt = "n", las = 1, cex.axis = 0.8, xlim = c(timeindex[1], timeindex[length(timeindex)]))
    legend("topleft", "max eigenvalue", bty = "n")
    plot(timeindex[mw:length(nsmY)], detB, type = "l", ylab = "", xlab = "",
         cex.lab = 1, las = 1, cex.axis = 0.8, xlim = c(timeindex[1], timeindex[length(timeindex)]))
    mtext("time", side = 1, line = 2, cex = 0.8)
    legend("topleft", "geometric mean root AR(n)", bty = "n")
    plot(timeindex[mw:length(nsmY)], smARall, type = "l", ylab = "", xlab = "",
         cex.lab = 1, las = 1, cex.axis = 0.8, xlim = c(timeindex[1], timeindex[length(timeindex)]))
    mtext("time", side = 1, line = 2, cex = 0.8)
    legend("topleft", "b1 of AR(n)", bty = "n")
    mtext("Resilience Estimators based on AR(n)", side = 3, line = 0.2, outer = TRUE)
  }

  #Power spectrum
  if (powerspectrum) {
    #dev.new()
    par(mar = (c(4.6, 4, 0.5, 2) + 0.2), oma = c(0.5, 1, 2, 1))
    image(x = (spectfft$freq[2:length(spectfft$freq)]), y = (seq(1, ncol(nSPECT),
                                                                 by = 1)), log(nSPECT[2:length(spectfft$freq), ]), ylab = "rolling window",
          xlab = "frequency", log = "x", xlim = c(spectfft$freq[2], spectfft$freq[length(spectfft$freq)]),
          col = topo.colors(20), xaxs = "i")
    contour(x = (spectfft$freq[2:length(spectfft$freq)]), y = (seq(1, ncol(nSPECT),
                                                                   by = 1)), log(nSPECT[2:length(spectfft$freq), ]), add = TRUE)
    mtext("Power spectrum within rolling windows", side = 3, line = 0.2, outer = TRUE)
  }

  # Output
  out <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD, nSK, nKURT, nCV, nRETURNRATE,
                    nDENSITYRATIO, nACF)
  colnames(out) <- c("timeindex", "ar1", "sd", "sk", "kurt", "cv", "returnrate",
                     "densratio", "acf1")

  #return(out)
}
