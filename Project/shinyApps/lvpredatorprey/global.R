### load dependencies ###
library(shiny)
library(shinyapps)
library(shinyjs)
library(deSolve)
library(breakpoint)
library(ggplot2)
#library(earlywarnings)

### start: the main model ###

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
  output <- data.frame(ode(y=initState, times=time, func=lvPredPreyEqs, parms=params)[,-1])

  return(output)
}

### end: the main model ###

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
# Details:
#' see ref below
#'
# Arguments:
#'    @param timeseries a numeric vector of the observed univariate timeseries values or a numeric matrix where the first column represents the time index and the second the observed timeseries values. Use vectors/matrices with headings. If the powerspectrum is to be plotted as well, the timeseries lenght should be even number.
#'    @param winsize is the size of the rolling window expressed as percentage of the timeseries length (must be numeric between 0 and 100). Default is 50\%.
#'    @param bandwidth is the bandwidth used for the Gaussian kernel when gaussian filtering is applied. It is expressed as percentage of the timeseries length (must be numeric between 0 and 100). Alternatively it can be given by the bandwidth selector \code{\link{bw.nrd0}} (Default).
#'    @param detrending the timeseries can be detrended/filtered prior to analysis. There are four options: \code{gaussian} filtering, \code{loess} fitting, \code{linear} detrending and \code{first-differencing}. Default is \code{no} detrending.
#'    @param span parameter that controls the degree of smoothing (numeric between 0 and 100, Default 25). see more on loess{stats}
#'    @param degree the degree of polynomial to be used for when loess fitting is applied, normally 1 or 2 (Default). see more on loess{stats}
#'    @param logtransform logical. If TRUE data are logtransformed prior to analysis as log(X+1). Default is FALSE.
#'    @param interpolate logical. If TRUE linear interpolation is applied to produce a timeseries of equal length as the original. Default is FALSE (assumes there are no gaps in the timeseries).
#'    @param AR_n logical. If TRUE the best fitted AR(n) model is fitted to the data. Default is FALSE.
#'    @param powerspectrum logical. If TRUE the power spectrum within each rolling window is plotted. Default is FALSE.
#'
# Returns:
#'   @return \code{generic_ews} returns a matrix that contains:
#'   @return \item{tim}{the time index.}
#'   @return \item{ar1}{the \code{autoregressive coefficient ar(1)} of a first order AR model fitted on the data within the rolling window.}
#'   @return \item{sd}{the \code{standard deviation} of the data estimated within each rolling window.}
#'   @return \item{sk}{the \code{skewness} of the data estimated within each rolling window.}
#'   @return \item{kurt}{the \code{kurtosis} of the data estimated within each rolling window.}
#'   @return \item{cv}{the \code{coefficient of variation} of the data estimated within each rolling window.}
#'   @return \item{returnrate}{the return rate of the data estimated as \code{1-ar(1)} cofficient within each rolling window.}
#'   @return \item{densratio}{the \code{density ratio} of the power spectrum of the data estimated as the ratio of low frequencies over high frequencies within each rolling window.}
#'   @return \item{acf1}{the \code{autocorrelation at first lag} of the data estimated within each rolling window.}
#'
#' In addition, \code{generic_ews} returns three plots. The first plot contains the original data, the detrending/filtering applied and the residuals (if selected), and all the moment statistics. For each statistic trends are estimated by the nonparametric Kendall tau correlation.  The second plot, if asked, quantifies resilience indicators fitting AR(n) selected by the Akaike Information Criterion. The third plot, if asked, is the power spectrum estimated by \code{\link{spec.ar}} for all frequencies within each rolling window.
#'
#' @export
#'
#' @author Vasilis Dakos \email{vasilis.dakos@@gmail.com}
#' @references Ives, A. R. (1995). 'Measuring resilience in stochastic systems.' \emph{Ecological Monographs} 65: 217-233
#'
#' Dakos, V., et al (2008). 'Slowing down as an early warning signal for abrupt climate change.' \emph{Proceedings of the National Academy of Sciences} 105(38): 14308-14312
#'
#' Dakos, V., et al (2012).'Methods for Detecting Early Warnings of Critical Transitions in Time Series Illustrated Using Simulated Ecological Data.' \emph{PLoS ONE} 7(7): e41010. doi:10.1371/journal.pone.0041010
#' @seealso \code{\link{generic_ews}}; \code{\link{ddjnonparam_ews}}; \code{\link{bdstest_ews}}; \code{\link{sensitivity_ews}}; \code{\link{surrogates_ews}}; \code{\link{ch_ews}}; \code{\link{movpotential_ews}}; \code{\link{livpotential_ews}};
#'
#' @importFrom moments skewness
#' @importFrom moments kurtosis
#'
#' @examples
#' data(foldbif)
#' out=generic_ews(foldbif,winsize=50,detrending='gaussian',
#' bandwidth=5,logtransform=FALSE,interpolate=FALSE)
#' @keywords early-warning

# Author: Vasilis Dakos, January 2, 2012

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

#' Description: Quick Detection Analysis for Generic Early Warning Signals
#'
#' \code{qda_ews} is used to estimate autocorrelation, variance within rolling windows along a timeseries, test the significance of their trends, and reconstruct the potential landscape of the timeseries
#'
# Details:
#' see ref below
#'
#' Arguments:
#'    @param timeseries a numeric vector of the observed univariate timeseries values or a numeric matrix where the first column represents the time index and the second the observed timeseries values. Use vectors/matrices with headings.
#'    @param param values corresponding to observations in timeseries
#'    @param winsize is the size of the rolling window expressed as percentage of the timeseries length (must be numeric between 0 and 100). Default is 50\%.
#'    @param detrending the timeseries can be detrended/filtered prior to analysis. There are four options: \code{gaussian} filtering, \code{linear} detrending and \code{first-differencing}. Default is \code{no} detrending.
#'    @param bandwidth is the bandwidth used for the Gaussian kernel when gaussian filtering is applied. It is expressed as percentage of the timeseries length (must be numeric between 0 and 100). Alternatively it can be given by the bandwidth selector \code{\link{bw.nrd0}} (Default).
# @param incrwinsize increments the rolling window size (must be numeric between
# 0 and 100). Default is 25.
#'    @param boots the number of surrogate data to generate from fitting an ARMA(p,q) model. Default is 100.
#'    @param s_level significance level. Default is 0.05.
# @param incrbandwidth is the size to increment the bandwidth used for the
# Gaussian kernel when gaussian filtering is applied. It is expressed as
# percentage of the timeseries length (must be numeric between 0 and 100).
# Default is 20.
#'    @param cutoff the cutoff value to visualize the potential landscape
#'    @param detection.threshold detection threshold for potential minima
#'    @param grid.size grid size (for potential analysis)
#'    @param logtransform logical. If TRUE data are logtransformed prior to analysis as log(X+1). Default is FALSE.
#'    @param interpolate logical. If TRUE linear interpolation is applied to produce a timeseries of equal length as the original. Default is FALSE (assumes there are no gaps in the timeseries).
#'
# Returns:
#' \code{qda_ews} produces three plots. The first plot contains the original data, the detrending/filtering applied and the residuals (if selected), autocorrelation and variance. For each statistic trends are estimated by the nonparametric Kendall tau correlation.  The second plot, returns a histogram of the distributions of the Kendall trend statistic for autocorrelation and variance estimated on the surrogated data. Vertical lines represent the level of significance, whereas the black dots the actual trend found in the time series. The third plot is the reconstructed potential landscape in 2D. In addition, the function returns a list containing the output from the respective functions generic_RShiny (indicators); surrogates_RShiny (trends); movpotential_ews (potential analysis)
#'
#' @export
#'
#' @author Vasilis Dakos, Leo Lahti, March 1, 2013 \email{vasilis.dakos@@gmail.com}
#' @references
#' Dakos, V., et al (2012).'Methods for Detecting Early Warnings of Critical Transitions in Time Series Illustrated Using Simulated Ecological Data.' \emph{PLoS ONE} 7(7): e41010. doi:10.1371/journal.pone.0041010
#' @seealso \code{\link{generic_ews}}; \code{\link{ddjnonparam_ews}}; \code{\link{bdstest_ews}}; \code{\link{sensitivity_ews}}; \code{\link{surrogates_ews}}; \code{\link{ch_ews}}; \code{\link{movpotential_ews}}; \code{\link{livpotential_ews}};
#'
#' @examples
#' data(foldbif)
#' out <- qda_ews(foldbif, param = NULL, winsize = 50,
#'                   detrending='gaussian', bandwidth=NULL,
#'  	  boots = 50, s_level = 0.05, cutoff=0.05,
#'		  detection.threshold = 0.002, grid.size = 50,
#'		  logtransform=FALSE, interpolate=FALSE)
#' @keywords early-warning

qda_ews <- function(timeseries, param = NULL, winsize = 50, detrending = c("no",
                                                                           "gaussian", "linear", "first-diff"), bandwidth = NULL, boots = 100, s_level = 0.05,
                    cutoff = 0.05, detection.threshold = 0.002, grid.size = 50, logtransform = FALSE,
                    interpolate = FALSE) {

  timeseries <- data.matrix(timeseries)
  message("Indicator trend analysis")
  g <- generic_RShiny(timeseries, winsize, detrending, bandwidth, logtransform,
                      interpolate, AR_n = FALSE, powerspectrum = FALSE)

  message("Trend significance analysis")
  dev.new()
  s <- surrogates_RShiny(timeseries, winsize, detrending, bandwidth, boots, s_level,
                         logtransform, interpolate)

  message("Potential analysis")
  p <- movpotential_ews(as.vector(timeseries[, 1]), param, detection.threshold = detection.threshold,
                        grid.size = grid.size, plot.cutoff = cutoff)
  dev.new()

  list(indicators = g, trends = s, potential.plot = p)

}

# generic_Rshiny for estimating only AR1 and Variance in moving windows with
# various options for pretreating the data 26 Feb 2013

generic_RShiny <- function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                    "linear", "first-diff"), bandwidth, logtransform, interpolate, AR_n = FALSE,
                           powerspectrum = FALSE) {

  # timeseries<-ts(timeseries)
  timeseries <- data.matrix(timeseries)  #strict data-types the input data as tseries object for use in later steps
  if (ncol(timeseries) == 1) {
    Y = timeseries
    timeindex = 1:dim(timeseries)[1]
  } else if (dim(timeseries)[2] == 2) {
    Y <- timeseries[, 2]
    timeindex <- timeseries[, 1]
  } else {
    warning("not right format of timeseries input")
  }
  # return(timeindex) Interpolation
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
    if (timeindex[1] > timeindex[length(timeindex)]) {
      nsmY <- Y - rev(smYY$y)
      smY <- rev(smYY$y)
    } else {
      nsmY <- Y - smYY$y
      smY <- smYY$y
    }
  } else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
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

  nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  for (i in 1:ncol(nMR)) {
    nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
    nARR[i] <- nYR$ar
  }

  nVAR = sqrt(nSD)

  # Estimate Kendall trend statistic for indicators
  timevec <- seq(1, length(nARR))
  KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtVAR <- cor.test(timevec, nVAR, alternative = c("two.sided"), method = c("kendall"),
                    conf.level = 0.95)

  # Plotting Generic Early-Warnings dev.new()
  par(mar = (c(1, 2, 0.5, 2) + 0), oma = c(2, 2, 2, 2), mfrow = c(4, 1))
  plot(timeindex, Y, type = "l", ylab = "", xlab = "", xaxt = "n", lwd = 2, las = 1,
       xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomleft", "data", , bty = "n")
  if (detrending == "gaussian") {
    lines(timeindex, smY, type = "l", ylab = "", xlab = "", xaxt = "n", lwd = 2,
          col = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  }
  if (detrending == "no") {
    plot(c(0, 1), c(0, 1), ylab = "", xlab = "", yaxt = "n", xaxt = "n", type = "n",
         las = 1)
    text(0.5, 0.5, "no detrending - no residuals")
  } else if (detrending == "first-diff") {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindexdiff, nsmY, ylab = "", xlab = "", type = "l", xaxt = "n", lwd = 2,
         las = 1, ylim = c(-limit, limit), xlim = c(timeindexdiff[1], timeindexdiff[length(timeindexdiff)]))
    legend("bottomleft", "first-differenced", bty = "n")
  } else {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindex, nsmY, ylab = "", xlab = "", type = "h", xaxt = "n", las = 1,
         lwd = 2, ylim = c(-limit, limit), xlim = c(timeindex[1], timeindex[length(timeindex)]))
    legend("bottomleft", "residuals", bty = "n")
  }
  plot(timeindex[mw:length(nsmY)], nARR, ylab = "", xlab = "", type = "l", xaxt = "n",
       col = "green", lwd = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))  #3
  legend("bottomright", paste("trend ", round(KtAR$estimate, digits = 3)), bty = "n")
  legend("bottomleft", "autocorrelation", bty = "n")
  plot(timeindex[mw:length(nsmY)], nVAR, ylab = "", xlab = "", type = "l", col = "blue",
       lwd = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomright", paste("trend ", round(KtVAR$estimate, digits = 3)), bty = "n")
  legend("bottomleft", "variance", bty = "n")
  mtext("time", side = 1, line = 2, cex = 0.8)
  mtext("Generic Early-Warnings: Autocorrelation - Variance", side = 3, line = 0.2,
        outer = TRUE)  #outer=TRUE print on the outer margin

  # Output
  out <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD)
  colnames(out) <- c("timeindex", "ar1", "sd")

  return(out)

}

plot_generic_RShiny <- function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                    "linear", "first-diff"), bandwidth, logtransform, interpolate, AR_n = FALSE,
                           powerspectrum = FALSE) {

  # timeseries<-ts(timeseries)
  timeseries <- data.matrix(timeseries)  #strict data-types the input data as tseries object for use in later steps
  if (ncol(timeseries) == 1) {
    Y = timeseries
    timeindex = 1:dim(timeseries)[1]
  } else if (dim(timeseries)[2] == 2) {
    Y <- timeseries[, 2]
    timeindex <- timeseries[, 1]
  } else {
    warning("not right format of timeseries input")
  }
  # return(timeindex) Interpolation
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
    if (timeindex[1] > timeindex[length(timeindex)]) {
      nsmY <- Y - rev(smYY$y)
      smY <- rev(smYY$y)
    } else {
      nsmY <- Y - smYY$y
      smY <- smYY$y
    }
  } else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
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

  nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  for (i in 1:ncol(nMR)) {
    nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
    nARR[i] <- nYR$ar
  }

  nVAR = sqrt(nSD)

  # Estimate Kendall trend statistic for indicators
  timevec <- seq(1, length(nARR))
  KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), method = c("kendall"),
                   conf.level = 0.95)
  KtVAR <- cor.test(timevec, nVAR, alternative = c("two.sided"), method = c("kendall"),
                    conf.level = 0.95)

  # Plotting Generic Early-Warnings dev.new()
  par(mar = (c(1, 2, 0.5, 2) + 0), oma = c(2, 2, 2, 2), mfrow = c(4, 1))
  plot(timeindex, Y, type = "l", ylab = "", xlab = "", xaxt = "n", lwd = 2, las = 1,
       xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomleft", "data", , bty = "n")
  if (detrending == "gaussian") {
    lines(timeindex, smY, type = "l", ylab = "", xlab = "", xaxt = "n", lwd = 2,
          col = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  }
  if (detrending == "no") {
    plot(c(0, 1), c(0, 1), ylab = "", xlab = "", yaxt = "n", xaxt = "n", type = "n",
         las = 1)
    text(0.5, 0.5, "no detrending - no residuals")
  } else if (detrending == "first-diff") {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindexdiff, nsmY, ylab = "", xlab = "", type = "l", xaxt = "n", lwd = 2,
         las = 1, ylim = c(-limit, limit), xlim = c(timeindexdiff[1], timeindexdiff[length(timeindexdiff)]))
    legend("bottomleft", "first-differenced", bty = "n")
  } else {
    limit <- max(c(max(abs(nsmY))))
    plot(timeindex, nsmY, ylab = "", xlab = "", type = "h", xaxt = "n", las = 1,
         lwd = 2, ylim = c(-limit, limit), xlim = c(timeindex[1], timeindex[length(timeindex)]))
    legend("bottomleft", "residuals", bty = "n")
  }
  plot(timeindex[mw:length(nsmY)], nARR, ylab = "", xlab = "", type = "l", xaxt = "n",
       col = "green", lwd = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))  #3
  legend("bottomright", paste("trend ", round(KtAR$estimate, digits = 3)), bty = "n")
  legend("bottomleft", "autocorrelation", bty = "n")
  plot(timeindex[mw:length(nsmY)], nVAR, ylab = "", xlab = "", type = "l", col = "blue",
       lwd = 2, las = 1, xlim = c(timeindex[1], timeindex[length(timeindex)]))
  legend("bottomright", paste("trend ", round(KtVAR$estimate, digits = 3)), bty = "n")
  legend("bottomleft", "variance", bty = "n")
  mtext("time", side = 1, line = 2, cex = 0.8)
  mtext("Generic Early-Warnings: Autocorrelation - Variance", side = 3, line = 0.2,
        outer = TRUE)  #outer=TRUE print on the outer margin

  # Output
  out <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD)
  colnames(out) <- c("timeindex", "ar1", "sd")

  #return(out)
}

# surrogates_Rshiny for estimating significance of trends for variance and
# autocorrelation 6 March 2013

surrogates_RShiny <- function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                       "linear", "first-diff"), bandwidth = NULL, boots = 100, s_level = 0.05, logtransform = FALSE,
                              interpolate = FALSE) {

  timeseries <- data.matrix(timeseries)
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
      bw <- round(length(Y) * bandwidth)/100
    }
    smYY <- ksmooth(timeindex, Y, kernel = c("normal"), bandwidth = bw, range.x = range(timeindex),
                    n.points = length(timeindex))
    nsmY <- Y - smYY$y
    smY <- smYY$y
  } else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
  } else if (detrending == "first-diff") {
    nsmY <- diff(Y)
    timeindexdiff <- timeindex[1:(length(timeindex) - 1)]
  } else if (detrending == "no") {
    smY <- Y
    nsmY <- Y
  }


  # Rearrange data for indicator calculation
  mw <- round(length(Y) * winsize)/100
  omw <- length(nsmY) - mw + 1
  low <- 6
  high <- omw
  nMR <- matrix(data = NA, nrow = mw, ncol = omw)
  for (i in 1:omw) {
    Ytw <- nsmY[i:(i + mw - 1)]
    nMR[, i] <- Ytw
  }
  # Estimate indicator

  indic_ar1 <- apply(nMR, 2, function(x) {
    nAR1 <- ar.ols(x, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
    nAR1$ar
  })

  indic_var <- apply(nMR, 2, var)

  # Calculate trend statistics
  timevec <- seq(1, length(indic_ar1))
  Kt_ar1 <- cor.test(timevec, indic_ar1, alternative = c("two.sided"), method = c("kendall"),
                     conf.level = 0.95)
  Ktauestind_ar1orig <- Kt_ar1$estimate

  Kt_var <- cor.test(timevec, indic_var, alternative = c("two.sided"), method = c("kendall"),
                     conf.level = 0.95)
  Ktauestind_varorig <- Kt_var$estimate

  # Fit ARMA model based on AIC
  arma = matrix(, 4, 5)
  for (ij in 1:4) {
    for (jj in 0:4) {
      ARMA <- arima(nsmY, order = c(ij, 0, jj), include.mean = FALSE)
      arma[ij, jj + 1] = ARMA$aic
      #print(paste("AR", "MA", "AIC"), quote = FALSE)
      #print(paste(ij, jj, ARMA$aic), zero.print = ".", quote = FALSE)
    }
  }

  # Simulate ARMA(p,q) model fitted on residuals
  ind = which(arma == min(arma), arr.ind = TRUE)
  ARMA <- arima(nsmY, order = c(ind[1], 0, ind[2] - 1), include.mean = FALSE)

  Ktauestind_ar1 <- numeric()
  Ktauestind_var <- numeric()

  for (jjj in 1:boots) {
    x = arima.sim(n = length(nsmY), list(ar = c(ARMA$coef[1:ind[1]]), ma = c(ARMA$coef[(1 +
                                                                                          ind[1]):(ind[1] + ind[2] - 1)])), sd = sqrt(ARMA$sigma2))

    ## Rearrange data for indicator calculation
    nMR1 <- matrix(data = NA, nrow = mw, ncol = omw)
    for (i in 1:omw) {
      Ytw <- x[i:(i + mw - 1)]
      nMR1[, i] <- Ytw
    }

    # Estimate indicator

    indic_ar1 <- apply(nMR1, 2, function(x) {
      nAR1 <- ar.ols(x, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)
      nAR1$ar
    })

    indic_var <- apply(nMR1, 2, var)

    # Calculate trend statistics
    timevec <- seq(1, length(indic_ar1))
    Kt_ar1 <- cor.test(timevec, indic_ar1, alternative = c("two.sided"), method = c("kendall"),
                       conf.level = 0.95)
    Ktauestind_ar1[jjj] <- Kt_ar1$estimate

    Kt_var <- cor.test(timevec, indic_var, alternative = c("two.sided"), method = c("kendall"),
                       conf.level = 0.95)
    Ktauestind_var[jjj] <- Kt_var$estimate

  }

  # Estimate probability of false positive
  q_ar1 <- sort(Ktauestind_ar1, na.last = NA)
  Kpos_ar1 <- max(which(Ktauestind_ar1orig > q_ar1), na.rm = TRUE)
  p <- (boots + 1 - Kpos_ar1)/boots
  print(paste("significance autocorrelation p = ", p, " estimated from ", boots,
              " surrogate ARMA timeseries"))

  q_var <- sort(Ktauestind_var, na.last = NA)
  Kpos_var <- max(which(Ktauestind_varorig > q_var), na.rm = TRUE)
  p <- (boots + 1 - Kpos_var)/boots
  print(paste("significance variance p = ", p, " estimated from ", boots, " surrogate ARMA timeseries"))

  # Plotting
  layout(matrix(1:2, 1, 2))
  par(font.main = 10, mar = (c(4.6, 3.5, 0.5, 2) + 0.2), mgp = c(2, 1, 0), oma = c(0.5,
                                                                                   0.5, 2, 0), cex.axis = 0.8, cex.lab = 0.8, cex.main = 0.8)
  hist(Ktauestind_ar1, freq = TRUE, nclass = 20, xlim = c(-1, 1), col = "green",
       main = NULL, xlab = "Surrogate trend estimates", ylab = "occurrence")  #,ylim=c(0,boots))
  abline(v = q_ar1[s_level * boots], col = "red", lwd = 2)
  abline(v = q_ar1[(1 - s_level) * boots], col = "red", lwd = 2)
  points(Ktauestind_ar1orig, 0, pch = 21, bg = "black", col = "black", cex = 4)
  title("Autocorrelation", cex.main = 1.3)

  hist(Ktauestind_var, freq = TRUE, nclass = 20, xlim = c(-1, 1), col = "blue",
       main = NULL, xlab = "Surrogate trend estimates", ylab = "occurrence")  #,ylim=c(0,boots))
  abline(v = q_var[s_level * boots], col = "red", lwd = 2)
  abline(v = q_var[(1 - s_level) * boots], col = "red", lwd = 2)
  points(Ktauestind_varorig, 0, pch = 21, bg = "black", col = "black", cex = 4)
  title("Variance", cex.main = 1.3)
}

#' Moving Average Potential
#'
#' This function reconstructs a potential derived from data along a gradient of a given parameter.
#'
#' Arguments:
#'  @param X a vector of the X observations of the state variable of interest
#'  @param param parameter values corresponding to the observations in X
#'  @param bw Bandwidth for smoothing kernels. Automatically determined by default.
#'  @param bw.adjust Bandwidth adjustment constant
#'  @param detection.threshold Threshold for local optima to be discarded.
#'  @param std Standard deviation.
#'  @param grid.size number of evaluation points; number of steps between min and max potential; also used as kernel window size
#'  @param plot.cutoff cuttoff for potential minima and maxima in visualization
#'  @param plot.contours Plot contours on the landscape visualization
#'  @param binwidth binwidth for contour plot
#'  @param bins bins for contour plot. Overrides binwidth if given
#'
#'  @return A list with the following elements:
#'     pars values of the covariate parameter as matrix;
#'     xis values of the x as matrix;
#'     pots smoothed potentials;
#'     mins minima in the densities (-potentials; neglecting local optima);
#'     maxs maxima in densities (-potentials; neglecting local optima);
#'     plot an object that displays the potential estimated in 2D
#'
#' @export
#'
#' @references Hirota, M., Holmgren, M., van Nes, E.H. & Scheffer, M. (2011). Global resilience of tropical forest and savanna to critical transitions. \emph{Science}, 334, 232-235.
#' @author L. Lahti, E. van Nes, V. Dakos.
#' @seealso \code{\link{generic_ews}}; \code{\link{ddjnonparam_ews}}; \code{\link{bdstest_ews}}; \code{\link{sensitivity_ews}};\code{\link{surrogates_ews}}; \code{\link{ch_ews}}; \code{livpotential_ews}
# ; \code{\link{timeVAR_ews}}; \code{\link{thresholdAR_ews}}
#' @examples X = c(rnorm(1000, mean = 0), rnorm(1000, mean = -2),
#'             rnorm(1000, mean = 2));
#'       param = seq(0,5,length=3000);
#'       res <- movpotential_ews(X, param)
#' @keywords early-warning

movpotential_ews <- function(X, param = NULL, bw = "nrd", bw.adjust = 1, detection.threshold = 0.1,
                             std = 1, grid.size = 50, plot.cutoff = 0.5, plot.contours = TRUE, binwidth = 0.2,
                             bins = NULL) {

  if (is.null(param)) {
    param <- seq(1, length(X), 1)
  }

  nas <- is.na(param) | is.na(X)
  if (sum(nas) > 0) {
    warning("The data contains NAs, removing the associated samples from X and param input arguments.")
    X <- X[!nas]
    param <- param[!nas]
  }

  minparam <- min(param)
  maxparam <- max(param)

  # Determine step size
  sdwindow <- step <- (maxparam - minparam)/grid.size

  # Place evaluation points evenly across data range
  xi <- seq(min(X), max(X), length = grid.size)

  # Initialize
  xis <- pars <- pots <- matrix(0, nrow = grid.size, ncol = length(xi))
  maxs <- mins <- matrix(0, nrow = grid.size, ncol = length(xi))

  for (i in 1:grid.size) {

    # Increase the parameter at each step
    par <- minparam + (i - 0.5) * step

    # Check which elements in evaluation range (param) are within 2*sd of par
    weights <- exp(-0.5 * (abs(par - param)/sdwindow)^2)

    # LL: Normalization was added in the R implementation 16.5.2012
    weights <- weights/sum(weights)

    # Calculate the potential
    tmp <- livpotential_ews(x = X, std = std, bw = bw, bw.adjust = bw.adjust,
                            weights = weights, grid.size = grid.size)

    # Store variables
    pots[i, ] <- tmp$pot
    xis[i, ] <- tmp$grid.points
    pars[i, ] <- par + rep(0, length(tmp$grid.points))
    mins[i, tmp$min.inds] <- 1
    maxs[i, tmp$max.inds] <- 1

  }

  res <- list(pars = pars, xis = xis, pots = pots, mins = mins, maxs = maxs, std = std)

  p <- PlotPotential(res, title = "Moving Average Potential", "parameter/time",
                     "state variable", cutoff = plot.cutoff, plot.contours = plot.contours, binwidth = binwidth,
                     bins = bins)

  list(res = res, plot = p)

}

#' Description: Plot Potential
#'
#' Visualization of the potential function from the movpotential function
#'
#' Arguments:
#'    @param res output from movpotential function
#'    @param title title text
#'    @param xlab.text xlab text
#'    @param ylab.text ylab text
#'    @param cutoff parameter determining the upper limit of potential for visualizations
#'    @param plot.contours Plot contour lines.
#'    @param binwidth binwidth for contour plot
#'    @param bins bins for contour plot. Overrides binwidth if given
#'
#' @importFrom tgp interp.loess
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 stat_contour
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 labs
#' @return \item{ggplot2}{potential plotted}
#'
#' @export
#'
#' @references Dakos, V., et al (2012).'Methods for Detecting Early Warnings of Critical Transitions in Time Series Illustrated Using Simulated Ecological Data.' \emph{PLoS ONE} 7(7): e41010. doi:10.1371/journal.pone.0041010
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @examples   X = c(rnorm(1000, mean = 0), rnorm(1000, mean = -2),
#'                  rnorm(1000, mean = 2))
#'	       param = seq(0,5,length=3000);
#'	       res <- movpotential_ews(X, param);
#'	       PlotPotential(res$res, title = '',
#'	       	             xlab.text = '', ylab.text = '',
#'			     cutoff = 0.5,
#'			     plot.contours = TRUE, binwidth = 0.2)
#'
#' @keywords early-warning

PlotPotential <- function(res, title = "", xlab.text, ylab.text, cutoff = 0.5, plot.contours = TRUE,
                          binwidth = 0.2, bins = NULL) {

  scale_fill_gradient <- NULL # Avoid build warnings

  cut.potential <- max(apply(res$pots, 1, min)) + cutoff * abs(max(apply(res$pots,
                                                                         1, min)))  # Ensure all minima are visualized
  pots <- res$pots
  pots[pots > cut.potential] <- cut.potential

  # Static contour Interpolate potential grid
  intp <- tgp::interp.loess(as.vector(res$pars), as.vector(res$xis), as.vector(pots),
                            gridlen = 2 * dim(pots))
  xy <- expand.grid(intp$x, intp$y)
  z <- as.vector(intp$z)
  z[is.na(z)] <- max(na.omit(z))

  potential <- NULL
  df <- data.frame(list(bg.var = xy[, 1], phylotype = xy[, 2], potential = z))
  bg.var <- NULL
  phylotype <- NULL

  p <- ggplot(df)
  p <- p + geom_tile(aes(x = bg.var, y = phylotype, z = potential, fill = potential))
  p <- p + scale_fill_gradient(low = "black", high = "white")

  if (plot.contours) {
    if (!is.null(bins)) {
      warning("bins argument is overriding the binwidth argument!")
      p <- p + stat_contour(bins = bins, aes(x = bg.var, y = phylotype, z = potential, fill = potential))
    } else {
      p <- p + stat_contour(binwidth = binwidth, aes(x = bg.var, y = phylotype, z = potential, fill = potential))
    }
  }

  p <- p + xlab(xlab.text) + ylab(ylab.text) + labs(title = title)

  p

}

#' Description: Potential Analysis for univariate data
#'
#' \code{livpotential_ews} performs one-dimensional potential estimation derived from a uni-variate timeseries
#'
#' Arguments:
#'    @param x Univariate data (vector) for which the potentials shall be estimated
#'    @param std Standard deviation of the noise (defaults to 1; this will set scaled potentials)
#'    @param bw kernel bandwidth estimation method
#'    @param weights optional weights in ksdensity (used by movpotentials).
#'    @param grid.size Grid size for potential estimation.
#'    @param detection.threshold maximum detection threshold as fraction of density kernel height dnorm(0, sd = bandwidth)/N
#'    @param bw.adjust The real bandwidth will be bw.adjust*bw; defaults to 1
#'    @param density.smoothing Add a small constant density across the whole observation range to regularize density estimation (and to avoid zero probabilities within the observation range). This parameter adds uniform density across the observation range, scaled by density.smoothing.
#'    @param detection.limit minimum accepted density for a maximum; as a multiple of kernel height
#'
# Returns:
#'   @return \code{livpotential} returns a list with the following elements:
#'   @return \item{xi}{the grid of points on which the potential is estimated}
#'   @return \item{pot}{The estimated potential: -log(f)*std^2/2, where f is the density.}
#'   @return \item{density}{Density estimate corresponding to the potential.}
#'   @return \item{min.inds}{indices of the grid points at which the density has minimum values; (-potentials; neglecting local optima)}
#'   @return \item{max.inds}{indices the grid points at which the density has maximum values; (-potentials; neglecting local optima)}
#'   @return \item{bw}{bandwidth of kernel used}
#'   @return \item{min.points}{grid point values at which the density has minimum values; (-potentials; neglecting local optima)}
#'   @return \item{max.points}{grid point values at which the density has maximum values; (-potentials; neglecting local optima)}
#'
#' @export
#'
#' @references Livina, VN, F Kwasniok, and TM Lenton, 2010. Potential analysis reveals changing number of climate states during the last 60 kyr . \emph{Climate of the Past}, 6, 77-82.
#'
#' Dakos, V., et al (2012).'Methods for Detecting Early Warnings of Critical Transitions in Time Series Illustrated Using Simulated Ecological Data.' \emph{PLoS ONE} 7(7): e41010. doi:10.1371/journal.pone.0041010
#' @author Based on Matlab code from Egbert van Nes modified by Leo Lahti. Implemented in early warnings package by V. Dakos.
#' @seealso
#' \code{\link{generic_ews}}; \code{\link{ddjnonparam_ews}}; \code{\link{bdstest_ews}}; \code{\link{sensitivity_ews}};\code{\link{surrogates_ews}}; \code{\link{ch_ews}};\code{\link{movpotential_ews}}
# ; \code{\link{timeVAR_ews}}; \code{\link{thresholdAR_ews}}
#' @examples
#' data(foldbif)
#' res <- livpotential_ews(foldbif[,1])
#' @keywords early-warning

livpotential_ews <- function(x, std = 1, bw = "nrd", weights = c(), grid.size = NULL,
                             detection.threshold = 1, bw.adjust = 1, density.smoothing = 0, detection.limit = 1) {

  # std <- 1; bw <- 'nrd'; weights <- c(); grid.size = floor(.2*length(x)); detection.threshold = 1; bw.adjust = 1; density.smoothing = 0; grid.size = NULL

  if (is.null(grid.size)) {
    grid.size <- floor(0.2 * length(x))
  }

  # Density estimation
  de <- density(ts(data.frame(x)), bw = bw, adjust = bw.adjust,
                kernel = "gaussian", weights = weights,
                window = kernel, n = grid.size,
                from = min(x), to = max(x),
                cut = 3, na.rm = FALSE)

  # Smooth the estimated density (f <- de$y) by adding a small
  # probability across the whole observation range (to avoid zero
  # probabilities for points in the observation range)
  f <- de$y + density.smoothing * 1/diff(range(de$x)) # *max(de$y)

  # Normalize the density such that it integrates to unity
  f <- f/sum(diff(de$x[1:2]) * f)

  # Final grid points and bandwidth
  grid.points <- de$x
  bw <- de$bw

  # Compute potential
  U <- -log(f) * std^2/2
  # f <- exp(-2*U/std^2) # backtransform to density distribution Ignore very local
  # optima Note mins and maxs for density given here (not for potential, which has
  # the opposite signs)

  ops <- find.optima(f, detection.threshold = detection.threshold, bw = bw, x = x, detection.limit = detection.limit)
  min.points <- grid.points[ops$min]
  max.points <- grid.points[ops$max]
  det.th <- ops$detection.threshold

  list(grid.points = grid.points, pot = U, density = f, min.inds = ops$min, max.inds = ops$max,
       bw = bw, min.points = min.points, max.points = max.points, detection.threshold = det.th)

}

#' Description: find.optima
#'
#' Detect optima, excluding very local optima below detection.threshold
#'
#'  Arguments:
#'    @param f density
#'    @param detection.threshold detection threshold for peaks
#'    @param bw bandwidth
#'    @param x original data
#'    @param detection.limit Minimun accepted density for a maximum;
#'                           as a multiple of kernel height
#'
#' Returns:
#'    @return A list with the following elements:
#'      min minima
#'      max maxima
#'	detection.density Minimum detection density
#' @export
#'
#' @references See citation('TBA')
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @examples #
#'
#' @keywords utilities

find.optima <- function(f, detection.threshold = 0, bw, x, detection.limit = 1) {

  # multiple of kernel height
  kernel.height <- dnorm(0, sd = bw) / length(x)
  deth <- detection.threshold * kernel.height
  detl <- detection.limit * kernel.height

  # Detect minima and maxima of the density (see Livina et al.) these correspond
  # to maxima and minima of the potential, respectively including end points of the
  # vector
  maxima <- find.maxima(f)
  minima <- find.minima(f)

  # remove maxima that are below detection limit
  maxima <- maxima[f[maxima] >= detl]
  minima <- remove_obsolete_minima(f, maxima, minima)
  minima <- unlist(minima)
  maxima <- unlist(maxima)

  # Remove minima and maxima that are too shallow
  delmini <- logical(length(minima))
  delmaxi <- logical(length(maxima))
  for (j in 1:length(maxima)) {

    # Calculate distance of this maximum to all minima
    s <- minima - maxima[[j]]

    # Set distances to deleted minima to zero
    s[delmini] <- 0

    # identify the closest remaining minima
    i1 <- i2 <- NULL
    if (length(s) > 0) {

      minima.spos <- minima[s > 0]
      minima.sneg <- minima[s < 0]

      if (length(minima.spos) > 0) {
        i1 <- min(minima.spos)
      }
      if (length(minima.sneg) > 0) {
        i2 <- max(minima.sneg)
      }

    }

    # if no positive differences available, set it to same value with i2
    if ((is.null(i1) && !is.null(i2))) {
      i1 <- i2
    } else if ((is.null(i2) && !is.null(i1))) {
      # if no negative differences available, set it to same value with i1
      i2 <- i1
    }

    if (!is.null(i1) && is.na(i1)) {
      i1 <- NULL
    }
    if (!is.null(i2) && is.na(i2)) {
      i2 <- NULL
    }

    # If a closest minimum exists, check differences and remove if difference is
    # under threshold
    if (!is.null(i1)) {

      # Smallest difference between this maximum and the closest minima
      diff <- min(c((f[maxima[[j]]] - f[i1]), (f[maxima[[j]]] - f[i2])))

      if (diff < deth) {

        # If difference is below threshold, delete this maximum
        delmaxi[[j]] <- TRUE

        # Delete the larger of the two neighboring minima
        if (f[[i1]] > f[[i2]]) {
          delmini[minima == i1] <- TRUE
        } else {
          delmini[minima == i2] <- TRUE
        }
      }

    } else {
      # if both i1 and i2 are NULL, do nothing
    }
  }

  # Delete the shallow minima and maxima
  if (length(minima) > 0 && sum(delmini) > 0) {
    minima <- minima[!delmini]
  }

  # Combine maxima that do not have minima in between
  if (length(maxima) > 1) {
    maxima2 <- c()
    for (i in 1:(length(maxima) - 1)) {
      nominima <- TRUE
      cnt <- 0
      while (nominima & (i + cnt) < length(maxima)) {
        cnt <- cnt + 1
        nominima <- sum(minima > maxima[[i]] & minima < maxima[[i + cnt]]) == 0
        # if (is.na(nominima)) {nominima <- TRUE}
      }
      maxs <- maxima[i:(i + cnt - 1)]
      maxima2 <- c(maxima2, round(mean(maxs[which(f[maxs] == max(f[maxs]))])))
    }
    if (!maxima[[length(maxima)]] %in% maxima2) {
      maxima2 <- c(maxima2, maxima[[length(maxima)]])
    }
    maxima <- maxima2
  }


  if (length(maxima) > 0 && sum(delmaxi) > 0) {
    maxima <- maxima[!delmaxi]
  }

  list(min = minima, max = maxima, detection.density = deth)

}

find.minima <- function (f) {
  find.maxima(-f)
}

find.maxima <- function (f) {

  f2 <- c(Inf, -f, Inf)
  cnt <- 1
  ops <- c()
  opcnt <- 0
  while (cnt < length(f2)) {
    if (f2[[cnt + 1]] - f2[[cnt]] <= 0) {
      while (f2[[cnt + 1]] - f2[[cnt]] <= 0) {
        cnt <- cnt + 1
      }
      ind1 <- cnt - 1
      while (f2[[cnt + 1]] - f2[[cnt]] == 0) {
        cnt <- cnt + 1
      }
      if (f2[[cnt + 1]] - f2[[cnt]] > 0) {
        ind2 <- cnt - 1
        opcnt <- opcnt + 1
        ops[[opcnt]] <- round(mean(c(ind1, ind2)))
      } else if (f2[[cnt + 1]] - f2[[cnt]] < 0) {
        ind2 <- NULL
      }
    }
    cnt <- cnt + 1
  }
  ops
}

find.minima <- function (f) {
  find.maxima(-f)
}

find.maxima <- function (f) {

  f2 <- c(Inf, -f, Inf)
  cnt <- 1
  ops <- c()
  opcnt <- 0
  while (cnt < length(f2)) {
    if (f2[[cnt + 1]] - f2[[cnt]] <= 0) {
      while (f2[[cnt + 1]] - f2[[cnt]] <= 0) {
        cnt <- cnt + 1
      }
      ind1 <- cnt - 1
      while (f2[[cnt + 1]] - f2[[cnt]] == 0) {
        cnt <- cnt + 1
      }
      if (f2[[cnt + 1]] - f2[[cnt]] > 0) {
        ind2 <- cnt - 1
        opcnt <- opcnt + 1
        ops[[opcnt]] <- round(mean(c(ind1, ind2)))
      } else if (f2[[cnt + 1]] - f2[[cnt]] < 0) {
        ind2 <- NULL
      }
    }
    cnt <- cnt + 1
  }
  ops
}

remove_obsolete_minima <- function (f, maxima, minima) {

  # remove minima that now became obsolete If there are multiple minima between two
  # consecutive maxima after removing the maxima that did not pass the threshold,
  # take the average of the minima;return the list of indices such that between
  # each pair of consecutive maxima, there is exactly one minimum
  if (length(maxima) > 1) {
    minima <- sapply(2:length(maxima), function(i) {

      mins <- minima[minima >= maxima[[i - 1]] & minima <= maxima[[i]]]
      if (length(mins) > 0) {
        round(mean(mins[which(f[mins] == min(f[mins]))]))
      } else {
        NULL
      }
    })

  } else {
    minima <- NULL
  }

  # Remove minima that are outside the most extreme maxima
  minima <- minima[minima > min(maxima) & minima < max(maxima)]

  minima
}
