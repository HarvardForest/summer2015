################################################################################
################################################################################
###################### Pitcher Plant Simulation ################################
################## By: Nathan Justice and Matt Lau #############################
##################### Last edited: 29July2015 ##################################
################################################################################
################################################################################

###### Global dependencies and function(s) ######

# load dependencies
library(shiny)
library(shinythemes)
library(shinyAce)
library(deSolve)
library(cpm)
#library(earlywarnings)
library(moments)
library(plotrix)

## Functions ##

# transform the values of x so that the range of x is equal to the range of y
rescale <- function(x,y){
    x.range <- range(x)
    y.range <- range(y)
    x <- ((x - x.range[1])* (diff(y.range))) / diff(x.range) + y.range[1]
    if (any(range(x) != range(y))){
        warning('Ranges do not match.')
    }else{
        return(x)
    }
}

# 6:00 sunrise = 360
# 12:00 noon = 720
# 18:00 sunset = 1080

PAR <- function(days=3,start=0,amp=100){
    amp * sin(2 * pi * rep((1:1440 + 1080 + start),days) * (1/1440))
}

photo <- function(days=3,Amax=4,Aqe=0.3,LCP=0,start=0,amp=50){
    out <- Amax * (1 - exp(-Aqe * (PAR(days,start,amp) - LCP)))
    out[out < LCP] <- 0
    return(out)
}

pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=5, beta=0.001, k=1, Bscaler=10,
                            aMax=10, aMin=1, s=10, d=1, c=100) {

minute <- vector(mode="numeric") # t/time variable
x <- vector(mode="numeric") # amount of o2
a <- vector(mode="numeric") # augmentation function
P <- vector(mode="numeric") # photosynthesis
B <- vector(mode="numeric") # biological o2 demand
n <- vector(mode="numeric") # amount of nutrients
w <- vector(mode="numeric") # amount of food

if (length(foodWeight) < days){
    foodWeight <- rep(foodWeight,days)[1:days]
}

## Initialization ##

# simulate photosynthesis as fixed values
P <- photo(days)

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
  w <- c(w, w[length(w)]+foodWeight[z])

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
        ## adjust amount of food
        w <- c(w, w[length(w)]*exp(-beta*(1)))
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

# The following is a local copy of the 'generic_ews()' method from the
# 'earlywarnings' Pakcage. A local copy was made to remove the plotting features
# present in the function implementation because it bugged the application's
# graphics device.

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
    timeseries <- as.matrix(timeseries)  #strict data-types the input data as tseries object for use in later steps
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
    low <- 2
    high <- mw
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
    nSPECT <- matrix(0, nrow = mw, ncol = ncol(nMR))
    nCV <- numeric()
    smARall <- numeric()
    smARmaxeig <- numeric()
    detB <- numeric()
    ARn <- numeric()

    nSD <- apply(nMR, 2, sd, na.rm = TRUE)
    for (i in 1:ncol(nMR)) {
        nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, demean = TRUE, intercept = FALSE)
        nARR[i] <- nYR$ar
        # nSD[i]<-sapply(nMR[,i], sd, na.rm = TRUE)#sd(nMR[,i], na.rm = TRUE)
        nSK[i] <- abs(moments::skewness(nMR[, i], na.rm = TRUE))
        nKURT[i] <- moments::kurtosis(nMR[, i], na.rm = TRUE)
        nCV[i] <- nSD[i]/mean(nMR[, i])
        ACF <- acf(nMR[, i], lag.max = 1, type = c("correlation"), plot = FALSE)
        nACF[i] <- ACF$acf[2]
        spectfft <- spec.ar(nMR[, i], n.freq = mw, plot = FALSE, order = 1)
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
        nDENSITYRATIO, nACF,row.names = NULL)
    colnames(out) <- c("timeindex", "ar1", "sd", "sk", "kurt", "cv", "returnrate",
        "densratio", "acf1")
    return(out)
}
