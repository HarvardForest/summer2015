### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Last edited: 09June2015

# server.R #

# load dependencies
library(shiny)
library(shinyapps)
library(deSolve)
library(breakpoint)
#library(earlywarnings)
library(ggplot2)
library(Cairo)
options(shiny.usecairo=T)
source("global.R", local=TRUE)

## start server ##
shinyServer(
  # predator-prey model
  function(input, output, session){
    theModel <- reactive({
     lotVpredPrey(seq(0, input$time, by=1),
                  c(prey=input$prey, predator=input$predators),
                  c(alpha=input$alpha, beta=input$beta, delta=input$delta,
                    gamma=input$gamma))
    })

    # load user-input boxes #
    output$alpha2 <- renderUI({
      numericInput("alpha2", label="", value=input$alpha)
    })

    output$beta2 <- renderUI({
      numericInput("beta2", label="", value=input$beta)
    })

    output$delta2 <- renderUI({
      numericInput("delta2", label="", value=input$delta)
    })

    output$gamma2 <- renderUI({
      numericInput("gamma2", label="", value=input$gamma)
    })
    # end load user-input boxes #

    # link user-input box values with respective slider values
    observe({
      updateSliderInput(session, "alpha", value=input$alpha2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "delta", value=input$delta2)
      updateSliderInput(session, "gamma", value=input$gamma2)
    })

    # predator-prey plot
    output$mainPlot <- renderPlot({
      matplot(theModel(), type="l", xlab=input$xaxis, ylab=input$yaxis)
      title(main=input$plotTitle)
      legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
             col=c(1, 2), bty="n")
    })

    ### breakpoint analysis ###

    output$tp1 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }

      switch(input$dataType,
        "Prey" =  selectInput("breakpointType", "Analysis:",
                    choice=c(" ", "with Negative Binomial Distribution", "for Continuous Data",
                      "with Zero-Inflated Negative Binomial Distribution")
                  ),
        "Predator" =  selectInput("breakpointType", "Analysis:",
                              choice=c(" ", "with Negative Binomial Distribution", "for Continuous Data",
                                       "with Zero-Inflated Negative Binomial Distribution")
        )
      )
    })

    output$tp2 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }

      switch(input$breakpointType,
        "with Negative Binomial Distribution" = selectInput("distributionType", "Distribution to simulate break-point locations:",
                                                  choice=c(" ", "Four Parameter Beta Distribution", "Truncated Normal Distribution")
                                                ),
        "for Continuous Data" = selectInput("distributionType", "Distribution to simulate break-point locations:",
                                                            choice=c(" ", "Four Parameter Beta Distribution", "Truncated Normal Distribution")
        ),
        "with Zero-Inflated Negative Binomial Distribution" = selectInput("distributionType", "Distribution to simulate break-point locations:",
                                                            choice=c(" ", "Four Parameter Beta Distribution", "Truncated Normal Distribution")
        )
      )
    })

    output$tp3 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("Nmax", "Maximum number of break-points:",
                   value=10)
    })

    output$tp4 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("a", "Used in the four parameter beta distribution to smooth both
                   shape parameters. When simulating from the truncated normal
                   distribution, this value is used to smooth the estimates of the
                   mean values:", value=0.8)
    })

    output$tp5 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("b", "A smoothing parameter value. It is used in the truncated
                   normal distribution to smooth the estimates of the standard
                   deviation:", value=0.8)
    })

    output$tp6 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("h", "Minimum aberration width:", value=5)
    })

    output$tp7 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("M", "Sample size to be used in simulating the locations of break-points:",
                   value=200)
    })

    output$tp8 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("rho", "The fraction which is used to obtain the best performing
                   set of sample solutions (i.e., elite sample):", value=0.05)
    })

    output$tp9 <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("eps", "the cut-off value for the stopping criterion in the CE method:",
                   value=0.01)
    })

    output$tpRun <- renderUI({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      actionButton("tpRun", "Run")
    })

    # run tipping point analysis based on user's parameters
    TPanalysis <- eventReactive(input$tpRun, function(){
      # loading window
      withProgress(message="Analyzing breakpoints", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for Prey
          if(input$dataType == "Prey"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(theModel()[1], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                      rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(theModel()[1], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                      rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[1], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[1], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == "with Zero-Inflated Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[1], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[1], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
          }

          # for predator
          else if(input$dataType == "Predator"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(theModel()[2], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                      rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(theModel()[2], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                      rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[2], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[2], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == "with Zero-Inflated Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[2], distyp=1, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[2], distyp=2, parallel=TRUE, Nmax=input$Nmax, eps=input$eps,
                          rho=input$rho, M=input$M, h=input$h, a=input$a, b=input$b)
              }
            }
          }
        }) # withProgress
      }) # withProgress
    })

    # display number of breakpoints detected
    output$tpAnalysis1 <- renderText({
      TPanalysis()[[1]]
    })

    # display "Location:" text
    output$location <- renderText({
      if(length(TPanalysis()) > 1){
        "Location:"
      }
    })

    # display locations of detected breakpoints
    output$tpAnalysis2 <- renderText({
      if(length(TPanalysis()) > 1){
       paste(TPanalysis()[[2]], collapse=", ")
      }
    })

    # display button to add breakpoint lines to new plot
    output$plotLinesButton <- renderUI({
      if(length(TPanalysis()) > 1){
        actionButton("plotLinesButton", "Add to Plot")
      }
    })

    # generate new instance of mainPlot to draw breakpoint lines
    tempPlot <- eventReactive(input$plotLinesButton, function(){
      theModel()
    })

    # display new plot with breakpoint lines
    output$breakpointPlot <- renderPlot({
      matplot(tempPlot(), type="l", xlab=input$xaxis, ylab=input$yaxis)
      title(main=input$plotTitle)
      legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
             col=c(1, 2), bty="n")
      # add breakpoint lines
      abline(v=TPanalysis()[[2[1]]], col="blue")
    })

    # display button to show tp profile-plot
    output$tpProfileButtonSlot <- renderUI({
      if(length(TPanalysis()) > 1){
        actionButton("tpProfileButton", "Mean Profile Plot")
      }
    })

    # load profile plot from breakpoint package
    tempProfilePlot <- eventReactive(input$tpProfileButton, function(){
      if(input$dataType == "Prey"){
        profilePlot(TPanalysis(), theModel()[1], x.label=input$dataType)
      }
      else if(input$dataType == "Predator"){
        profilePlot(TPanalysis(), theModel()[2], x.label=input$dataType)
      }
    })

    # display tp breakpoint profile
    output$tpProfilePlot <- renderPlot({
      # check required information
      if(input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      tempProfilePlot()
    })

    ### earlywarnings analysis ###

    output$ews1 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }

      switch(input$ewsDataType,
             "Prey" =  selectInput("ewsMethod", "Method:",
                        choice=c(" ", "Generic Early Warning Signals")
             ),
             "Predator" =  selectInput("ewsMethod", "Method:",
                        choice=c(" ", "Generic Early Warning Signals")
             )
      )
    })

    output$ews2 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      selectInput("detrending", label="Detrended/filtered prior to analysis:",
                  choice=c("no", "gaussian", "loess", "linear", "first-diff"))
    })

    output$ews3 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      numericInput("bandwidth", label="Bandwidth used for the Gaussian kernel when gaussian filtering
                  is applied. It is expressed as percentage of the timeseries length
                  (must be numeric between 0 and 100):",
                  value=5)
    })

    output$ews4 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      numericInput("winsize", label="The size of the rolling window expressed as
                percentage of the timeseries length (must be numeric between 0 and 100):",
                value=50)
    })

    output$ews5 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      numericInput("span", label="Parameter that controls the degree of smoothing
                (numeric between 0 and 100):",
                value=25)
    })

    output$ews6 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      numericInput("degree", label="The degree of polynomial to be used for when loess
                fitting is applied, normally 1 or 2:",
                value=2)
    })

    output$ews7 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      selectInput("logtransform", label="If TRUE data are logtransformed prior to analysis as log(X+1):",
                choice=c(FALSE, TRUE))
    })

    output$ews8 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      selectInput("interpolate", label="If TRUE linear interpolation is applied to produce a timeseries of equal length as
                the original. (FALSE assumes there are no gaps in the timeseries):",
                choice=c(FALSE, TRUE))
    })

    output$ews9 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      selectInput("AR_n", label="If TRUE the best fitted AR(n) model is fitted to the data:",
                  choice=c(FALSE, TRUE))
    })

    output$ews10 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }

      selectInput("powerspectrum", label="If TRUE the power spectrum within each rolling window is plotted:",
                  choice=c(FALSE, TRUE))
    })

    # display ewsRun button
    output$ewsRun <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      actionButton("ewsRunButton", "Run")
    })

    # run early warnings analysis based on user's parameters
    EWSanalysis <- eventReactive(input$ewsRunButton, function(){
      # for prey
      if(input$ewsDataType == "Prey"){
        ews <- local_generic_ews(timeseries=subset(theModel(), select=prey), winsize=input$winsize,
                          detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                          degree=input$degree, logtransform=input$logtransform,
                          interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
      }

      # for predator
      else if(input$ewsDataType == "Predator"){
        ews <- local_generic_ews(timeseries=subset(theModel(), select=predator), winsize=input$winsize,
                           detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                           degree=input$degree, logtransform=input$logtransform,
                           interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
      }
    })

    # load guide for ews data table
    load_ewsTableGuide <- eventReactive(input$ewsRunButton, function(){
"tim = the time index.

ar1  = the autoregressive coefficient ar(1) of a first order AR model fitted on the data within the rolling window.

sd = the standard deviation of the data estimated within each rolling window.

sk	= the skewness of the data estimated within each rolling window.

kurt	= the kurtosis of the data estimated within each rolling window.

cv	= the coefficient of variation of the data estimated within each rolling window.

returnrate	= the return rate of the data estimated as 1-ar(1) cofficient within each rolling window.

densratio	= the density ratio of the power spectrum of the data estimated as the ratio of low frequencies over high frequencies within each rolling window.

acf1	= the autocorrelation at first lag of the data estimated within each rolling window."
    })

    # display guide for ews data table
    output$ewsTableGuide <- renderText({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      load_ewsTableGuide()
    })

    # display ews data table
    output$ewsTable <- renderDataTable({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      EWSanalysis()
    }, options=list(pageLength=5))

    # load ews plot button
    ewsPlotButtonLoad <- eventReactive(input$ewsRunButton, function(){
      actionButton("ewsPlotButton", "Show Plot")
    })

    # display ews plot button
    output$ewsPlotButtonSlot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      ewsPlotButtonLoad()
    })

    # load ews plot
    ewsPlotLoad <- eventReactive(input$ewsPlotButton, function(){
      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- local_generic_ews(timeseries=subset(theModel(), select=prey), winsize=input$winsize,
                                     detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                                     degree=input$degree, logtransform=input$logtransform,
                                     interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- local_generic_ews(timeseries=subset(theModel(), select=predator), winsize=input$winsize,
                                     detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                                     degree=input$degree, logtransform=input$logtransform,
                                     interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
          }
        })
      })
    })

    # display ews plot
    output$ewsPlot <- renderPlot({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      ewsPlotLoad()
    }, height=850, width=1200)

    # predator-prey data table
    output$mainTable <- renderDataTable({
      mNew <- cbind(time=0:(input$time), theModel())
    })

    output$temp <- renderPrint({
      data <- theModel()
      qda_ews(data[1], param = NULL, winsize = 50, detrending='gaussian', bandwidth=NULL,
                       boots = 50, s_level = 0.05, cutoff=0.05, detection.threshold = 0.002, grid.size = 50,
                       logtransform=FALSE, interpolate=FALSE)
    })

  } # End
)
