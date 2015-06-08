### Lotka-Volterra predator-prey model
# server.R
library(shiny)
library(shinyapps)
library(deSolve)
library(breakpoint)
library(earlywarnings)
library(ggplot2)
library(Cairo)
options(shiny.usecairo=T)

lotVpredPrey <- function(time, initState, params){
  # Function for ordinary differential equations (ODE)
  lotVPPeqs <-function(time, initState, params){
    with(as.list(c(initState, params)),{

      # Lotka-Volterra predator-prey model
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
  output <- data.frame(ode(y=initState, times=time, func=lotVPPeqs, parms=params)[,-1])

  return(output)
}

shinyServer(
  function(input, output, session){
    theModel <- reactive({
     lotVpredPrey(seq(0, input$time, by=1),
                  c(prey=input$prey, predator=input$predators),
                  c(alpha=input$alpha, beta=input$beta, delta=input$delta,
                    gamma=input$gamma))
    })

    output$plot <- renderPlot({
      matplot(theModel(), type="l", xlab=input$xaxis, ylab=input$yaxis)
      title(main=input$plotTitle)
      legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
             col=c(1, 2), bty="n")
    })

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

    observe({
      updateSliderInput(session, "alpha", value=input$alpha2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "delta", value=input$delta2)
      updateSliderInput(session, "gamma", value=input$gamma2)
    })

    output$table <- renderDataTable({
      mNew <- cbind(time=0:(input$time), theModel())
    })

    negBinDistUI <- renderUI({
      input$CENB
    })

    output$tpOne <- renderUI({
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

    output$tpTwo <- renderUI({
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

    output$tpRun <- renderUI({
      if(input$dataType == " "){
        return()
      }

      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }

      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      actionButton("run", "Run")
    })

    TPanalysis <- eventReactive(input$run, function(){
      withProgress(message="Analyzing breakpoints", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          if(input$dataType == "Prey"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(theModel()[1], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(theModel()[1], distyp=2, parallel=TRUE)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[1], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[1], distyp=2, parallel=TRUE)
              }
            }
            else if(input$breakpointType == "with Zero-Inflated Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[1], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[1], distyp=2, parallel=TRUE)
              }
            }
          }

          else if(input$dataType == "Predator"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(theModel()[2], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(theModel()[2], distyp=2, parallel=TRUE)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[2], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[2], distyp=2, parallel=TRUE)
              }
            }
            else if(input$breakpointType == "with Zero-Inflated Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(theModel()[2], distyp=1, parallel=TRUE)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(theModel()[2], distyp=2, parallel=TRUE)
              }
            }
          }
        }) # withProgress
      }) # withProgress
    })

    output$tpAnalysis_one <- renderText({
      TPanalysis()[[1]]
    })

    output$tpAnalysis_two <- renderText({
      if(length(TPanalysis()) > 1){
       paste(TPanalysis()[[2]], collapse=", ")
      }
    })

    output$plotLinesButton <- renderUI({
      if(length(TPanalysis()) > 1){
        actionButton("plotLinesButton", "Add to Plot")
      }
    })

    output$location <- renderText({
      if(length(TPanalysis()) > 1){
        "Location:"
      }
    })

    temp <- eventReactive(input$plotLinesButton, function(){
      theModel()
    })

    output$plot2 <- renderPlot({
      matplot(temp(), type="l", xlab=input$xaxis, ylab=input$yaxis)
      title(main=input$plotTitle)
      legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
             col=c(1, 2), bty="n")
      abline(v=TPanalysis()[[2[1]]], col="blue")
    })

    output$ewsOne <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }

      switch(input$dataType_2,
             "Prey" =  selectInput("ewsType", "Method:",
                        choice=c(" ", "Generic Early Warning Signals")
             ),
             "Predator" =  selectInput("ewsType", "Method:",
                        choice=c(" ", "Generic Early Warning Signals")
             )
      )
    })

    output$e1 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      selectInput("detrending", label="Detrended/filtered prior to analysis:",
                  choice=c("no", "gaussian", "loess", "linear", "first-diff"))
    })

    output$e2 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }

      if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      numericInput("bandwidth", label="Bandwidth used for the Gaussian kernel when gaussian filtering
                  is applied. It is expressed as percentage of the timeseries length
                  (must be numeric between 0 and 100):",
                  value=5)
    })

    output$e3 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      numericInput("winsize", label="The size of the rolling window expressed as
                percentage of the timeseries length (must be numeric between 0 and 100):",
                value=50)
    })

    output$e4 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      numericInput("span", label="Parameter that controls the degree of smoothing
                (numeric between 0 and 100):",
                value=25)
    })

    output$e5 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      numericInput("degree", label="The degree of polynomial to be used for when loess
                fitting is applied, normally 1 or 2:",
                value=2)
    })

    output$e6 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      selectInput("logtransform", label="If TRUE data are logtransformed prior to analysis as log(X+1):",
                choice=c(FALSE, TRUE))
    })

    output$e7 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      selectInput("interpolate", label="If TRUE linear interpolation is applied to produce a timeseries of equal length as
                the original. (FALSE assumes there are no gaps in the timeseries):",
                choice=c(FALSE, TRUE))
    })

    output$e8 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      selectInput("AR_n", label="If TRUE the best fitted AR(n) model is fitted to the data:",
                  choice=c(FALSE, TRUE))
    })

    output$e9 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }
      else if(input$ewsType != "Generic Early Warning Signals"){
        return()
      }

      selectInput("powerspectrum", label="If TRUE the power spectrum within each rolling window is plotted:",
                  choice=c(FALSE, TRUE))
    })

    output$ewsRun <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }

      actionButton("run_2", "Run")
    })

    EWSanalysis <- eventReactive(input$run_2, function(){
      if(input$dataType_2 == "Prey"){
        ews <- generic_ews(timeseries=subset(theModel(), select=prey), winsize=input$winsize,
                          detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                          degree=input$degree, logtransform=input$logtransform,
                          interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
        return(ews)
      }
      else if(input$dataType_2 == "Predator"){
        ews <- generic_ews(timeseries=subset(theModel(), select=predator), winsize=input$winsize,
                           detrending=input$detrending, bandwidth=input$bandwidth, span=input$span,
                           degree=input$degree, logtransform=input$logtransform,
                           interpolate=input$interpolate, AR_n=input$AR_n, powerspectrum=input$powerspectrum)
        return(ews)
      }
    })

    output$ewsPlot <- renderPlot({
        EWSanalysis()
    }, height=850, width=1200)

    output$ewsTable <- renderDataTable({
      EWSanalysis()
    }, options=list(pageLength=5))

    add_ewsTableGuide <- eventReactive(input$run_2, function(){
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

    output$ewsTableGuide <- renderText({
        add_ewsTableGuide()
    })

  } # End
)
