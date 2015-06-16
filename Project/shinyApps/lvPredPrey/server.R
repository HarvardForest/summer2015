### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Last edited: 15June2015

### Lotka-Volterra Predator-Prey ###

# load dependencies
source("global.R", local=TRUE)

## start server ##
shinyServer(
  function(input, output, session){

    # declare instance of the simulation
    lvPredPrey <- reactive({
     lvPredPreyModel(seq(0, input$time, by=1),
                  c(prey=input$prey, predator=input$predators),
                  c(alpha=input$alpha, beta=input$beta, delta=input$delta,
                    gamma=input$gamma))
    })

################# Side Panel ###################################################
   
    ### load user-input boxes ###
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
    ## end load user-input boxes ##

    # link user-input box values with respective slider values
    observe({
      updateSliderInput(session, "alpha", value=input$alpha2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "delta", value=input$delta2)
      updateSliderInput(session, "gamma", value=input$gamma2)
    })

################################################################################

############## Display dynamic plot and table of the simulation ################

    # predator-prey simulation plot
    output$mainPlot <- renderPlot({
      matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis)
      title(main=input$plotTitle)
      legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
             col=c(1, 2), bty="n")
    })

    # predator-prey simulation data table
    output$mainTable <- renderDataTable({
      mNew <- cbind(time=0:(input$time), lvPredPrey())
    })

################################################################################

#################### Tipping Point Analysis ####################################

    ### build user-input interface ###

    output$tp1 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      switch(input$dataType,
        "Prey" = selectInput("breakpointType", "Analysis:",
                    choice=c(" ", "with Negative Binomial Distribution",
                            "for Continuous Data",
                            "with Zero-Inflated Negative Binomial Distribution")
                  ),

        "Predator" = selectInput("breakpointType", "Analysis:",
                      choice=c(" ", "with Negative Binomial Distribution", 
                              "for Continuous Data",
                              "with Zero-Inflated Negative Binomial Distribution")
                      )
      ) # switch
    })

    output$tp2 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }

      switch(input$breakpointType,
        "with Negative Binomial Distribution" = 
          selectInput("distributionType", 
            "Distribution to simulate break-point locations:",
            choice=c(" ", "Four Parameter Beta Distribution", 
                    "Truncated Normal Distribution")
          ),

        "for Continuous Data" = 
          selectInput("distributionType", 
            "Distribution to simulate break-point locations:",
            choice=c(" ", "Four Parameter Beta Distribution", 
                    "Truncated Normal Distribution")
          ),

        "with Zero-Inflated Negative Binomial Distribution" = 
          selectInput("distributionType", 
            "Distribution to simulate break-point locations:",
            choice=c(" ", "Four Parameter Beta Distribution", 
                    "Truncated Normal Distribution")
          )
      ) # switch
    })

    output$tp3 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("Nmax", "Maximum number of break-points:", value=10)
    })

    output$tp4 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("a", "Used in the four parameter beta distribution to smooth
        both shape parameters. When simulating from the truncated normal
        distribution, this value is used to smooth the estimates of the mean 
        values:", value=0.8)
    })

    output$tp5 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("b", "A smoothing parameter value. It is used in the truncated
                   normal distribution to smooth the estimates of the standard
                   deviation:", value=0.8)
    })

    output$tp6 <- renderUI({
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("h", "Minimum aberration width:", value=5)
    })

    output$tp7 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("M", "Sample size to be used in simulating the locations of 
        break-points:", value=200)
    })

    output$tp8 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("rho", "The fraction which is used to obtain the best performing
                   set of sample solutions (i.e., elite sample):", value=0.05)
    })

    output$tp9 <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      numericInput("eps", "the cut-off value for the stopping criterion in the 
        CE method:", value=0.01)
    })

    output$tpRun <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      actionButton("tpRun", "Run")
    })

    ### end user-input interface ###

    ### display tipping point analysis ###

    # run tipping point analysis based on user's parameters
    TPanalysis <- eventReactive(input$tpRun, function(){
      ## loading window
      withProgress(message="Analyzing breakpoints", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          ## for prey
          if(input$dataType == "Prey"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(lvPredPrey()[1], distyp=1, parallel=TRUE, Nmax=input$Nmax, 
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a, 
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(lvPredPrey()[1], distyp=2, parallel=TRUE, Nmax=input$Nmax, 
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a, 
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(lvPredPrey()[1], distyp=1, parallel=TRUE, 
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(lvPredPrey()[1], distyp=2, parallel=TRUE, 
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == 
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.Normal(lvPredPrey()[1], distyp=1, parallel=TRUE, 
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.Normal(lvPredPrey()[1], distyp=2, parallel=TRUE, 
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          ## for predator
          else if(input$dataType == "Predator"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(lvPredPrey()[2], distyp=1, parallel=TRUE, Nmax=input$Nmax, 
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(lvPredPrey()[2], distyp=2, parallel=TRUE, Nmax=input$Nmax, 
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a, 
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(lvPredPrey()[2], distyp=1, parallel=TRUE, 
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(lvPredPrey()[2], distyp=2, parallel=TRUE, 
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType == 
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.Normal(lvPredPrey()[2], distyp=1, parallel=TRUE, 
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.Normal(lvPredPrey()[2], distyp=2, parallel=TRUE, 
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M, 
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }
        }) # withProgress
      }) # withProgress
    })

    # display "Number of breakpoints detected:" text
    output$numBreakpointsText <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # display text only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        "Number of breakpoints detected:"
      }
    })

    # display number of breakpoints detected
    output$tpAnalysis1 <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      TPanalysis()[[1]]
    })

    # display "Location:" text
    output$locationText <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # display text only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        "Location(s):"
      }
    })

    # display locations of detected breakpoints
    output$tpAnalysis2 <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # display only if breakpoints are detected
      if(length(TPanalysis()) > 1){
       paste(TPanalysis()[[2]], collapse=", ")
      }
    })

    # generate new instance of simulation plot to draw breakpoint lines
    plot_breakpoints <- eventReactive(input$tpRun, function(){
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        lvPredPrey()
      }
    })

    # generate plot output for breakpoints plot
    load_BreakpointPlotSlot <- eventReactive(input$tpRun, function(){
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        plotOutput("breakpointPlot")
      }
    })

    # display plot output for breakpoints plot
    output$breakpointPlotSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        load_BreakpointPlotSlot()
      }
    })

    # display plot breakpoint lines
    output$breakpointPlot <- renderPlot({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # display only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        matplot(plot_breakpoints(), type="l", xlab=input$xaxis, ylab=input$yaxis)
        title(main="Breakpoints")
        legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
              col=c(1, 2), bty="n")
        # add breakpoint lines
        abline(v=TPanalysis()[[2[1]]], col="blue")
      }
    })

    # generate title text for profile plot
    load_ProfileTitle <- eventReactive(input$tpRun, function(){
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        "Mean Profile Plot"
      }
    })

    # display title text for profile plot
    output$profilePlotTitleSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        textOutput("profilePlotTitle")
      }
    })

    # display mean plot title
    output$profilePlotTitle <- renderText({
            # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        load_ProfileTitle()
      }
    })

    # run profile plot from breakpoint package
    run_ProfilePlot <- eventReactive(input$tpRun, function(){
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected      
      if(length(TPanalysis()) > 1){
        if(input$dataType == "Prey"){
          profilePlot(TPanalysis(), lvPredPrey()[1], x.label=input$dataType, 
            y.label="Population")
        }
        else if(input$dataType == "Predator"){
          profilePlot(TPanalysis(), lvPredPrey()[2], x.label=input$dataType, 
            y.label="Population")
        }
      }
    })

    # load profile plot output
    load_ProfilePlot <- eventReactive(input$tpRun, function(){
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected      
      if(length(TPanalysis()) > 1){
        plotOutput("profilePlot")
      }
    })

    # display profile plot output
    output$profilePlotSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected      
      if(length(TPanalysis()) > 1){
        load_ProfilePlot()
      }
    })

    # display profile plot
    output$profilePlot <- renderPlot({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      if(is.null(input$distributionType) || (input$distributionType == " ")){
        return()
      }

      # run only if breakpoints are detected      
      if(length(TPanalysis()) > 1){
        run_ProfilePlot()
      }
    })

################################################################################

################## Earlywarnings Analysis ######################################

    ## Input for Quick Detection Analysis for Generic Early Warning Signals ##
    ##          and                                                         ##
    ##            Generic Early Warning Signals                             ##

    output$ews1 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }

      switch(input$ewsDataType,
        "Prey" = selectInput("ewsMethod", "Method:",
                   choice=c(" ", 
                    "Quick Detection Analysis for Generic Early Warning Signals",
                    "Generic Early Warning Signals")
                    #"Potential Analysis for univariate data")
                  ),

        "Predator" = selectInput("ewsMethod", "Method:",
                      choice=c(" ", 
                        "Quick Detection Analysis for Generic Early Warning Signals",
                        "Generic Early Warning Signals")
                        #"Potential Analysis for univariate data")
                      )
      ) # switch
    })

    output$ews2 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }

      selectInput("detrending", label="Detrended/filtered prior to analysis:",
                  choice=c(" ", "gaussian", "loess", "linear", "first-diff", "no"))
    })

    output$ews3 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("bandwidth", 
        label="Bandwidth used for the Gaussian kernel when gaussian filtering
                  is applied. It is expressed as percentage of the timeseries 
                  length (must be numeric between 0 and 100):", value=5)
    })

    output$ews4 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("winsize", label="The size of the rolling window expressed as
                  percentage of the timeseries length (must be numeric between 
                  0 and 100):", value=50)
    })

        ### Input specific to Generic Early Warning Signals ###

    output$ews5 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("span", label="Parameter that controls the degree of 
                  smoothing (numeric between 0 and 100):", value=25)
    })

    output$ews6 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("degree", label="The degree of polynomial to be used for 
                  when loess fitting is applied, normally 1 or 2:", value=2)
    })

    output$ews7 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("logtransform", label="If TRUE data are logtransformed prior 
                  to analysis as log(X+1):", choice=c(FALSE, TRUE))
    })

    output$ews8 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("interpolate", label="If TRUE linear interpolation is applied 
                  to produce a timeseries of equal length as the original. 
                  (FALSE assumes there are no gaps in the timeseries):",
                  choice=c(FALSE, TRUE))
    })


    output$ews9 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("AR_n", label="If TRUE the best fitted AR(n) model is fitted 
                  to the data:", choice=c(FALSE, TRUE))
    })

    output$ews10 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("powerspectrum", label="If TRUE the power spectrum within each
                  rolling window is plotted:", choice=c(FALSE, TRUE))
    })

### Input specific to Quick Detection Analysis for Generic Early Warning Signals ###

    output$ews11 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("boots", label="The number of surrogate data to generate from
                  fitting an ARMA(p,1) model:", value=100)
    })

    output$ews12 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("s_level", label="Significance level:", value=0.05)
    })

    output$ews13 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("cutoff", label="The cutoff value to visualize the potential
                  landscape:", value=0.05)
    })

    output$ews14 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("detection.threshold", label="Detection threshold for
                  potential minima:", value=0.002)
    })

    output$ews15 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("grid.size", label="Grid size (for potential analysis):",
                   value=50)
    })

    output$ews16 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("logtransform", label="If TRUE, data are logtransformed prior
                  to analysis as log(X+1):", choice=c(FALSE, TRUE))
    })

    output$ews17 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != 
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("interpolate", label="If TRUE, linear interpolation is applied
                  to produce timeseries of equal length as the original 
                  (Default FALSE assumes there are no gaps in the timeseries:",
                  choice=c(FALSE, TRUE))
    })

    ### Input specific to Potential Analysis for univariate data ###

    output$ews18 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("std", label="Standard Deviation of the noice (this will set 
                  scaled potentials):", value=1)
    })

    output$ews19 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      selectInput("bw", label="Bandwidth for kernel estimation:",
                  choice=c("nrd"))
    })

    output$ews20 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("detection.threshold", label="Maximum detection threshold as 
                  fraction of density kernel height:", value=0.01)
    })

    output$ews21 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("bw.adjust", label="The real bandwidth will be this value 
                  multiplied by the bandwidth for kernel estimation:", value=1)
    })

    output$ews22 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("density.smoothing", label="Add a small constant density 
                  across the whole observation range to regularize density 
                  estimation (and to avoid zero probabilities within the 
                  observation range). This parameter adds uniform density 
                  across the observation range, scaled by density.smoothing:", 
                  value=0)
    })

    output$ews23 <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Potential Analysis for univariate data"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      numericInput("detection.limit", label="Ignore maxima that are below 
                  this value multiplied by maximum density:", value=0.1)
    })

    # display ews run button
    output$ewsRun <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      actionButton("ewsRun", "Run")
    })

    ### end user-input interface ###

    ###### Earlywarnings Output ######

    # run early warnings analysis based on user input
    EWSanalysis <- eventReactive(input$ewsRun, function(){
      # loading window
      withProgress(message="Detecting Early Warning Signals", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            if(input$ewsMethod == "Generic Early Warning Signals"){
              ews <- generic_ews(timeseries=subset(lvPredPrey(), select=prey), 
                        winsize=input$winsize, detrending=input$detrending, 
                        bandwidth=input$bandwidth, span=input$span, degree=input$degree, 
                        logtransform=input$logtransform, interpolate=input$interpolate, 
                        AR_n=input$AR_n, powerspectrum=input$powerspectrum)

            }
            else if(input$ewsMethod == 
              "Quick Detection Analysis for Generic Early Warning Signals"){
                ews <- qda_ews(timeseries=lvPredPrey()[1], winsize=input$winsize,
                        detrending=input$detrending, bandwidth=input$bandwidth, 
                        boots=input$boots, s_level=input$s_level, 
                        cutoff=input$cutoff, detection.threshold=input$detection.threshold,
                        grid.size=input$grid.size, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$ewsMethod == "Potential Analysis for univariate data"){
              ews <- livpotential_ews(x=subset(lvPredPrey(), select=prey),
                        std=input$std, bw=input$bw, 
                        detection.threshold=input$detection.threshold,
                        bw.adjust=input$bw.adjust, density.smoothing=input$density.smoothing,
                        detection.limit=input$detection.limit)
            }
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            if(input$ewsMethod== "Generic Early Warning Signals"){
              ews <- generic_ews(timeseries=subset(lvPredPrey(), select=predator), 
                        winsize=input$winsize, detrending=input$detrending, 
                        bandwidth=input$bandwidth, span=input$span, degree=input$degree, 
                        logtransform=input$logtransform, interpolate=input$interpolate, 
                        AR_n=input$AR_n, powerspectrum=input$powerspectrum)
            }
            else if(input$ewsMethod ==
              "Quick Detection Analysis for Generic Early Warning Signals"){
                ews <- qda_ews(timeseries=lvPredPrey()[2], winsize=input$winsize,
                        detrending=input$detrending, bandwidth=input$bandwidth, 
                        boots=input$boots, s_level=input$s_level, 
                        cutoff=input$cutoff, detection.threshold=input$detection.threshold,
                        grid.size=input$grid.size, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$ewsMethod == "Potential Analysis for univariate data"){
              ews <- livpotential_ews(x=subset(lvPredPrey(), select=predator),
                        std=input$std, bw=input$bw, 
                        detection.threshold=input$detection.threshold,
                        bw.adjust=input$bw.adjust, density.smoothing=input$density.smoothing,
                        detection.limit=input$detection.limit)
            }
          }
        }) # withProgress
      }) # withProgress
    })

    ### Generic Early Warning Signals Output ###

    # load guide slot for ews data table
    load_generic_ewsTableGuide <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      verbatimTextOutput("generic_ewsTableGuide")
    })

    # display guide slot for ews data table
    output$generic_ewsTableGuideSlot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_generic_ewsTableGuide()
    })

    # display text for ews data table guide
    output$generic_ewsTableGuide <- renderText({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

"tim = the time index.

ar1 = the autoregressive coefficient ar(1) of a first order AR model fitted on
  the data within the rolling window.

sd = the standard deviation of the data estimated within each rolling window.

sk = the skewness of the data estimated within each rolling window.

kurt = the kurtosis of the data estimated within each rolling window.

cv= the coefficient of variation of the data estimated within each rolling window.

returnrate = the return rate of the data estimated as 1-ar(1) cofficient within 
  each rolling window.

densratio	= the density ratio of the power spectrum of the data estimated as the 
  ratio of low frequencies over high frequencies within each rolling window.

acf1 = the autocorrelation at first lag of the data estimated within each
  rolling window."
    })

    # load ews Data Table info
    load_generic_ewsTable <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      EWSanalysis()
    })

    # load ews Data Table slot
    output$generic_ewsTableSlot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      dataTableOutput("generic_ewsDataTable")
    })

    # display ews Data Table
    output$generic_ewsDataTable <- renderDataTable({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_generic_ewsTable()
    }, options=list(pageLength=5))

    # load ews plot
    generic_ewsPlotLoad <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- 
              generic_ews(timeseries=subset(lvPredPrey(), select=prey),
                winsize=input$winsize, detrending=input$detrending, 
                bandwidth=input$bandwidth, span=input$span, degree=input$degree,
                logtransform=input$logtransform, interpolate=input$interpolate,
                AR_n=input$AR_n, powerspectrum=input$powerspectrum)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- 
              generic_ews(timeseries=subset(lvPredPrey(), select=predator),
                winsize=input$winsize, detrending=input$detrending, 
                bandwidth=input$bandwidth, span=input$span, degree=input$degree,
                logtransform=input$logtransform, interpolate=input$interpolate,
                AR_n=input$AR_n, powerspectrum=input$powerspectrum)
          }
        })
      })
    })

    # load ews plot output
    load_generic_ewsPlotSlot <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      plotOutput("generic_ewsPlot", width="100%", height="100%")
    })

    # display ews plot output
    output$generic_ewsPlotSlot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_generic_ewsPlotSlot()
    })

    # display ews plot
    output$generic_ewsPlot <- renderPlot({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod != "Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      generic_ewsPlotLoad()
    }, height=850, width=1200)

    ##### End Generic Early Warning Signals Output #####

    ##### Start Quick Detection Analysis for Generic Early Warning Signals #####
    #####                       Output                                    ######

    load_qda_ewsDetail <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      textOutput("qda_ewsDetail")     
    })

    output$qda_ewsDetailSlot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsDetail()
    })

    output$qda_ewsDetail <- renderText({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      "The first plot contains the original data, the detrending/filtering 
      applied and the residuals (if selected), autocorrelation, and variance. 
      For each statistic, trends are estimated by the nonparametric Kendall tau 
      correlation. The second plot, returns a histogram of the distributions of
      the Kendall trend statistic for autocorrelation and variance estimated on
      the surrogated data. Vertical lines represent the level of significance,
      whereas the black indicates the actual trend found in the time series. The
      third plot is the reconstructed potential landscape in 2D. In addition,
      the function returns a list containing the output from the respective
      functions generic_RShiny (indicators); surrogates_RShiny (trends);
      movpotential_ews (potential analysis). (Dakos et al. 2012)"
    })

    ### Start load Quick Detection Analysis plots ###

    load_qda_ewsPlot1 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- 
              plot_generic_RShiny(timeseries=lvPredPrey()[1], winsize=input$winsize,
                  detrending=input$detrending, bandwidth=input$bandwidth,
                  logtransform=input$logtransform, interpolate=input$interpolate,
                  AR_n=FALSE, powerspectrum=FALSE)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- 
              plot_generic_RShiny(timeseries=lvPredPrey()[2], winsize=input$winsize,
                  detrending=input$detrending, bandwidth=input$bandwidth,
                  logtransform=input$logtransform, interpolate=input$interpolate,
                  AR_n=FALSE, powerspectrum=FALSE)
          }
        }) # withProgress
      }) # withProgress
    })

    load_qda_ewsPlot2 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- 
              surrogates_RShiny(timeseries=lvPredPrey()[1], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                boots= input$boots, s_level=input$s.level,
                logtransform=input$logtransform, interpolate=input$interpolate)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- 
              surrogates_RShiny(timeseries=lvPredPrey()[2], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                boots= input$boots, s_level=input$s.level,
                logtransform=input$logtransform, interpolate=input$interpolate)
          }
        }) # withProgress
      }) # withProgress
    })

    load_qda_ewsPlot3 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="This may take awhile", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- 
              movpotential_ews(as.vector(lvPredPrey()[1][, 1]), param=input$param,
                detection.threshold=input$detection.threshold, 
                grid.size=input$grid.size, plot.cutoff=input$cutoff)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- 
              movpotential_ews(as.vector(lvPredPrey()[2][, 1]), param=input$param,
                detection.threshold=input$detection.threshold,
                grid.size=input$grid.size, plot.cutoff=input$cutoff)
          }
        })
      })
    })

    ### End load Quick Detection Analysis plots ###

    ### Start display Quick Detection Analysis plots ###

    load_qda_ewsPlot1Slot <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      plotOutput("qda_ewsPlot1")
    })

    output$qda_ewsPlot1Slot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsPlot1Slot()
    })

    output$qda_ewsPlot1 <- renderPlot({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsPlot1()
    })

    load_qda_ewsPlot2Slot <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      plotOutput("qda_ewsPlot2")
    })

    output$qda_ewsPlot2Slot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsPlot2Slot()
    })


    output$qda_ewsPlot2 <- renderPlot({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsPlot2()
    })

    load_qda_ewsPlot3Slot <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      plotOutput("qda_ewsPlot3")
    })

    output$qda_ewsPlot3Slot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsPlot3Slot()
    })


    output$qda_ewsPlot3 <- renderPlot({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      print(load_qda_ewsPlot3())
    })

    ### End display Quick Detection Analysis plots ###

    ### Start load Quick Detection Analysis data output ###      

    load_qda_ewsData1 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Analyzing Data", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <- 
              generic_RShiny(timeseries=lvPredPrey()[1], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                logtransform=input$logtransform, interpolate=input$interpolate,
                AR_n=FALSE, powerspectrum=FALSE)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <- 
              generic_RShiny(timeseries=lvPredPrey()[2], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                logtransform=input$logtransform, interpolate=input$interpolate,
                AR_n = FALSE, powerspectrum = FALSE)
          }
        }) # withProgress
      }) # withProgress
    })

    # load ews plot
    load_qda_ewsDataLoad2 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      withProgress(message="Analyzing Data", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {
          # for prey
          if(input$ewsDataType == "Prey"){
            ews <-
              surrogates_RShiny(timeseries=lvPredPrey()[1], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                boots= input$boots, s_level=input$s.level,
                logtransform=input$logtransform, interpolate=input$interpolate)
          }

          # for predator
          else if(input$ewsDataType == "Predator"){
            ews <-
              surrogates_RShiny(timeseries=lvPredPrey()[2], winsize=input$winsize,
                detrending=input$detrending, bandwidth=input$bandwidth,
                boots= input$boots, s_level=input$s.level, 
                logtransform=input$logtransform, interpolate=input$interpolate)
          }
        }) # withProgress
      }) # withProgress
    })

    ### End load Quick Detection Analysis data output ###

    ### Start display Quick Detection Analysis data output ###

    render_qda_ewsData1 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      dataTableOutput("qda_ewsData1")
    })

    output$qda_ewsData1Slot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      render_qda_ewsData1()
    })

    output$qda_ewsData1 <- renderDataTable({
            # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsData1()
    }, options=list(pageLength=5))

    render_qda_ewsData2 <- eventReactive(input$ewsRun, function(){
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      verbatimTextOutput("qda_ewsData2")
    })

    output$qda_ewsData2Slot <- renderUI({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      render_qda_ewsData2()
    })

    output$qda_ewsData2 <- renderPrint({
      # check required information
      if(is.null(input$ewsDataType) || input$ewsDataType == " "){
        return()
      }
      else if(is.null(input$ewsMethod) || input$ewsMethod == " "){
        return()
      }
      else if(input$ewsMethod !=
        "Quick Detection Analysis for Generic Early Warning Signals"){
        return()
      }
      else if(is.null(input$detrending) || input$detrending == " "){
        return()
      }

      load_qda_ewsDataLoad2()
    })

    ##### End display Quick Detection Analysis data output #####

    ##### Start Potential Analysis for univariate data #####



    ##### End Potential Analysis for univariate data #####

  } ## end server ##
)
