### Lotka-Volterra predator-prey model
# server.R

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

    output$tpOne_2 <- renderUI({
      if(is.null(input$dataType_2)){
        return()
      }
      else if(input$dataType_2 == " "){
        return()
      }

      switch(input$dataType_2,
             "Prey" =  selectInput("ewsType", "Method:",
                        choice=c(" ", "Quick Detection for Generic Early Warning Signals",
                                 "Generic Early Warning Signals")
             ),
             "Predator" =  selectInput("ewsType", "Method:",
                        choice=c(" ", "Quick Detection for Generic Early Warning Signals",
                                 "Generic Early Warning Signals")
             )
      )
    })

    output$tpTwo_2 <- renderUI({
      if(is.null(input$dataType_2) || input$dataType_2 == " "){
        return()
      }
      if(is.null(input$ewsType) || input$ewsType == " "){
        return()
      }

      selectInput("ok", "ok", choice=c("a", "b"))
    })

    output$ewsRun <- renderUI({
      actionButton("run_2", "Run")
    })

    ewsResults <- eventReactive(input$run_2, function(){
      #ewsInfo <- qda_ews(theModel()[,1], param=NULL, winsize=50, detrending='gaussian', bandwidth=NULL,
       #       boots=50, s_level=0.05, cutoff=0.05, detection.threshold=0.002, grid.size=50,
        #      logtransform=FALSE, interpolate=FALSE)

      return()
    })

    output$ewsAnalysis <- renderText({
      #ewsResults()
    })

  } # End
)
