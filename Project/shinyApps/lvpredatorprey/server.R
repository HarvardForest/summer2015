################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 17July2015 ##################################
################################################################################
################################################################################

###### Shiny server ######

## load dependencies ##
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

################################################################################
################# Side Panel ###################################################
################################################################################

    ### start: load user input-boxes (parameters) ###

    output$alpha2 <- renderUI({
      numericInput("alpha2", label=NULL, value=input$alpha)
    })

    output$beta2 <- renderUI({
      numericInput("beta2", label=NULL, value=input$beta)
    })

    output$delta2 <- renderUI({
      numericInput("delta2", label=NULL, value=input$delta)
    })

    output$gamma2 <- renderUI({
      numericInput("gamma2", label=NULL, value=input$gamma)
    })

    ### end: load user input-boxes (parameters) ###

    # link user input-box values with respective slider values (parameters)
    observe({
      updateSliderInput(session, "alpha", value=input$alpha2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "delta", value=input$delta2)
      updateSliderInput(session, "gamma", value=input$gamma2)
    })

################################################################################
################################################################################
################################################################################

################################################################################
########### Display dynamic plot (main) of the simulation ######################
################################################################################

    output$mainPlot <- renderPlot({

############ start: display default simulation plots  ##########################

      ### start: show only a default plot for 'customize graph' panel ###

      if(input$tabset_analyses == "Customize Graph"){
        matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis)
          title(main=input$plotTitle)
          legend("topleft", c(input$preyLabel, input$predatorLabel),
                 lty=c(1, 2), col=c("black", "red"), bty="n")
      }

      ### end: show only a default plot for 'customize graph' panel ###

      ### start: show default plot for 'quick analysis' panel ###

      else if(input$tabset_analyses == "Quick Analysis"){
        # this check removes a transient error
        if(is.null(input$quickPlotOptions)){
          return()
        }

        # generate default plot based on radio-selection
        if(input$quickPlotOptions == "Both"){
          matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=c(1, 1), col=c("black", "red"))
          title(main=input$plotTitle)
          legend("topleft", c(input$preyLabel, input$predatorLabel),
                 lty=c(1, 1), col=c("black", "red"), bty="n")
        }
        else if(input$quickPlotOptions == "Prey"){
          matplot(lvPredPrey()[1], type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=1, col="black")
          title(main=input$preyLabel)
          legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
        }
        else if(input$quickPlotOptions == "Predator"){
          matplot(lvPredPrey()[2], type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=1, col="red")
          title(main=input$predatorLabel)
          legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
        }
      }

      ### end: show default plot for 'quick analysis' panel ###

      ### start: show default for 'advanced analysis' panel ###

      if(input$tabset_analyses == "Advanced Analysis"){
        # this check removes a transient error
        if(is.null(input$advancedPlotOptions)){
          return()
        }

        # generate default plot based on radio-selection
        if(input$advancedPlotOptions == "Both"){
          matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=c(1, 1), col=c("black", "red"))
          title(main=input$plotTitle)
          legend("topleft", c(input$preyLabel, input$predatorLabel),
                 lty=c(1, 1), col=c("black", "red"), bty="n")
        }
        else if(input$advancedPlotOptions == "Prey"){
          matplot(lvPredPrey()[1], type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=1, col="black")
          title(main=input$preyLabel)
          legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
        }
        else if(input$advancedPlotOptions == "Predator"){
          matplot(lvPredPrey()[2], type="l", xlab=input$xaxis, ylab=input$yaxis,
                  lty=1, col="red")
          title(main=input$predatorLabel)
          legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
        }
      }

      ### end: show default for 'advanced analysis' panel ###

############# end: display default simulation plots  ###########################

#### start: draw breakpoint lines on main plot for 'quick analysis' panel ######

      # run only if the "Quick Analysis" tab is active
      if(input$tabset_analyses == "Quick Analysis"){
        if(is.null(input$quick_decomposeOptions)
          || input$quick_decomposeOptions == "Observed (Simulated Data)"
          || input$quick_decomposeOptions == " "){

          # draw new instance of basic plot based on state variable selection
          if(input$quick_dataType == "Prey"){
            matplot(lvPredPrey()[1], type="l", xlab=input$xaxis,
                    ylab=input$yaxis, lty=1, col="black")
            title(main=input$preyLabel)
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$quick_dataType == "Predator"){
            matplot(lvPredPrey()[2], type="l", xlab=input$xaxis,
                    ylab=input$yaxis, lty=1, col="red")
            title(main=input$predatorLabel)
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines and ews lines can be drawn
          if(is.null(input$quick_dataType) || input$quick_dataType == " "){
            return()
          }
          # indicates breakpoint lines (only) can be drawn
          else if(!is.null(input$quick_breakpointsCheckbox)
                  && input$quick_breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend
            if(input$quick_dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                     col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

##### end: draw breakpoint lines on main plot for 'quick analysis' panel #######

##### start: update plot and legend with ews line (quick analysis) #############

          # draw ews line based on radio button selection

######### start: draw ews line for 'observed' (quick analysis) #################

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$quick_ewsRadioButtons)
             || input$quick_ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$quick_ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

################ end: draw ews line for 'observed' (quick analysis) ############

########### start: draw ews line for 'trend' (quick analysis) ##################

        else if(input$quick_decomposeOptions == "Trend"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$quick_dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$quick_frequency))
            plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Trend")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$quick_dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$quick_frequency))
            plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Trend")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$quick_breakpointsCheckbox)
            && input$quick_breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend
            if(input$quick_dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$quick_ewsRadioButtons)
             || input$quick_ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$quick_ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############# end: draw ews line for 'trend' (quick analysis) ##################

########### start: draw ews line for 'seasonal' (quick analysis) ###############

        else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$quick_dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$quick_frequency))
            plot(decomposed$seasonal, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Seasonal (Periodicity)")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$quick_dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$quick_frequency))
            plot(decomposed$seasonal, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Seasonal (Periodicity)")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$quick_breakpointsCheckbox)
            && input$quick_breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend
            if(input$quick_dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$quick_ewsRadioButtons)
             || input$quick_ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$quick_ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############# end: draw ews line for 'seasonal' (quick analysis) ###############

############# start: draw ews line for 'random' (quick analysis) ###############

        else if(input$quick_decomposeOptions == "Random (Residuals)"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$quick_dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$quick_frequency))
            plot(decomposed$random, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Random (Residuals)")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$quick_dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$quick_frequency))
            plot(decomposed$random, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Random (Residuals)")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$quick_breakpointsCheckbox)
            && input$quick_breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend
            if(input$quick_dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$quick_ewsRadioButtons)
             || input$quick_ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$quick_ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$quick_dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$quick_dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$quick_ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############### end: draw ews line for 'random' (quick analysis) ###############

      } # end tabset_quickAnalysis

####### end: update plot and legend with ews line (quick analysis) #############

    }) # end render_plot (main)

################################################################################
################################################################################
################################################################################

################################################################################
################ Display plot selection options (main plot) ####################
################################################################################

    # display plot selection options
    output$plotOptionsSlot <- renderUI({
      # one copy of radio buttons for each panel
      if(input$quick_dataType == " "){
        if(input$tabset_analyses == "Quick Analysis"){
          radioButtons("quickPlotOptions", "Display:",
                        choices=c("Prey", "Predator", "Both"),
                        selected="Both", inline=TRUE)
        }
      }
      else if(input$tabset_analyses == "Advanced Analysis"){
        radioButtons("advancedPlotOptions", "Display:",
                      choices=c("Prey", "Predator", "Both"),
                      selected="Both", inline=TRUE)
      }
    })

################################################################################
################################################################################
################################################################################

################################################################################
################# Load and display main data table #############################
################################################################################

    # simulation data table (main table)
    output$mainTable <- renderDataTable({
      lvPredPrey()
    })

    # download main table feature (with button)
    output$downloadMainTable <- downloadHandler(
      filename = function() { paste("PredPreySimulatedData", '.csv', sep='') },
      content = function(file) {
        write.csv(lvPredPrey(), file)
      }
    )

################################################################################
################################################################################
################################################################################

################################################################################
######################### Quick Analysis #######################################
################################################################################

############ start: build responsive user-input widgets ########################

    output$quick_decomposeOptionsSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }

      selectInput("quick_decomposeOptions", "Select a component for analysis:",
                    choices=c(" ", "Observed (Simulated Data)", "Trend",
                              "Seasonal (Periodicity)", "Random (Residuals)"))
    })

    output$quick_frequencySlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }

      numericInput("quick_frequency",
                   "The number of observations per unit of time (frequency) for
                      Decomposition analysis:",
                    value=2)
    })

    output$quick_NmaxSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }

      numericInput("quick_Nmax",
                    "Maximum number of breakpoints for Tipping Point analysis:",
                    value=10)
    })

    output$quick_winsizeSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }

      numericInput("quick_winsize",
                    "Size of the rolling window used in the Early Warning Signals
                      analysis (expressed as a percentage of the timeseries):",
                  value=50)
    })

    output$quick_runButtonSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      actionButton("quick_runButton", "Run Analysis")
    })

############## end: build responsive user-input widgets ########################

############# start: display decomposition plot (quick analysis) ###############

    output$quick_decomposePlotSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }

      plotOutput("quick_decomposePlot")
    })

    output$quick_decomposePlot <- renderPlot({
      if(input$quick_dataType == "Prey"){
        plot(decompose(ts(lvPredPrey()[[1]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Predator"){
        plot(decompose(ts(lvPredPrey()[[2]], frequency=input$quick_frequency)))
      }
    })

############# end: display decomposition plot (quick analysis) #################

########## start: predetermined (quick) breakpoint analysis ####################

    quickTP <- eventReactive(input$quick_runButton, {
      # check required information
      if(is.null(input$quick_runButton)){
        return()
      }

      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          # for prey
          if(input$quick_dataType == "Prey"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[1],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$x)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$trend)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$seasonal)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$random)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[2],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$x)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$trend)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$seasonal)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              # wrap appropriate component into a data-frame for breakpoint method
              component <- data.frame(decomposed$random)
              # run breakpoint method on component
              CE.Normal(component[1], distyp=1, parallel=FALSE,
                        Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                        a=0.8, b=0.8)
            }
          }

        }) # withProgress
      }) # withProgress
    })

############## end: predetermined (quick) breakpoint analysis ##################

############## start: predetermined (quick) ews analysis #######################

    quickGeneric <- eventReactive(input$quick_runButton, {
      # check required information
      if(is.null(input$quick_runButton)){
        return()
      }

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          # for prey
          if(input$quick_dataType == "Prey"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[1],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=decomposed$x[1:input$time],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              # range is offset because head and tail values are NA
              generic_ews(timeseries=decomposed$trend[3:input$time-1],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              generic_ews(timeseries=decomposed$seasonal[1:input$time],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              # range is offset because head and tail values are NA
              generic_ews(timeseries=decomposed$random[3:input$time-1],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[2],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=decomposed$x[1:input$time],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              # range is offset because head and tail values are NA
              generic_ews(timeseries=decomposed$trend[3:input$time-1],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              generic_ews(timeseries=decomposed$seasonal[1:input$time],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              # range is offset because head and tail values are NA
              generic_ews(timeseries=decomposed$random[3:input$time-1],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
          }

        }) # withProgress
      }) # withProgress
    })

################ end: predetermined (quick) ews analysis #######################

########### start: breakpoint analysis output for 'quick analysis' panel #######

    # display "Number of breakpoints detected:" text and value
    output$quick_numBreakpoints <- renderText({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display text only if breakpoints are detected
      if(length(quickTP()) > 1){
        c("Number of breakpoints detected:", quickTP()[[1]])
      }
    })

    # display "Location:" text
    output$quick_locationText <- renderText({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display text only if breakpoints are detected
      if(length(quickTP()) > 1){
        "Location(s):"
      }
    })

    # display locations of detected breakpoints
    output$quick_tpOutput <- renderText({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display this if breakpoints are detected
      if(length(quickTP()) > 1){
        paste(quickTP()[[2]], collapse=", ")
      }
      # if no breakpoints are detected, use default output
      else{
        quickTP()
      }
    })

    # display checkbox for drawing breakpoint lines
    output$quick_breakpointsCheckboxSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display only if breakpoints are detected
      if(length(quickTP()) > 1){
        checkboxInput("quick_breakpointsCheckbox", "Draw Breakpoint Lines",
                      value=FALSE)
      }
    })

######## end: breakpoint analysis output for 'quick analysis' panel ############

######## start: ews analysis output for 'quick analysis' panel #################

    # display ews radio buttons
    output$quick_ewsRadioButtonSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      if(length(quickTP()) >= 1){
        radioButtons("quick_ewsRadioButtons",
                      "View Early Warning Signal Analysis:",
                      c("None", "Standard Deviation", "Skewness", "Kurtosis",
                        "Coefficient of Variation", "Return Rate",
                        "Density Ratio", "Autocorrelation at First Lag",
                        "Autoregressive Coefficient"), selected=NULL,
                      inline=FALSE)
      }
    })

    # display button to download ews statistics
    output$quick_downloadTable <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      if(length(quickTP()) >= 1){
        downloadButton('downloadQuickTable', 'Download Data')
      }
    })

    # download ews data
    output$downloadQuickTable <- downloadHandler(
      filename = function() { paste("PredatorPreyEWS(quick)", '.csv', sep='') },
      content = function(file) {
        write.csv(quickGeneric(), file)
      }
    )

    # fill ews breakdown table with appropriate data based on radio buttons
    output$quick_ewsTable <- renderDataTable({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_Nmax) || !is.numeric(input$quick_Nmax)){
        return()
      }
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }
      else if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "None"){
        return()
      }

      # check radio buttons value
      if(input$quick_ewsRadioButtons == "Standard Deviation"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[3])
        colnames(table) <- c("Time Index", "Standard Deviation")
      }
      else if(input$quick_ewsRadioButtons == "Skewness"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[4])
        colnames(table) <- c("Time Index", "Skewness")
      }
      else if(input$quick_ewsRadioButtons == "Kurtosis"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[5])
        colnames(table) <- c("Time Index", "Kurtosis")
      }
      else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[6])
        colnames(table) <- c("Time Index", "Coefficient of Variation")
      }
      else if(input$quick_ewsRadioButtons == "Return Rate"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[7])
        colnames(table) <- c("Time Index", "Return Rate")
      }
      else if(input$quick_ewsRadioButtons == "Density Ratio"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[8])
        colnames(table) <- c("Time Index", "Density Ratio")
      }
      else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[9])
        colnames(table) <- c("Time Index", "Autocorrelation at First Lag")
      }
      else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[2])
        colnames(table) <- c("Time Index", "Autoregressive Coefficient")
      }

      # return table with updated column names
      return(table)
    }, options=list(pageLength=10))

########## end: ews analysis output for 'quick analysis' panel #################

################################################################################
################################################################################
################################################################################

################################################################################
#################### Advanced Tipping Point Analysis ###########################
################################################################################

    ### start: run (advanced) tipping point analysis based on user-input ###

    TPanalysis <- reactive({
      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          # for prey
          if(input$dataType == "Prey"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(lvPredPrey()[1], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(lvPredPrey()[1], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(lvPredPrey()[1], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(lvPredPrey()[1], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(lvPredPrey()[1], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(lvPredPrey()[1], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for predator
          else if(input$dataType == "Predator"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(lvPredPrey()[2], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(lvPredPrey()[2], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(lvPredPrey()[2], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(lvPredPrey()[2], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(lvPredPrey()[2], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(lvPredPrey()[2], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

        }) # withProgress
      }) # withProgress
    })

    ### end: run (advanced) tipping point analysis based on user-input ###

    ### start: (advanced) tipping point analysis output ###

    # display "Number of breakpoints detected:" text and value
    output$numBreakpoints <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      #display text only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        c("Number of breakpoints detected:", TPanalysis()[[1]])
      }
    })

    # display "Location:" text
    output$locationText <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      # display text only if breakpoints are detected
      if(length(TPanalysis()) > 1){
        "Location(s):"
      }
    })

    # display locations of detected breakpoints
    output$tpOutput <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      # display this if breakpoints are detected
      if(length(TPanalysis()) > 1){
        paste(TPanalysis()[[2]], collapse=", ")
      }
      #if no breakpoints are detected, use default output
      else{
       TPanalysis()
      }
    })

    output$breakpointsCheckboxSlot <- renderUI({
            # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      # display this if breakpoints are detected
      if(length(TPanalysis()) > 1){
        checkboxInput("breakpointsCheckbox", "Draw Breakpoint Lines",
                      value=FALSE)
      }
    })

    ### end: (advanced) tipping point analysis output ###

################################################################################

############## Advanced Early Warning Signals Analysis #########################

    ### start: run (advanced) ews analysis based on user-input ###

    # reactive for dynamic updates
    advancedGeneric <- reactive({

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$dataType == "Prey"){
            generic_ews(timeseries=subset(lvPredPrey(), select="prey"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Predator"){
            generic_ews(timeseries=subset(lvPredPrey(), select="predator"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }

        }) # withProgress
      }) # withProgress
    })

    ### end: run (advanced) ews analysis based on user-input ###

    ### start: (advanced) ews point analysis output ###

    # display ews radio buttons
    output$ewsRadioButtonSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(advancedGeneric())){
        return()
      }

      radioButtons("ewsRadioButtons", "View Early Warning Signal Analysis:",
                   c("None", "Standard Deviation", "Skewness", "Kurtosis",
                     "Coefficient of Variation", "Return Rate", "Density Ratio",
                     "Autocorrelation at First Lag",
                     "Autoregressive Coefficient"), selected=NULL, inline=FALSE)
    })

    output$ewsTableCheckboxSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$ewsRadioButtons)){
        return()
      }
      else if(input$ewsRadioButtons == "None"){
        return()
      }

      checkboxInput("ewsTableCheckbox", "Show Statistic Table",
                      value=FALSE)
    })

    output$downloadEWStableSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$ewsRadioButtons)){
        return()
      }
      if(is.null(TPanalysis())){
        return()
      }

      downloadButton('downloadEWStable', 'Download Data')
    })

    # download ews data
    output$downloadEWStable <- downloadHandler(
      filename = function() { paste("PredatorPreyEWS(advanced)", '.csv', sep='') },
      content = function(file) {
        write.csv(quickGeneric(), file)
      }
    )

    # fill ews breakdown table with appropriate data based on radio buttons
    output$ewsTable <- renderDataTable({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(is.null(input$ewsRadioButtons)){
        return()
      }
      else if(input$ewsRadioButtons == "None"){
        return()
      }
      else if(is.null(input$ewsTableCheckbox) || input$ewsTableCheckbox == FALSE){
        return()
      }

      # check radio buttons value
      if(input$ewsRadioButtons == "Standard Deviation"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[3])
        colnames(EWStable) <- c("Time Index", "Standard Deviation")
      }
      else if(input$ewsRadioButtons == "Skewness"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[4])
        colnames(EWStable) <- c("Time Index", "Skewness")
      }
      else if(input$ewsRadioButtons == "Kurtosis"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[5])
        colnames(EWStable) <- c("Time Index", "Kurtosis")
      }
      else if(input$ewsRadioButtons == "Coefficient of Variation"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[6])
        colnames(EWStable) <- c("Time Index", "Coefficient of Variation")
      }
      else if(input$ewsRadioButtons == "Return Rate"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[7])
        colnames(EWStable) <- c("Time Index", "Return Rate")
      }
      else if(input$ewsRadioButtons == "Density Ratio"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[8])
        colnames(EWStable) <- c("Time Index", "Density Ratio")
      }
      else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[9])
        colnames(EWStable) <- c("Time Index", "Autocorrelation at First Lag")
      }
      else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
        # use ews time-index determination and associated value
        EWStable <- cbind(timeIndex=advancedGeneric()[1], advancedGeneric()[2])
        colnames(EWStable) <- c("Time Index", "Autoregressive Coefficient")
      }

      # return table with updated column names
      return(EWStable)

    }, options=list(pageLength=10))

    ### end: (advanced) ews point analysis output ###

################################################################################

############################## Model ###########################################

    # load code text to page
    output$codeText <- renderText({
"# Load dependencies
library(deSolve)
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
output <- data.frame(ode(y=initState, times=time, func=lvPredPreyEqs,
                         parms=params)[,-1])
return(output)
}"
    })

######################### Ace ##################################################

    # render script text
    aceScript <- reactive({
"# load dependencies
library(deSolve)
library(breakpoint)
library(ggplot2)
library(earlywarnings)
##### Lotka-Volterra Predator Prey Model #####
lvPredPreyModel <- function(time, initState, params){
  ## function for ordinary differential equations (ODE)
  lvPredPreyEqs <-function(time, initState, params){
    with(as.list(c(initState, params)),{
      ## lotka-Volterra predator-prey model
      dx <- (alpha * prey) - (beta * prey * predator)
      dy <- (gamma * prey * predator) - (delta * predator)
      list(c(dx, dy))
    })
  }
  ## deSolve method to solve initial value problems (IVP)
  output <- data.frame(ode(y=initState, times=time, func=lvPredPreyEqs,
                            parms=params)[,-1])
  return(output)
}
## Test-values ##
## alpha = the growth rate of prey
## beta = the rate at which predators kill prey
## delta = the death rate of predators
## gamma = the rate at which predators increase by consuming prey
time <- seq(1, 100, by=1)
initState <- c(prey=500, predator=10)
params <- c(alpha=1.5, beta=0.02, delta=0.4, gamma=0.01)
## Function-call ##
data <- lvPredPreyModel(time, initState, params)
##### Breakpoint Analysis ('breakpoint' Package) ######
## data[1] = 'Prey' and data[2] = 'Predator'
## argument 'distyp=1' is four parameter beta distribution
## argument 'distyp=2' is truncated normal distribution
## argument 'Nmax' is the maximum number of breakpoints
## run breakpoint analysis for Continuous Data
  ## used in this application's 'Quick Analysis'
cont_BP <- CE.Normal(data=data[1], Nmax=10, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8, distyp=1, parallel=FALSE)
## run breakpoint analysis with Negative Binomial Distribution
#nb_BP <- CE.NB(data=data[1], Nmax=10, eps=0.01, rho=0.05, M=200, h=5, a=0.8,
#                b=0.8, distyp=1, parallel=FALSE)
## run breakpoint analysis with Zero-Inflated Negative Binomial Distribution
#zinb_BP <- CE.ZINB(data=data[1], Nmax=10, eps=0.01, rho=0.05, M=200, h=5,
#                    a=0.8, b=0.8, distyp=1, parallel=FALSE)
##### Early Warning Signals Analysis ('earlywarnings' Package)
## generic early warning signals
  ## used in this application's 'Quick Analysis'
gen_EWS <- generic_ews(timeseries=subset(lvPredPrey(), select='prey'),
                        winsize=50, detrending = c('no', 'gaussian', 'loess',
                                                    'linear', 'first-diff'),
                        bandwidth = NULL, span = NULL, degree = NULL,
                        logtransform = FALSE, interpolate = FALSE, AR_n = FALSE,
                        powerspectrum = FALSE))
## quick detection analysis for generic early warning signals
#quick_EWS <- qda_ews(timeseries=subset(lvPredPrey(), select='prey'),
#                      param = NULL, winsize = 50, detrending = c('no',
#                      'gaussian', 'linear', 'first-diff'), bandwidth = NULL,
#                      boots = 100, s_level = 0.05, cutoff = 0.05,
#                      detection.threshold = 0.002, grid.size = 50,
#                      logtransform = FALSE, interpolate = FALSE)
"
    })

    # load script text into Ace
    observe({
      updateAceEditor(session, "ace", value=aceScript(), mode="r",
                      theme="chrome", readOnly=TRUE)
    })

    # download ace scripte
    output$aceDownloadButton <- downloadHandler(
      filename = function() { paste("PredatorPreyScript", '.R', sep='') },
      content = function(file) {
        write(aceScript(), file)
      }
    )

################################################################################

  } ## end server ##
)
