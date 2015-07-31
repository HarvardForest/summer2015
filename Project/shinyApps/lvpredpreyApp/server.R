################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 31July2015 ##################################
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
########### Display dynamic plot (main) of the simulation ######################
################################################################################

    output$mainPlot <- renderPlot({

############ start: display default simulation plots  ##########################

      ### start: show only a default plot for 'customize graph' panel ###

      if(input$tabset_analyses == "Customize Graph"){
        matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis,
                lty=c(1, 1), col=c("black", "red"))
          title(main=input$plotTitle)
          legend("topleft", c(input$preyLabel, input$predatorLabel),
                 lty=c(1, 1), col=c("black", "red"), bty="n")
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
            #dev.off()
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
          if(input$quick_ewsRadioButtons == "Standard Deviation"){
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

#### start: draw breakpoint lines on main plot for 'advanced analysis' panel ###

      # run only if the "Advanced Analysis" tab is active
      if(input$tabset_analyses == "Advanced Analysis"){
        if(is.null(input$decomposeOptions)
          || input$decomposeOptions == "Observed (Simulated Data)"
          || input$decomposeOptions == " "){

          # draw new instance of basic plot based on state variable selection
          if(input$dataType == "Prey"){
            matplot(lvPredPrey()[1], type="l", xlab=input$xaxis,
                    ylab=input$yaxis, lty=1, col="black")
            title(main=input$preyLabel)
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$dataType == "Predator"){
            matplot(lvPredPrey()[2], type="l", xlab=input$xaxis,
                    ylab=input$yaxis, lty=1, col="red")
            title(main=input$predatorLabel)
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines and ews lines can be drawn
          if(is.null(input$dataType) || input$dataType == " "){
            return()
          }
          # indicates breakpoint lines (only) can be drawn
          else if(!is.null(input$breakpointsCheckbox)
                  && input$breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend
            if(input$dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                     col=c("black", "blue"), bty="n")
            }
            else if(input$dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

#### end: draw breakpoint lines on main plot for 'advanced analysis' panel #####

##### start: update plot and legend with ews line (advanced analysis) ##########

          # draw ews line based on radio button selection

######### start: draw ews line for 'observed' (advanced analysis) ##############

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$ewsRadioButtons)
             || input$ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(lvPredPrey()[[1]]), lvPredPrey()[[1]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main=input$preyLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(lvPredPrey()[[2]]), lvPredPrey()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main=input$predatorLabel)
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

################ end: draw ews line for 'observed' (advanced analysis) #########

########### start: draw ews line for 'trend' (advanced analysis) ###############

        else if(input$decomposeOptions == "Trend"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$frequency))
            plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Trend")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$frequency))
            plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Trend")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$breakpointsCheckbox)
            && input$breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend
            if(input$dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$ewsRadioButtons)
             || input$ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Trend")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

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
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############# end: draw ews line for 'trend' (advanced analysis) ###############

########### start: draw ews line for 'seasonal' (advanced analysis) ############

        else if(input$decomposeOptions == "Seasonal (Periodicity)"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$frequency))
            plot(decomposed$seasonal, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Seasonal (Periodicity)")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$frequency))
            plot(decomposed$seasonal, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Seasonal (Periodicity)")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$breakpointsCheckbox)
            && input$breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend
            if(input$dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$ewsRadioButtons)
             || input$ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'standard deviation'
          else if(input$ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$seasonal), decomposed$seasonal,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Seasonal (Periodicity)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############# end: draw ews line for 'seasonal' (advanced analysis) ############

############# start: draw ews line for 'random' (advanced analysis) ############

        else if(input$decomposeOptions == "Random (Residuals)"){
          # draw new instance of basic plot based on state variable selection

          # for 'prey'
          if(input$dataType == "Prey"){
            decomposed = decompose(ts(lvPredPrey()[[1]],
                                      frequency=input$frequency))
            plot(decomposed$random, xlab=input$xaxis, ylab=input$yaxis)
            title(main="Prey: Random (Residuals)")
            legend("topleft", input$preyLabel, lty=1, col="black", bty="n")
          }
          else if(input$dataType == "Predator"){
            decomposed = decompose(ts(lvPredPrey()[[2]],
                                      frequency=input$frequency))
            plot(decomposed$random, xlab=input$xaxis, ylab=input$yaxis, col="red")
            title(main="Predator: Random (Residuals)")
            legend("topleft", input$predatorLabel, lty=1, col="red", bty="n")
          }

          # check if breakpoint lines (only) can be drawn
          if(!is.null(input$breakpointsCheckbox)
            && input$breakpointsCheckbox == TRUE) {

            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend
            if(input$dataType == "Prey"){
              legend("topleft", c(input$preyLabel, "Breakpoints"), lty=c(1, 1),
                      col=c("black", "blue"), bty="n")
            }
            else if(input$dataType == "Predator"){
              legend("topleft", c(input$predatorLabel, "Breakpoints"),
                     lty=c(1, 1), col=c("red", "blue"), bty="n")
            }
          }

          # draw ews line based on radio button selection

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$ewsRadioButtons)
             || input$ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          else if(input$ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[3]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          else if(input$ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[4]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          else if(input$ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[5]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          else if(input$ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[6]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          else if(input$ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[7]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          else if(input$ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[8]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[9]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- advancedGeneric()[2]
            # adjust starting point to accomodate rolling window size
            for(i in 1:(input$time * (input$winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for 'prey'
            if(input$dataType == "Prey"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Prey: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$preyLabel, "Breakpoints",
                                   input$ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$preyLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for 'predator'
            if(input$dataType == "Predator"){
              twoord.plot(1:length(decomposed$random), decomposed$random,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="red")
              title(main="Predator: Random (Residuals)")
              mtext(input$ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$breakpointsCheckbox)
                 && input$breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=TPanalysis()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c(input$predatorLabel, "Breakpoints",
                                   input$ewsRadioButtons),
                       lty=c(1, 1, 1), col=c("red", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c(input$predatorLabel, input$ewsRadioButtons),
                       lty=c(1, 1), col=c("red", "green4"), bty="n")
              }
            }
          }
        }

############### end: draw ews line for 'random' (advanced analysis) ############

      } # end tabset_advancedAnalysis

####### end: update plot and legend with ews line (advanced analysis) ##########

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
      if(input$tabset_analyses == "Quick Analysis"){
        if(input$quick_dataType == " "){
          radioButtons("quickPlotOptions", "Display:",
                        choices=c("Prey", "Predator", "Both"),
                        selected="Both", inline=TRUE)
        }
      }
      else if(input$tabset_analyses == "Advanced Analysis"){
        if(input$dataType == " "){
          radioButtons("advancedPlotOptions", "Display:",
                        choices=c("Prey", "Predator", "Both"),
                        selected="Both", inline=TRUE)
        }
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
                      Decomposition analysis:", value=20)
    })

    output$quick_cpmTypeSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

        return()
      }

      selectInput("quick_cpmType", "Change point model type:",
                  choices=c("Gaussian sequence", "Exponential distribution"))
    })

    output$quick_winsizeSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      else if(is.null(input$quick_frequency)
        || !is.numeric(input$quick_frequency)){

        return()
      }
      else if(is.null(input$quick_decomposeOptions)
        || input$quick_decomposeOptions == " "){

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
      if(!is.numeric(input$quick_frequency)){
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

############# start: display decomposition plot (advanced analysis) ############

    output$decomposePlotSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      if(!is.numeric(input$frequency)){
        return()
      }

      plotOutput("decomposePlot")
    })

    output$decomposePlot <- renderPlot({
      if(input$dataType == "Prey"){
        plot(decompose(ts(lvPredPrey()[[1]], frequency=input$frequency)))
      }
      else if(input$dataType == "Predator"){
        plot(decompose(ts(lvPredPrey()[[2]], frequency=input$frequency)))
      }
    })

############# end: display decomposition plot (advanced analysis) ##############

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
            decomposed <- decompose(ts(lvPredPrey()[[1]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(lvPredPrey()[[1]], cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(lvPredPrey()[[1]], cpmType="GLR"))
              }
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
            }
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[2]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(lvPredPrey()[[2]], cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(lvPredPrey()[[2]]), cpmType="GLR")
              }
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$quick_cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential"))
              }
              else if(input$quick_cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR"))
              }
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
            decomposed <- decompose(ts(lvPredPrey()[[1]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=lvPredPrey()[[1]],
                          detrending="gaussian", winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
            }
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[2]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=lvPredPrey()[[2]],
                          detrending="gaussian", winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              return(generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize))
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      c("Number of breakpoints detected:", length(quickTP()[[2]]))
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display text only if breakpoints are detected
      if(length(quickTP()[[2]]) > 0){
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display this if breakpoints are detected
      if(length(quickTP()[[2]]) > 0){
        paste(quickTP()[[2]], collapse=", ")
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # display only if breakpoints are detected
      if(length(quickTP()[[2]]) > 0){
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # this check simulated execution of the 'quick_runButton'
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }

      # this check simulated execution of the 'quick_runButton'
      if(length(quickTP()) >= 1){
        downloadButton('downloadQuickTable', 'Download Early Warning Statistics')
      }
    })

    # download ews data
    output$downloadQuickTable <- downloadHandler(
      filename = function() { paste("PredatorPreyEWS(quick)", '.csv', sep='') },
      content = function(file) {
        write.csv(quickGeneric(), file)
      }
    )

    output$quick_ewsTableCheckboxSlot <- renderUI({
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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }
      else if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "None"){
        return()
      }

      checkboxInput("quick_ewsTableCheckbox", "Show Statistic Table", value=FALSE)
    })

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
      else if(is.null(input$quick_winsize) || !is.numeric(input$quick_winsize)){
        return()
      }
      else if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "None"){
        return()
      }
      if(is.null(input$quick_ewsTableCheckbox)
        || input$quick_ewsTableCheckbox == FALSE){

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
############### Build Advanced Analysis Dynamic User-input #####################
################################################################################

    output$frequencySlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("frequency",
                   "The number of observations per unit of time (frequency) for
                      Decomposition analysis:", value=20)
    })

    output$decomposeOptionsSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      selectInput("decomposeOptions", "Select a component for analysis:",
                    choices=c(" ", "Observed (Simulated Data)", "Trend",
                              "Seasonal (Periodicity)", "Random (Residuals)"))
    })

    output$runButtonSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      actionButton("runButton", "Run Analysis")
    })

################################################################################
################################################################################
################################################################################

################################################################################
################ Advanced Breakpoint Analysis ##################################
################################################################################

###### start: run (advanced) tipping point analysis based on user-input ########

    TPanalysis <- eventReactive(input$runButton, {
      # check required information
      if(is.null(input$runButton)){
        return()
      }

      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          # for prey
          if(input$dataType == "Prey"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[1]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                return(processStream(lvPredPrey()[[1]], cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(lvPredPrey()[[1]], cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
          }


          # for predator
          else if(input$dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[2]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                return(processStream(lvPredPrey()[[2]], cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(lvPredPrey()[[2]], cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                return(processStream(x, cpmType="Exponential",
                              startup=input$startup))
              }
              else if(input$cpmType == "Gaussian sequence"){
                return(processStream(x, cpmType="GLR",
                              startup=input$startup))
              }
            }
          }

        }) # withProgress
      }) # withProgress
    })

######## end: run (advanced) tipping point analysis based on user-input ########

############ start: (advanced) tipping point analysis output ###################

    # display "Number of breakpoints detected:" text and value
    output$numBreakpoints <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      c("Number of breakpoints detected:", length(TPanalysis()[[2]]))
    })

    # display "Location:" text
    output$locationText <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      # display text only if breakpoints are detected
      if(length(TPanalysis()[[2]]) > 0){
        "Location(s):"
      }
    })

    # display locations of detected breakpoints
    output$tpOutput <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      # display this if breakpoints are detected
      if(length(TPanalysis()[[2]]) > 0){
        paste(TPanalysis()[[2]], collapse=", ")
      }
    })

    output$breakpointsCheckboxSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      # display this if breakpoints are detected
      if(length(TPanalysis()[[2]]) > 0){
        checkboxInput("breakpointsCheckbox", "Draw Breakpoint Lines",
                      value=FALSE)
      }
    })

############## end: (advanced) tipping point analysis output ###################

################################################################################
################################################################################
################################################################################

################################################################################
############## Advanced Early Warning Signals Analysis #########################
################################################################################

########### start: run (advanced) ews analysis based on user-input #############

    advancedGeneric <- eventReactive(input$runButton, {
      # check required information
      if(is.null(input$runButton)){
        return()
      }

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          # for prey
          if(input$dataType == "Prey"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[1]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=lvPredPrey()[[1]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
          }

          # for predator
          else if(input$dataType == "Predator"){
            # decompose simulated data
            decomposed <- decompose(ts(lvPredPrey()[[2]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=lvPredPrey()[[2]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              return(generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate))
            }
          }

        }) # withProgress
      }) # withProgress
    })

########### end: run (advanced) ews analysis based on user-input ###############

################## start: show (advanced) ews analysis output ##################

    # display ews radio buttons
    output$ewsRadioButtonSlot <- renderUI({
      # check required information
      if(is.null(advancedGeneric())){
        return()
      }
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      radioButtons("ewsRadioButtons", "View Early Warning Signal Analysis:",
                   c("None", "Standard Deviation", "Skewness", "Kurtosis",
                     "Coefficient of Variation", "Return Rate", "Density Ratio",
                     "Autocorrelation at First Lag",
                     "Autoregressive Coefficient"), selected=NULL, inline=FALSE)
    })

    output$downloadEWStableSlot <- renderUI({
      # check required information
      if(is.null(advancedGeneric())){
        return()
      }
      if(is.null(input$ewsRadioButtons)){
        return()
      }
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      downloadButton('downloadEWStable', 'Download Early Warning Statistics')
    })

    # download ews data
    output$downloadEWStable <- downloadHandler(
      filename = function() { paste("PredatorPreyEWS(advanced)", '.csv', sep='') },
      content = function(file) {
        write.csv(quickGeneric(), file)
      }
    )

    output$ewsTableCheckboxSlot <- renderUI({
      # check required information
      if(is.null(input$ewsRadioButtons) || input$ewsRadioButtons == "None"){
        return()
      }
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }

      checkboxInput("ewsTableCheckbox", "Show Statistic Table", value=FALSE)
    })

    # fill ews breakdown table with appropriate data based on radio buttons
    output$ewsTable <- renderDataTable({
      # check required information
      if(is.null(input$ewsRadioButtons) || input$ewsRadioButtons == "None"){
        return()
      }
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      else if(is.null(input$decomposeOptions)
        || input$decomposeOptions == " "){

        return()
      }
      else if(is.null(input$frequency)
        || !is.numeric(input$frequency)){

        return()
      }
      # check for all valid tipping point arguments
      else if(!is.numeric(input$startup)){
        return()
      }
      # check for all valid ews arguments
      else if(!is.numeric(input$winsize) || !is.numeric(input$bandwidth)
        || !is.numeric(input$span) || !is.numeric(input$degree)){
        return()
      }
      if(is.null(input$ewsTableCheckbox) || input$ewsTableCheckbox == FALSE){
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

################## end: show (advanced) ews analysis output ####################

################################################################################
################################################################################
################################################################################

################################################################################
############################## Model ###########################################
################################################################################

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

################################################################################
################################################################################
################################################################################

################################################################################
################################## Ace #########################################
################################################################################

    # render script text
    aceScript <- reactive({
"# load dependencies
library(deSolve)
library(cpm)
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

##### Decompose timeseries data #####

## argument 'frequency' is the number of observations per time step

decomposedData <- decompose(ts(data[[1]], frequency=2))

## decompose breaks the timeseries data into 4 components
  # 1) 'decomposedData$x' = original timeseries
  # 2) 'decomposedData$trend' = trend taken from original timeseries
  # 3) 'decomposedData$seasonal' = periodicity taken from original timeseries
  # 4) 'decomposedData$random' = residuals taken from original timeseries

##### Breakpoint Analysis ('cpm' Package) ######

## data[1] = 'Prey' and data[2] = 'Predator'

## GLR: Generalized Likelihood Ratio test statistic.
  ## Use to detect both mean and variance changes in a Gaussian sequence.

bp1 <- processStream(data[[1]], cpmType='GLR')

## Exponential: Generalized Likelihood Ratio test statistic for Exponential
  ## distribution, as in [Ross, 2013]. Used to detect changes in the parameter
    ## of an Exponentially distributed sequence.

bp2 <- processStream(data[[1]], cpmType='Exponential')

##### Early Warning Signals Analysis ('earlywarnings' Package) #####

## generic early warning signals
  ## used in this application's 'Quick Analysis'

gen_EWS <- generic_ews(timeseries=decomposedData$x,
                        winsize=50, detrending = c('no', 'gaussian', 'loess',
                                                    'linear', 'first-diff'),
                        bandwidth = NULL, span = NULL, degree = NULL,
                        logtransform = FALSE, interpolate = FALSE, AR_n = FALSE,
                        powerspectrum = FALSE))

## quick detection analysis for generic early warning signals

#quick_EWS <- qda_ews(timeseries=sdecomposedData$x,
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
################################################################################
################################################################################

  }
)

## end server ##
