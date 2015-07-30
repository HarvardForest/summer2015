################################################################################
################################################################################
################## Pitcher Plant Simulation ####################################
####################### By: Nathan Justice #####################################
##################### Last edited: 30July2015 ##################################
################################################################################
################################################################################

###### Shiny server ######

# load dependencies
source("global.R", local=TRUE)

## start server ##
shinyServer(
  function(input, output, session){

    # declare instance of the simulation
    ppSim <- reactive({
      pitcherPlantSim(days=input$days, feedingTime=input$feedingTime,
                      foodWeight=input$foodWeight, beta=input$beta,
                      k=input$k, Bscaler=input$Bscaler, aMax=input$aMax,
                      aMin=input$aMin, s=input$s, d=input$d, c=input$c)
    })

################################################################################
########### Display dynamic plot (main) of the simulation ######################
################################################################################

    output$mainPlot <- renderPlot({

############ start: display default simulation plots  ##########################

      ### start: show only a default plot for 'customize graph' panel ###

      if(input$tabset_analyses == "Customize Graph"){
        # this check removes a transient error
        if(is.null(input$graphPlotOptions)){
          return()
        }

        # display plot based on user input from "graphPlotOptions" radio buttons
        else if(input$graphPlotOptions == "Oxygen"){
          matplot(x=ppSim()[1], y=ppSim()[2], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Oxygen")
          legend("topleft", "Oxygen", lty=c(1), col="black", bty="n")
        }
        else if(input$graphPlotOptions == "Photosynthesis"){
          matplot(x=ppSim()[1], y=ppSim()[3], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Photosynthesis")
          legend("topleft", "Photosynthesis", lty=c(1), col="black", bty="n")
        }
        else if(input$graphPlotOptions == "Biological Oxygen Demand"){
          matplot(x=ppSim()[1], y=ppSim()[4], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Biological Oxygen Demand")
          legend("topleft", "Biological Oxygen Demand", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$graphPlotOptions == "Nutrients"){
          matplot(x=ppSim()[1], y=ppSim()[5], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Nutrients")
          legend("topleft", "Nutrients", lty=c(1), col="black", bty="n")
        }
        else if(input$graphPlotOptions == "Augmentation Value"){
          matplot(x=ppSim()[1], y=ppSim()[6], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Augmentation Value")
          legend("topleft", "Augmentation Value", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$graphPlotOptions == "Food Amount"){
          matplot(x=ppSim()[1], y=ppSim()[7], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Food Amount")
          legend("topleft", "Food Amount", lty=c(1), col="black", bty="n")
        }
      }

      ### end: show only a default plot for 'customize graph' panel ###

      ### start: show default plot for 'quick analysis' panel ###

      else if(input$tabset_analyses == "Quick Analysis"){
        # this check removes a transient error
        if(is.null(input$quickPlotOptions)){
          return()
        }

        # generate default plot based on radio-selection
        if(input$quickPlotOptions == "Oxygen"){
          matplot(x=ppSim()[1], y=ppSim()[2], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Oxygen")
          legend("topleft", "Oxygen", lty=c(1), col="black", bty="n")
        }
        else if(input$quickPlotOptions == "Photosynthesis"){
          matplot(x=ppSim()[1], y=ppSim()[3], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Photosynthesis")
          legend("topleft", "Photosynthesis", lty=c(1), col="black", bty="n")
        }
        else if(input$quickPlotOptions == "Biological Oxygen Demand"){
          matplot(x=ppSim()[1], y=ppSim()[4], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Biological Oxygen Demand")
          legend("topleft", "Biological Oxygen Demand", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$quickPlotOptions == "Nutrients"){
          matplot(x=ppSim()[1], y=ppSim()[5], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Nutrients")
          legend("topleft", "Nutrients", lty=c(1), col="black", bty="n")
        }
        else if(input$quickPlotOptions == "Augmentation Value"){
          matplot(x=ppSim()[1], y=ppSim()[6], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Augmentation Value")
          legend("topleft", "Augmentation Value", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$quickPlotOptions == "Food Amount"){
          matplot(x=ppSim()[1], y=ppSim()[7], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Food Amount")
          legend("topleft", "Food Amount", lty=c(1), col="black", bty="n")
        }
      }

      ### end: show default plot for 'quick analysis' panel ###

      ### start: show default for 'advanced analysis' panel ###

      else if(input$tabset_analyses == "Advanced Analysis"){
        # this check removes a transient error
        if(is.null(input$advancedPlotOptions)){
          return()
        }

        # generate default plot based on radio-selection
        if(input$advancedPlotOptions == "Oxygen"){
          matplot(x=ppSim()[1], y=ppSim()[2], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Oxygen")
          legend("topleft", "Oxygen", lty=c(1), col="black", bty="n")
        }
        else if(input$advancedPlotOptions == "Photosynthesis"){
          matplot(x=ppSim()[1], y=ppSim()[3], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Photosynthesis")
          legend("topleft", "Photosynthesis", lty=c(1), col="black", bty="n")
        }
        else if(input$advancedPlotOptions == "Biological Oxygen Demand"){
          matplot(x=ppSim()[1], y=ppSim()[4], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Biological Oxygen Demand")
          legend("topleft", "Biological Oxygen Demand", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$advancedPlotOptions == "Nutrients"){
          matplot(x=ppSim()[1], y=ppSim()[5], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Nutrients")
          legend("topleft", "Nutrients", lty=c(1), col="black", bty="n")
        }
        else if(input$advancedPlotOptions == "Augmentation Value"){
          matplot(x=ppSim()[1], y=ppSim()[6], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Augmentation Value")
          legend("topleft", "Augmentation Value", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$advancedPlotOptions == "Food Amount"){
          matplot(x=ppSim()[1], y=ppSim()[7], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Food Amount")
          legend("topleft", "Food Amount", lty=c(1), col="black", bty="n")
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
        if(input$quick_dataType == "Oxygen"){
          matplot(x=ppSim()[1], y=ppSim()[2], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Oxygen")
          legend("topleft", "Oxygen", lty=c(1), col="black", bty="n")
        }
        else if(input$quick_dataType == "Photosynthesis"){
          matplot(x=ppSim()[1], y=ppSim()[3], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Photosynthesis")
          legend("topleft", "Photosynthesis", lty=c(1), col="black", bty="n")
        }
        else if(input$quick_dataType == "Biological Oxygen Demand"){
          matplot(x=ppSim()[1], y=ppSim()[4], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Biological Oxygen Demand")
          legend("topleft", "Biological Oxygen Demand", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$quick_dataType == "Nutrients"){
          matplot(x=ppSim()[1], y=ppSim()[5], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Nutrients")
          legend("topleft", "Nutrients", lty=c(1), col="black", bty="n")
        }
        else if(input$quick_dataType == "Augmentation Value"){
          matplot(x=ppSim()[1], y=ppSim()[6], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Augmentation Value")
          legend("topleft", "Augmentation Value", lty=c(1), col="black",
                 bty="n")
        }
        else if(input$quick_dataType == "Food Amount"){
          matplot(x=ppSim()[1], y=ppSim()[7], type="l", xlab=input$xaxis,
                  ylab=input$yaxis, pch=1)
          title("Food Amount")
          legend("topleft", "Food Amount", lty=c(1), col="black", bty="n")
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
            if(input$quick_dataType == "Oxygen"){
              legend("topleft", c("Oxygen", "Breakpoints"), lty=c(1, 1),
                     col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Photosynthesis"){
              legend("topleft", c("Photosynthesis", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Biological Oxygen Demand"){
              legend("topleft", c("Biological Oxygen Demand", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Nutrients"){
              legend("topleft", c("Nutrients", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Augmentation Value"){
              legend("topleft", c("Augmentation", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Food Amount"){
              legend("topleft", c("Food Amount", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
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
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(ppSim()[[2]]), ppSim()[[2]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(ppSim()[[3]]), ppSim()[[3]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(ppSim()[[4]]), ppSim()[[4]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(ppSim()[[5]]), ppSim()[[5]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(ppSim()[[6]]), ppSim()[[6]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(ppSim()[[7]]), ppSim()[[7]],
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }
        }

################ end: draw ews line for 'observed' (quick analysis) ############

########### start: draw ews line for 'trend' (quick analysis) ##################

        else if(input$quick_decomposeOptions == "Trend"){

        # draw new instance of basic plot based on state variable selection
        if(input$quick_dataType == "Oxygen"){
          decomposed = decompose(ts(ppSim()[[2]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Oxygen: Trend")
          legend("topleft", "Oxygen", lty=1, col="black", bty="n")
        }
        else if(input$quick_dataType == "Photosynthesis"){
          decomposed = decompose(ts(ppSim()[[3]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Photosynthesis: Trend")
          legend("topleft", "Photosynthesis", lty=1, col="black", bty="n")
        }
        else if(input$quick_dataType == "Biological Oxygen Demand"){
          decomposed = decompose(ts(ppSim()[[4]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Biological Oxygen Demand: Trend")
          legend("topleft", "Biological Oxygen Demand", lty=1, col="black",
                 bty="n")
        }
        else if(input$quick_dataType == "Nutrients"){
          decomposed = decompose(ts(ppSim()[[5]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Nutrients: Trend")
          legend("topleft", "Nutrients", lty=1, col="black", bty="n")
        }
        else if(input$quick_dataType == "Augmentation Value"){
          decomposed = decompose(ts(ppSim()[[6]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Augmentation Value: Trend")
          legend("topleft", "Augmentation Value", lty=1, col="black", bty="n")
        }
        else if(input$quick_dataType == "Food Amount"){
          decomposed = decompose(ts(ppSim()[[7]],
                                      frequency=input$quick_frequency))
          plot(decomposed$trend, xlab=input$xaxis, ylab=input$yaxis)
          title(main="Food Amount: Trend")
          legend("topleft", "Food Amount", lty=1, col="black", bty="n")
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
            if(input$quick_dataType == "Oxygen"){
              legend("topleft", c("Oxygen", "Breakpoints"), lty=c(1, 1),
                     col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Photosynthesis"){
              legend("topleft", c("Photosynthesis", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Biological Oxygen Demand"){
              legend("topleft", c("Biological Oxygen Demand", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Nutrients"){
              legend("topleft", c("Nutrients", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Augmentation Value"){
              legend("topleft", c("Augmentation", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
            else if(input$quick_dataType == "Food Amount"){
              legend("topleft", c("Food Amount", "Breakpoints"),
                     lty=c(1, 1), col=c("black", "blue"), bty="n")
            }
          }

          # display default plot attributes if there are no ews lines selected
          if(is.null(input$quick_ewsRadioButtons)
             || input$quick_ewsRadioButtons == "None"){
            return()
          }

          # for 'standard deviation'
          if(input$quick_ewsRadioButtons == "Standard Deviation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[3]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'skewness'
          if(input$quick_ewsRadioButtons == "Skewness"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[4]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'kurtosis'
          if(input$quick_ewsRadioButtons == "Kurtosis"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[5]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'coefficient of variation'
          if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[6]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'return rate'
          if(input$quick_ewsRadioButtons == "Return Rate"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[7]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'density ratio'
          if(input$quick_ewsRadioButtons == "Density Ratio"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[8]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'autocorrelation at first lag'
          if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[9]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }

          # for 'autoregressive coefficient'
          if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
            # re-scale ews statistic
            ewsLine <- quickGeneric()[2]
            totalMinutes <- 3 * 1440

            # adjust starting point to accomodate rolling window size
            for(i in 2:(totalMinutes * (input$quick_winsize * 0.01))){
              ewsLine <- rbind(NA, ewsLine)
            }

            # draw rescaled ews line, axis, and label (from 'plotrix')

            # for oxygen
            if(input$quick_dataType == "Oxygen"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Oxygen")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Oxygen", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Oxygen", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for photosynthesis
            if(input$quick_dataType == "Photosynthesis"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Photosynthesis")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Photosynthesis", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Photosynthesis", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for biological oxygen demand
            if(input$quick_dataType == "Biological Oxygen Demand"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Biological Oxygen Demand")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Biological Oxygen Demand", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Biological Oxygen Demand",
                         input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for nutrients
            if(input$quick_dataType == "Nutrients"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Nutrients")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Nutrients", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Nutrients", input$quick_ewsRadioButtons), lty=c(1, 1),
                       col=c("black", "green4"), bty="n")
              }
            }

            # for augmentation value
            if(input$quick_dataType == "Augmentation Value"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Augmentation Value")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Augmentation Value", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Augmentation Value", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }

            # for food amount
            if(input$quick_dataType == "Food Amount"){
              twoord.plot(1:length(decomposed$trend), decomposed$trend,
                          1:length(ewsLine[[1]]), ewsLine[[1]], type="l",
                          rcol="green4", xlab=input$xaxis, ylab=input$yaxis,
                          lty=1, lcol="black")
              title(main="Food Amount")
              mtext(input$quick_ewsRadioButtons, side=4, col="green4")
              if(!is.null(input$quick_breakpointsCheckbox)
                 && input$quick_breakpointsCheckbox == TRUE) {

                # include breakpoint lines
                abline(v=quickTP()[[2]], col="blue")
                # update plot legend with ews and breakpoint lines
                legend("topleft",c("Food Amount", "Breakpoints",
                                   input$quick_ewsRadioButtons), lty=c(1, 1, 1),
                       col=c("black", "blue", "green4"), bty="n")
              }
              else{
                # update plot legend with only ews line
                legend("topleft",
                       c("Food Amount", input$quick_ewsRadioButtons),
                       lty=c(1, 1), col=c("black", "green4"), bty="n")
              }
            }
          }
        }

############# end: draw ews line for 'trend' (quick analysis) ##################

########### start: draw ews line for 'seasonal' (quick analysis) ###############

      } # end: quick_analysis
    }) # end: render_plot (main)

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
                        choices=c("Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount"),
                        selected="Oxygen", inline=TRUE)
        }
      }
      else if(input$tabset_analyses == "Advanced Analysis"){
        if(input$dataType == " "){
          radioButtons("advancedPlotOptions", "Display:",
                        choices=c("Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount"),
                        selected="Oxygen", inline=TRUE)
        }
      }
      else if(input$tabset_analyses == "Customize Graph"){
        radioButtons("graphPlotOptions", "Display:",
                     choices=c("Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount"),
                        selected="Oxygen", inline=TRUE)
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
      ppSim()
    })

    # download main table feature
    output$downloadMainTable <- downloadHandler(
      filename = function() { paste("PitcherPlantSimulatedData", '.csv', sep='')},
      content = function(file) {
        write.csv(ppSim(), file)
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
                      Decomposition analysis:", value=1440)
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
      if(input$quick_dataType == "Oxygen"){
        plot(decompose(ts(ppSim()[[2]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Photosynthesis"){
        plot(decompose(ts(ppSim()[[3]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Biological Oxygen Demand"){
        plot(decompose(ts(ppSim()[[4]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Nutrients"){
        plot(decompose(ts(ppSim()[[5]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Augmentation Value"){
        plot(decompose(ts(ppSim()[[6]], frequency=input$quick_frequency)))
      }
      else if(input$quick_dataType == "Food Amount"){
        plot(decompose(ts(ppSim()[[7]], frequency=input$quick_frequency)))
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
      if(input$dataType == "Oxygen"){
        plot(decompose(ts(ppSim()[[2]], frequency=input$frequency)))
      }
      else if(input$dataType == "Photosynthesis"){
        plot(decompose(ts(ppSim()[[3]], frequency=input$frequency)))
      }
      else if(input$dataType == "Biological Oxygen Demand"){
        plot(decompose(ts(ppSim()[[4]], frequency=input$frequency)))
      }
      else if(input$dataType == "Nutrients"){
        plot(decompose(ts(ppSim()[[5]], frequency=input$frequency)))
      }
      else if(input$dataType == "Augmentation Value"){
        plot(decompose(ts(ppSim()[[6]], frequency=input$frequency)))
      }
      else if(input$dataType == "Food Amount"){
        plot(decompose(ts(ppSim()[[7]], frequency=input$frequency)))
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

          # for oxygen
          if(input$quick_dataType == "Oxygen"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[2]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[2]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

          # for photosynthesis
          else if(input$quick_dataType == "Photosynthesis"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[3]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[3]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

          # for biological oxygen demand
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[4]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[4]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

          # for nutrients
          else if(input$quick_dataType == "Nutrients"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[5]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[5]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

          # for augmentation value
          else if(input$quick_dataType == "Augmentation Value"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[6]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[6]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

          # for food amount
          else if(input$quick_dataType == "Food Amount"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[7]],
                                       frequency=input$quick_frequency))

            # run breakpoint analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              processStream(ppSim()[[7]], cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              processStream(x, cpmType="Exponential")
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              processStream(x, cpmType="Exponential")
            }
          }

        }) # withProgress
      }) # withProgress
    })

############## end: predetermined (quick) breakpoint analysis ##################

############## start: predetermined (quick) ews analysis #######################

    # reactive for dynamic updates
    quickGeneric <- reactive({
      # check required information
      if(is.null(input$quick_runButton)){
        return()
      }

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          # for oxygen
          if(input$quick_dataType == "Oxygen"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[2]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[2]],
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

          # for photosynthesis
          else if(input$quick_dataType == "Photosynthesis"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[3]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[3]],
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

          # for biological oxygen demand
          if(input$quick_dataType == "Biological Oxygen Demand"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[4]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[2]],
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

          # for nutrients
          else if(input$quick_dataType == "Nutrients"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[5]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[5]],
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

          # for augmentation value
          else if(input$quick_dataType == "Augmentation Value"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[6]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[6]],
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

          # for food amount
          else if(input$quick_dataType == "Food Amount"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[7]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              return(generic_ews(timeseries=ppSim()[[7]],
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
      filename = function() { paste("PitcherPlantEWS(quick)", '.csv', sep='') },
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

          # for oxygen
          if(input$dataType == "Oxygen"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[2]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[2]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[2]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
          }


          # for photosynthesis
          else if(input$dataType == "Photosynthesis"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[3]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[3]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[3]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
          }

          # for biological oxygen demand
          else if(input$dataType == "Biological Oxygen Demand"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[4]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[4]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[4]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
          }

          # for nutrients
          else if(input$dataType == "Nutrients"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[5]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[5]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[5]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
          }

          # for augmentation value
          else if(input$dataType == "Augmentation Value"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[6]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[6]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[6]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
          }

          # for food amount
          if(input$dataType == "Food Amount"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[7]],
                                       frequency=input$frequency))

            # run breakpoint analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              if(input$cpmType == "Exponential distribution"){
                processStream(ppSim()[[7]], cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(ppSim()[[7]], cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
              }
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              if(input$cpmType == "Exponential distribution"){
                processStream(x, cpmType="Exponential",
                              startup=input$startup)
              }
              else if(input$cpmType == "Gaussian sequence"){
                processStream(x, cpmType="GLR",
                              startup=input$startup)
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

          # for oxygen
          if(input$dataType == "Oxygen"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[2]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[2]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
          }

          # for photosynthesis
          if(input$dataType == "Photosynthesis"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[3]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[3]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
          }

          # for biological oxygen demand
          if(input$dataType == "Oxygen"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[4]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[4]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
          }

          # for nutrients
          if(input$dataType == "Nutrients"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[5]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[5]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
          }

          # for augmentation value
          if(input$dataType == "Augmentation Value"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[6]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[6]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
          }

          # for food amount
          if(input$dataType == "Food Amount"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[7]],
                                       frequency=input$frequency))

            # run ews analysis on desired component
            if(input$decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[7]],
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
            }
            else if(input$decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x,
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate)
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
      filename = function() { paste("PitcherPlantEWS(advanced)", '.csv', sep='') },
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

    ### end: (advanced) ews point analysis output ###

################## end: show (advanced) ews analysis output ####################

################################################################################
################################################################################
################################################################################

################################################################################
############################## Model ###########################################
################################################################################

    # load code text to page
    output$codeText <- renderText({
"PASS
"
    })

################################################################################
################################################################################
################################################################################

################################################################################
################################## Ace #########################################
################################################################################

    # render script text
    aceScript <- reactive({
"PASS
"
    })

    # load script text into Ace
    observe({
      updateAceEditor(session, "ace", value=aceScript(), mode="r",
                      theme="chrome", readOnly=TRUE)
    })

    # download ace scripte
    output$aceDownloadButton <- downloadHandler(
      filename = function() { paste("PitcherPlantScript", '.R', sep='') },
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
