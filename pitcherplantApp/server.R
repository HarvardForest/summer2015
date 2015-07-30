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

      if(input$tabset_analyses == "Quick Analysis"){
          # displays Oxygen plot as default
          if(input$quick_dataType == "Oxygen" || input$quick_dataType == " "){
            matplot(x=ppSim()[1], y=ppSim()[2], type="l",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title("Oxygen")
            legend("topleft", "Oxygen", lty=c(1, 2), col=(1), bty="n")
          }
          else if(input$quick_dataType == "Photosynthesis"){
            matplot(x=ppSim()[1], y=ppSim()[3], type="p",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title(main=input$quick_dataType)
            legend("topleft", input$quick_dataType, lty=c(1, 2), col=(1), bty="n")
          }
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            matplot(x=ppSim()[1], y=ppSim()[4], type="p",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title(main=input$quick_dataType)
            legend("topleft", input$quick_dataType, lty=c(1, 2), col=(1), bty="n")
          }
          else if(input$quick_dataType == "Nutrients"){
            matplot(x=ppSim()[1], y=ppSim()[5], type="p",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title(main=input$quick_dataType)
            legend("topleft", input$quick_dataType, lty=c(1, 2), col=(1), bty="n")
          }
          else if(input$quick_dataType == "Augmentation Value"){
            matplot(x=ppSim()[1], y=ppSim()[6], type="p",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title(main=input$quick_dataType)
            legend("topleft", input$quick_dataType, lty=c(1, 2), col=(1), bty="n")
          }
          else if(input$quick_dataType == "Food Amount"){
            matplot(x=ppSim()[1], y=ppSim()[7], type="p",
                    xlab=input$xaxis, ylab=input$yaxis, pch=1)
            title(main=input$quick_dataType)
            legend("topleft", input$quick_dataType, lty=c(1, 2), col=(1), bty="n")
          }
        }

      if(input$tabset_analyses == "Advanced Analysis"){
        # displays Oxygen plot as default
        if(input$dataType == "Oxygen" || input$dataType == " "){
          matplot(x=ppSim()[1], y=ppSim()[2], type="l",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title("Oxygen")
          legend("topleft", "Oxygen", lty=c(1, 2), col=(1), bty="n")
        }
        else if(input$dataType == "Photosynthesis"){
          matplot(x=ppSim()[1], y=ppSim()[3], type="p",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title(main=input$dataType)
          legend("topleft", input$dataType, lty=c(1, 2), col=(1), bty="n")
        }
        else if(input$dataType == "Biological Oxygen Demand"){
          matplot(x=ppSim()[1], y=ppSim()[4], type="p",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title(main=input$dataType)
          legend("topleft", input$dataType, lty=c(1, 2), col=(1), bty="n")
        }
        else if(input$dataType == "Nutrients"){
          matplot(x=ppSim()[1], y=ppSim()[5], type="p",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title(main=input$dataType)
          legend("topleft", input$dataType, lty=c(1, 2), col=(1), bty="n")
        }
        else if(input$dataType == "Augmentation Value"){
          matplot(x=ppSim()[1], y=ppSim()[6], type="p",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title(main=input$dataType)
          legend("topleft", input$dataType, lty=c(1, 2), col=(1), bty="n")
        }
        else if(input$dataType == "Food Amount"){
          matplot(x=ppSim()[1], y=ppSim()[7], type="p",
                  xlab=input$xaxis, ylab=input$yaxis, pch=1)
          title(main=input$dataType)
          legend("topleft", input$dataType, lty=c(1, 2), col=(1), bty="n")
        }
      }

      ### end: show simulation plot based on selector ###

      ### start: draw breakpoint lines on main plot ###

      # run only if the "Quick Analysis" tab is active
      if(input$tabset_analyses == "Quick Analysis"){
        # check if breakpoint lines and ews lines can be drawn
        if(is.null(input$quick_dataType) || input$quick_dataType == " "){
          return()
        }
        else if(is.null(input$quick_breakpointsCheckbox)){
         return()
        }
        else if(is.null(input$quick_ewsRadioButtons)){
         return()
        }
        # indicates breakpoint lines can be drawn
        else if(input$quick_breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend
          legend("topleft", c(input$quick_dataType, "Breakpoints"), lty=c(1, 2),
                 col=c(1, "blue"), bty="n")
        }

        ### end: draw breakpoint lines on main plot ###

        ### start: update plot and legend with ews line ###

        # variable used to adjust ews-line start value
        ewsLineTime <- input$days * 1440

        # display default plot attributes if there are no ews lines selected
        if(input$quick_ewsRadioButtons == "None"){
          return()
        }
        # draw ews line based on radio button selection
        else if(input$quick_ewsRadioButtons == "Standard Deviation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[3]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Skewness"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[4]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Kurtosis"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[5]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[6]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Return Rate"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[7]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Density Ratio"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[8]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[9]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[2]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$quick_dataType, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$quick_dataType, input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }
      }

      # run only if the "Advanced  Analysis" tab is active
      if(input$tabset_analyses == "Advanced Analysis"){
        # check if breakpoint lines and ews lines can be drawn
        if(is.null(input$dataType) || input$dataType == " "){
          return()
        }
        else if(is.null(input$breakpointsCheckbox)){
         return()
        }
        else if(is.null(input$ewsRadioButtons)){
         return()
        }
        # indicates breakpoint lines can be drawn
        else if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=TPanalysis()[[2]], col="blue")
          # update plot legend
          legend("topleft", c(input$dataType, "Breakpoints"), lty=c(1, 2),
                 col=c(1, "blue"), bty="n")
        }

        ### end: draw breakpoint lines on main plot ###

        ### start: update plot and legend with ews line ###

        # variable used to adjust ews-line start value
        ewsLineTime <- input$days * 1440

        # display default plot attributes if there are no ews lines selected
        if(input$ewsRadioButtons == "None"){
          return()
        }
        # draw ews line based on radio button selection
        else if(input$ewsRadioButtons == "Standard Deviation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[3]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Skewness"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[4]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Kurtosis"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[5]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Coefficient of Variation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[6]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Return Rate"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[7]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Density Ratio"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[8]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[9]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[2]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$dataType, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$dataType, input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, "green"), bty="n")
          }
        }
      }


        ### end: update plot and legend with ews line ###
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

    quickTP <- reactive({
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
              generic_ews(timeseries=ppSim()[[2]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
          }

          # for photosynthesis
          else if(input$quick_dataType == "Photosynthesis"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[3]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[3]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
          }

          # for biological oxygen demand
          if(input$quick_dataType == "Biological Oxygen Demand"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[4]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[2]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
          }

          # for nutrients
          else if(input$quick_dataType == "Nutrients"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[5]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[5]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
          }

          # for augmentation value
          else if(input$quick_dataType == "Augmentation Value"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[6]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[6]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
          }

          # for food amount
          else if(input$quick_dataType == "Food Amount"){
            # decompose simulated data
            decomposed <- decompose(ts(ppSim()[[7]],
                                       frequency=input$quick_frequency))

            # run ews analysis on desired component
            if(input$quick_decomposeOptions == "Observed (Simulated Data)"){
              generic_ews(timeseries=ppSim()[[7]],
                          detrending="gaussian", winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Trend"){
              x <- decomposed$trend[!is.na(decomposed$trend)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Seasonal (Periodicity)"){
              x <- decomposed$seasonal[!is.na(decomposed$seasonal)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
            }
            else if(input$quick_decomposeOptions == "Random (Residuals)"){
              x <- decomposed$random[!is.na(decomposed$random)]
              generic_ews(timeseries=x, detrending="gaussian",
                          winsize=input$quick_winsize)
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
