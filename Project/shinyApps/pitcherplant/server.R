### Pitcher Plant model
## By: Nathan Justice
# Last edited: 03July2015

### Pitcher Plant Simulation ###

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

################# Side Panel ###################################################

    ### start: load user-input boxes (parameters) ###

    output$days2 <- renderUI({
      numericInput("days2", label=NULL, value=input$days)
    })

    output$feedingTime2 <- renderUI({
      numericInput("feedingTime2", label=NULL, value=input$feedingTime)
    })

    output$foodWeight2 <- renderUI({
      numericInput("foodWeight2", label=NULL, value=input$foodWeight)
    })

    output$beta2 <- renderUI({
      numericInput("beta2", label=NULL, value=input$beta)
    })

    output$k2 <- renderUI({
      numericInput("k2", label=NULL, value=input$k)
    })

    output$Bscaler2 <- renderUI({
      numericInput("Bscaler2", label=NULL, value=input$Bscaler)
    })

    output$aMax2 <- renderUI({
      numericInput("aMax2", label=NULL, value=input$aMax)
    })

    output$aMin2 <- renderUI({
      numericInput("aMin2", label=NULL, value=input$aMin)
    })

    output$s2 <- renderUI({
      numericInput("s2", label=NULL, value=input$s)
    })

    output$d2 <- renderUI({
      numericInput("d2", label=NULL, value=input$d)
    })

    output$c2 <- renderUI({
      numericInput("c2", label=NULL, value=input$c)
    })

    ### end: load user-input boxes (parameters) ###

    # link user-input box values with respective slider values (parameters)
    observe({
      updateSliderInput(session, "days", value=input$days2)
      updateSliderInput(session, "feedingTime", value=input$feedingTime2)
      updateSliderInput(session, "foodWeight", value=input$foodWeight2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "k", value=input$k2)
      updateSliderInput(session, "Bscaler", value=input$Bscaler2)
      updateSliderInput(session, "aMax", value=input$aMax2)
      updateSliderInput(session, "aMin", value=input$aMin2)
      updateSliderInput(session, "s", value=input$s2)
      updateSliderInput(session, "d", value=input$d2)
      updateSliderInput(session, "c", value=input$c2)
    })

################################################################################

########### Display dynamic plot (main) and table of the simulation ############

    ### start: show simulation plot based on selector ###

    output$mainPlot <- renderPlot({
      if(input$tabset_analyses == "Quick Analysis" ||
         input$tabset_analyses == "Customize Graph"){
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
    })

    # simulation data table (main table)
    output$mainTable <- renderDataTable({
      ppSim()
    })

    # download main table feature
    output$downloadMainTable <- downloadHandler(
      filename = function() { paste("PitcherPlant", '.csv', sep='') },
      content = function(file) {
        write.csv(ppSim(), file)
      }
    )

################################################################################

########## Dynamic plot for (quick) ews analysis - below main plot #############

    output$ewsMainPlotSlot <- renderUI({
      # check required information
      if(input$tabset_analyses == "Quick Analysis"){
        if(is.null(input$quick_dataType) || input$quick_dataType == " "){
          return()
        }
        if(is.null(input$quick_ewsRadioButtons)){
          return()
        }
        else if(input$quick_ewsRadioButtons == "None"){
          return()
        }
      }
      else if(input$tabset_analyses == "Advanced Analysis"){
        if(is.null(input$dataType) || input$dataType == " "){
          return()
        }
        if(is.null(input$ewsRadioButtons)){
          return()
        }
        else if(input$ewsRadioButtons == "None"){
          return()
        }
      }
      else if(input$tabset_analyses == "Customize Graph"){
        return()
      }

      plotOutput("ewsMainPlot")
    })

    output$ewsMainPlot <- renderPlot({
      # check required information
      if(input$tabset_analyses == "Quick Analysis"){
        if(is.null(input$quick_dataType) || input$quick_dataType == " "){
          return()
        }
        if(is.null(input$quick_ewsRadioButtons)){
          return()
        }
        else if(input$quick_ewsRadioButtons == "None"){
          return()
        }

        # variable used to adjust ews-line start value
        ewsLineTime = input$days * 1440

        if(input$quick_ewsRadioButtons == "Standard Deviation"){
          x <- quickGeneric()[3]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Skewness"){
          x <- quickGeneric()[4]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Kurtosis"){
          x <- quickGeneric()[5]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
          x <- quickGeneric()[6]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Return Rate"){
          x <- quickGeneric()[7]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Density Ratio"){
          x <- quickGeneric()[8]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
          x <- quickGeneric()[9]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
          x <- quickGeneric()[2]
          for(i in 1:(ewsLineTime * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }
      }

      if(input$tabset_analyses == "Advanced Analysis"){
        if(is.null(input$dataType) || input$dataType == " "){
          return()
        }
        if(is.null(input$ewsRadioButtons)){
          return()
        }
        else if(input$ewsRadioButtons == "None"){
          return()
        }

        # variable used to adjust ews-line start value
        ewsLineTime = input$days * 1440

        if(input$ewsRadioButtons == "Standard Deviation"){
          x <- advancedGeneric()[3]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Skewness"){
          x <- advancedGeneric()[4]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Kurtosis"){
          x <- advancedGeneric()[5]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Coefficient of Variation"){
          x <- advancedGeneric()[6]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Return Rate"){
          x <- advancedGeneric()[7]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Density Ratio"){
          x <- quickGeneric()[8]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
          x <- advancedGeneric()[9]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
          x <- advancedGeneric()[2]
          for(i in 1:(ewsLineTime * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }
      }
    })

################################################################################

############## Quick Analysis ##################################################

    ### start: predetermined (quick) breakpoint and ews analyses ###

    # reactive for dynamic updates
    quickTP <- reactive({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.na(input$quick_Nmax)){
        return()
      }
      if(is.na(input$quick_winsize)){
        return()
      }

      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quick_dataType == "Oxygen"){
            CE.Normal(ppSim()[2], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Photosynthesis"){
            CE.Normal(ppSim()[3], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            CE.Normal(ppSim()[4], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Nutrients"){
            CE.Normal(ppSim()[5], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Augmentation Value"){
            CE.Normal(ppSim()[6], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Food Amount"){
            CE.Normal(ppSim()[7], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }

        }) # withProgress
      }) # withProgress
    })

    # reactive for dynamic updates
    quickGeneric <- reactive({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.na(input$quick_Nmax)){
        return()
      }
      if(is.na(input$quick_winsize)){
        return()
      }

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quick_dataType == "Oxygen"){
            generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }
          else if(input$quick_dataType == "Photosynthesis"){
            generic_ews(timeseries=subset(ppSim(), select="Photosynthesis"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            generic_ews(timeseries=subset(ppSim(),
                          select="Biological Oxygen Demand"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }
          else if(input$quick_dataType == "Nutrients"){
            generic_ews(timeseries=subset(ppSim(), select="Nutrients"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }
          else if(input$quick_dataType == "Augmentation Value"){
            generic_ews(timeseries=subset(ppSim(), select="Augmentation Value"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }
          else if(input$quick_dataType == "Food Amount"){
            generic_ews(timeseries=subset(ppSim(), select="Food Amount"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }

        }) # withProgress
      }) # withProgress
    })

    ### end: predetermined (quick) breakpoint and ews analyses ###

    ### start: (quick) breakpoint analysis output ###

    # display "Number of breakpoints detected:" text and value
    output$quick_numBreakpoints <- renderText({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
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

      # display only if breakpoints are detected
      if(length(quickTP()) > 1){
        checkboxInput("quick_breakpointsCheckbox", "Draw Breakpoint Lines",
                      value=FALSE)
      }
    })

    ### end: (quick) breakpoint analysis output ###

    ### start: (quick) ews analysis output ###

    # display ews radio buttons
    output$quick_ewsRadioButtonSlot <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }

      radioButtons("quick_ewsRadioButtons", "View Early Warning Signal Analysis:",
                   c("None", "Standard Deviation", "Skewness", "Kurtosis",
                     "Coefficient of Variation", "Return Rate", "Density Ratio",
                     "Autocorrelation at First Lag",
                     "Autoregressive Coefficient"), selected=NULL, inline=FALSE)
    })

    output$quick_downloadTable <- renderUI({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.null(input$quick_ewsRadioButtons)){
        return()
      }

      downloadButton('downloadQuickTable', 'Download Data')
    })

    # download ews data
    output$downloadQuickTable <- downloadHandler(
      filename = function() { paste("PitcherPlantEWS(quick)", '.csv', sep='') },
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
      if(is.null(input$quick_ewsRadioButtons)){
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

    ### end: (quick) ews analysis output ###

################################################################################

#################### Advanced Tipping Point Analysis ###########################

    ### start: run (advanced) tipping point analysis based on user-input ###

    TPanalysis <- reactive({
      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          # for oxygen
          if(input$dataType == "Oxygen"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[2], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[2], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[2], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[2], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[2], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[2], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for photosynthesis
          else if(input$dataType == "Photosynthesis"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[3], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[3], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[3], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[3], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[3], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[3], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for biological oxygen demand
          else if(input$dataType == "Biological Oxygen Demand"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[4], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[4], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[4], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[4], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[4], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[4], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for nutrients
          else if(input$dataType == "Nutrients"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[5], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[5], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[5], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[5], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[5], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[5], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for augmentation value
          else if(input$dataType == "Augmentation Value"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[6], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[6], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[6], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[6], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[6], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[6], distyp=2, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
            }
          }

          # for food amount
          else if(input$dataType == "Food Amount"){
            if(input$breakpointType == "with Negative Binomial Distribution"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.NB(ppSim()[7], distyp=1, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.NB(ppSim()[7], distyp=2, parallel=FALSE, Nmax=input$Nmax,
                  eps=input$eps, rho=input$rho, M=input$M, h=input$h, a=input$a,
                  b=input$b)
              }
            }
            else if(input$breakpointType == "for Continuous Data"){
              if(input$distributionType == "Four Parameter Beta Distribution"){
                CE.Normal(ppSim()[7], distyp=1, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
              else if(input$distributionType == "Truncated Normal Distribution"){
                CE.Normal(ppSim()[7], distyp=2, parallel=FALSE,
                  Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                  h=input$h, a=input$a, b=input$b)
              }
            }
            else if(input$breakpointType ==
              "with Zero-Inflated Negative Binomial Distribution"){
                if(input$distributionType == "Four Parameter Beta Distribution"){
                  CE.ZINB(ppSim()[7], distyp=1, parallel=FALSE,
                    Nmax=input$Nmax, eps=input$eps, rho=input$rho, M=input$M,
                    h=input$h, a=input$a, b=input$b)
                }
                else if(input$distributionType == "Truncated Normal Distribution"){
                  CE.ZINB(ppSim()[7], distyp=2, parallel=FALSE,
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

      print(length(TPanalysis()))

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

          if(input$dataType == "Oxygen"){
            generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Photosynthesis"){
            generic_ews(timeseries=subset(ppSim(), select="Photosynthesis"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Biological Oxygen Demand"){
            generic_ews(timeseries=subset(ppSim(),
                          select="Biological Oxygen Demand"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Nutrients"){
            generic_ews(timeseries=subset(ppSim(), select="Nutrients"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Augmentation Value"){
            generic_ews(timeseries=subset(ppSim(), select="Augmentation Value"),
                        winsize=input$winsize, bandwidth=input$bandwidth,
                        detrending=input$detrending, span=input$span,
                        degree=input$degree, logtransform=input$logtransform,
                        interpolate=input$interpolate, AR_n=input$AR_n,
                        powerspectrum=input$powerspectrum)
          }
          else if(input$dataType == "Food Amount"){
            generic_ews(timeseries=subset(ppSim(), select="Food Amount"),
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
      filename = function() { paste("PitcherPlantEWS(advanced)", '.csv', sep='') },
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

  } ## end server ##
)
