#### Lotka-Volterra Predator Prey Model
### Nathan Justice
## Last edited: 06July2015

################## Lotka-Volterra Predator-Prey ################################

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

################# Side Panel ###################################################

    ### start: load user-input boxes (parameters) ###

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

    ### end: load user-input boxes (parameters) ###

    # link user-input box values with respective slider values (parameters)
    observe({
      updateSliderInput(session, "alpha", value=input$alpha2)
      updateSliderInput(session, "beta", value=input$beta2)
      updateSliderInput(session, "delta", value=input$delta2)
      updateSliderInput(session, "gamma", value=input$gamma2)
    })

################################################################################

########### Display dynamic plot (main) and table of the simulation ############

    ### start: show simulation plot  ###

    output$mainPlot <- renderPlot({
      if(input$tabset_analyses == "Quick Analysis" ||
         input$tabset_analyses == "Customize Graph"){

        matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis)
        title(main=input$plotTitle)
        legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
               col=c(1, 2), bty="n")
      }

      if(input$tabset_analyses == "Advanced Analysis"){

        matplot(lvPredPrey(), type="l", xlab=input$xaxis, ylab=input$yaxis)
        title(main=input$plotTitle)
        legend("topleft", c(input$preyLabel, input$predatorLabel), lty=c(1, 2),
               col=c(1, 2), bty="n")

      }

      ### end: show simulation plot ###

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
          legend("topleft", c(input$preyLabel, input$predatorLabel, "Breakpoints"),
                 lty=c(1, 2), col=c(1, 2, "blue"), bty="n")
        }

        ### end: draw breakpoint lines on main plot ###

        ### start: update plot and legend with ews line ###

        # display default plot attributes if there are no ews lines selected
        if(input$quick_ewsRadioButtons == "None"){
          return()
        }
        # draw ews line based on radio button selection
        else if(input$quick_ewsRadioButtons == "Standard Deviation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[3]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                    input$quick_ewsRadioButtons), lty=c(1, 2),
                   col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Skewness"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[4]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Kurtosis"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[5]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[6]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Return Rate"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[7]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Density Ratio"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[8]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[9]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[2]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$quick_breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$quick_ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$quick_ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
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
          legend("topleft", c(input$preyLabel, input$predatorLabel, "Breakpoints"),
                 lty=c(1, 2), col=c(1, 2, "blue"), bty="n")
        }

        ### end: draw breakpoint lines on main plot ###

        ### start: update plot and legend with ews line ###

        # display default plot attributes if there are no ews lines selected
        if(input$ewsRadioButtons == "None"){
          return()
        }
        # draw ews line based on radio button selection
        else if(input$ewsRadioButtons == "Standard Deviation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[3]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Skewness"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[4]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Kurtosis"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[5]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Coefficient of Variation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[6]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Return Rate"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[7]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Density Ratio"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[8]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft", c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[9]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- advancedGeneric()[2]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            ewsLine <- rbind(NA, ewsLine)
          }
          # draw ews line
          matlines(ewsLine, type='l', col="green")

          if(input$breakpointsCheckbox == TRUE) {
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            # update plot legend with ews and breakpoint lines
            legend("topleft",c(input$preyLabel, input$predatorLabel, "Breakpoints",
                               input$ewsRadioButtons),
                   lty=c(1, 2), col=c(1, 2, "blue", "green"), bty="n")
          }
          else{
            # update plot legend with only ews line
            legend("topleft", c(input$preyLabel, input$predatorLabel,
                                input$ewsRadioButtons),
                    lty=c(1, 2), col=c(1, 2, "green"), bty="n")
          }
        }
      }

      ### end: update plot and legend with ews line ###
    })

    # simulation data table (main table)
    output$mainTable <- renderDataTable({
      lvPredPrey()
    })

    # download main table feature
    output$downloadMainTable <- downloadHandler(
      filename = function() { paste("PredatorPreySim", '.csv', sep='') },
      content = function(file) {
        write.csv(lvPredPrey(), file)
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

        if(input$quick_ewsRadioButtons == "Standard Deviation"){
          x <- quickGeneric()[3]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Skewness"){
          x <- quickGeneric()[4]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Kurtosis"){
          x <- quickGeneric()[5]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Coefficient of Variation"){
          x <- quickGeneric()[6]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Return Rate"){
          x <- quickGeneric()[7]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Density Ratio"){
          x <- quickGeneric()[8]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autocorrelation at First Lag"){
          x <- quickGeneric()[9]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
          if(input$quick_breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=quickTP()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$quick_ewsRadioButtons == "Autoregressive Coefficient"){
          x <- quickGeneric()[2]
          for(i in 1:(input$time * (input$quick_winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$quick_ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$quick_breakpointsCheckbox)){
            return()
          }
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

        if(input$ewsRadioButtons == "Standard Deviation"){
          x <- advancedGeneric()[3]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Skewness"){
          x <- advancedGeneric()[4]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Kurtosis"){
          x <- advancedGeneric()[5]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Coefficient of Variation"){
          x <- advancedGeneric()[6]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Return Rate"){
          x <- advancedGeneric()[7]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Density Ratio"){
          x <- advancedGeneric()[8]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autocorrelation at First Lag"){
          x <- advancedGeneric()[9]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
          if(input$breakpointsCheckbox == TRUE){
            # include breakpoint lines
            abline(v=TPanalysis()[[2]], col="blue")
            legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                   bty="n")
          }
        }

        else if(input$ewsRadioButtons == "Autoregressive Coefficient"){
          x <- advancedGeneric()[2]
          for(i in 1:(input$time * (input$winsize * 0.01))){
            x <- rbind(NA, x)
          }
          # display ews plot
          matplot(x, type='l', col="green", ylab=input$ewsRadioButtons,
                  xlab="Time (minutes)")

          # draw breakpoint lines if checkbox button is selected
          if(is.null(input$breakpointsCheckbox)){
            return()
          }
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

          # for prey
          if(input$quick_dataType == "Prey"){
            CE.Normal(lvPredPrey()[1], distyp=1, parallel=FALSE,
                      Nmax=input$quick_Nmax, eps=0.01, rho=0.05, M=200, h=5,
                      a=0.8, b=0.8)
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            CE.Normal(lvPredPrey()[2], distyp=1, parallel=FALSE,
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

          # for prey
          if(input$quick_dataType == "Prey"){
            generic_ews(timeseries=subset(lvPredPrey(), select="prey"),
                        detrending="gaussian", winsize=input$quick_winsize)
          }

          # for predator
          else if(input$quick_dataType == "Predator"){
            generic_ews(timeseries=subset(lvPredPrey(), select="predator"),
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
