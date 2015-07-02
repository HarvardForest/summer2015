### Pitcher Plant model
## By: Nathan Justice
# Last edited: 01July2015

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
        if(input$quick_ewsRadioButtons == "Show all"){
          return()
        }
        # draw ews line based on radio button selection
        else if(input$quick_ewsRadioButtons == "Standard Deviation"){
          # adjust starting point to accomodate rolling window size (10%)
          ewsLine <- quickGeneric()[4]
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
          for(i in 1:(ewsLineTime * 0.1)){
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
      if(input$tabset_analyses != "Quick Analysis"){
        return()
      }
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "Show all"){
        return()
      }

      plotOutput("ewsMainPlot")
    })

    output$ewsMainPlot <- renderPlot({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "Show all"){
        return()
      }

      # variable used to adjust ews-line start value
      ewsLineTime = input$days * 1440

      if(input$quick_ewsRadioButtons == "Standard Deviation"){
        x <- quickGeneric()[3]
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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
        for(i in 1:(ewsLineTime * 0.1)){
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

      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quick_dataType == "Oxygen"){
            CE.Normal(ppSim()[2], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Photosynthesis"){
            CE.Normal(ppSim()[3], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            CE.Normal(ppSim()[4], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Nutrients"){
            CE.Normal(ppSim()[5], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Augmentation Value"){
            CE.Normal(ppSim()[6], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quick_dataType == "Food Amount"){
            CE.Normal(ppSim()[7], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
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

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quick_dataType == "Oxygen"){
            generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quick_dataType == "Photosynthesis"){
            generic_ews(timeseries=subset(ppSim(), select="Photosynthesis"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quick_dataType == "Biological Oxygen Demand"){
            generic_ews(timeseries=subset(ppSim(),
                          select="Biological Oxygen Demand"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quick_dataType == "Nutrients"){
            generic_ews(timeseries=subset(ppSim(), select="Nutrients"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quick_dataType == "Augmentation Value"){
            generic_ews(timeseries=subset(ppSim(), select="Augmentation Value"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quick_dataType == "Food Amount"){
            generic_ews(timeseries=subset(ppSim(), select="Food Amount"),
                        detrending="gaussian", winsize=10)
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
    output$ewsRadioButtonSlot <- renderUI({
        # check required information
        if(is.null(input$quick_dataType) || input$quick_dataType == " "){
          return()
        }

        radioButtons("quick_ewsRadioButtons", "View Early Warning Signal Analysis:",
                     c("Show all", "Standard Deviation", "Skewness", "Kurtosis",
                       "Coefficient of Variation", "Return Rate", "Density Ratio",
                       "Autocorrelation at First Lag",
                       "Autoregressive Coefficient"), selected=NULL, inline=FALSE)
    })

    # display aggregate plot matrix from generic_ews
    output$quick_ewsPlotMatrix <- renderPlot({
      # check required information
      if(is.null(input$quick_dataType) || input$quick_dataType == " "){
        return()
      }
      if(is.null(input$quick_ewsRadioButtons)){
        return()
      }
      else if(input$quick_ewsRadioButtons == "Show all"){
        # loading bar
        withProgress(message="Plotting Data", value=0, {
          withProgress(message="...", detail="This may take awhile.", value=0, {

            if(input$quick_dataType == "Oxygen"){
              plot_generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                               detrending="gaussian", winsize=10)
            }
            else if(input$quick_dataType == "Photosynthesis"){
              plot_generic_ews(timeseries=subset(ppSim(), select="Photosynthesis"),
                               detrending="gaussian", winsize=10)
            }
            else if(input$quick_dataType == "Biological Oxygen Demand"){
              plot_generic_ews(timeseries=subset(ppSim(),
                                                 select="Biological Oxygen Demand"),
                               detrending="gaussian", winsize=10)
            }
            else if(input$quick_dataType == "Nutrients"){
              plot_generic_ews(timeseries=subset(ppSim(), select="Nutriens"),
                               detrending="gaussian", winsize=10)
            }
            else if(input$quick_dataType == "Augmentation Value"){
              plot_generic_ews(timeseries=subset(ppSim(),
                                                 select="Augmentation Value"),
                               detrending="gaussian", winsize=10)
            }
            else if(input$quick_dataType == "Food Amount"){
              plot_generic_ews(timeseries=subset(ppSim(), select="Food Amount"),
                               detrending="gaussian", winsize=10)
            }

          }) # withProgress
        }) # withProgress
      }
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
      filename = function() { paste("PitcherPlantEWS", '.csv', sep='') },
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
      else if(input$quick_ewsRadioButtons == "Show all"){
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

    ### start: dynamic display of tipping point input-interface ###

    output$tpParametersText <- renderText({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      "Tipping Point parameters"
    })

    output$breakpointTypeSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      selectInput("breakpointType", "Analysis Type:",
                  choices=c("for Continuous Data",
                            "with Negative Binomial Distribution",
                          "with Zero-Inflated Negative Binomial Distribution"))
    })

    output$distributionTypeSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      selectInput("distributionType",
                  "Distribution to simulate break-point locations:",
                  choices=c("Four Parameter Beta Distribution",
                            "Truncated Normal Distribution"))
    })

    output$NmaxSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }
      numericInput("Nmax", "Maximum number of breakpoints:", value=10)
    })

    output$aSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("a", "Used in the four parameter beta distribution to smooth
        both shape parameters. When simulating from the truncated normal
        distribution, this value is used to smooth the estimates of the mean
        values:", value=0.8)
    })

    output$bSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("b", "A smoothing parameter value. It is used in the truncated
                   normal distribution to smooth the estimates of the standard
                   deviation:", value=0.8)
    })

    output$hSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("h", "Minimum aberration width:", value=5)
    })

    output$MSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("M", "Sample size to be used in simulating the locations of
        break-points:", value=200)
    })

    output$rhoSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("rho", "The fraction which is used to obtain the best
                    performing set of sample solutions (i.e., elite sample):",
                   value=0.05)
    })

    output$epsSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      numericInput("eps", "the cut-off value for the stopping criterion in the
                    CE method:", value=0.01)
    })

    output$breakpointDocumentation <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      helpText(a("Click here to view the R 'breakpoint' Package documentation.",
                href="http://cran.r-project.org/web/packages/breakpoint/breakpoint.pdf",
                target="_blank")
      )
    })

    output$runButtonSlot <- renderUI({
      # check required information
      if(is.null(input$dataType) || input$dataType == " "){
        return()
      }

      actionButton("runButton", "Run Analysis")
    })

    ### end: dynamic display of tipping point input-interface ###

    ### start: run (advanced) tipping point analysis based on user-input ###

    TPanalysis <- eventReactive(input$runButton, {
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
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
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
    output$tpOutput <- renderText({
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
      else if(is.null(input$breakpointType) || input$breakpointType == " "){
        return()
      }
      else if(is.null(input$distributionType) || (input$distributionType == " ")){
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

  } ## end server ##
)
