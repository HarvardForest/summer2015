### Pitcher Plant model
## By: Nathan Justice
# Last edited: 30June2015

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
      if(input$quickDataType == "Oxygen" || input$quickDataType == " "){
        matplot(x=ppSim()[1], y=ppSim()[2], type="l",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title("Oxygen")
        legend("topleft", "Oxygen", lty=c(1, 2), col=(1), bty="n")
      }
      else if(input$quickDataType == "Photosynthesis"){
        matplot(x=ppSim()[1], y=ppSim()[3], type="p",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title(main=input$quickDataType)
        legend("topleft", input$quickDataType, lty=c(1, 2), col=(1), bty="n")
      }
      else if(input$quickDataType == "Biological Oxygen Demand"){
        matplot(x=ppSim()[1], y=ppSim()[4], type="p",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title(main=input$quickDataType)
        legend("topleft", input$quickDataType, lty=c(1, 2), col=(1), bty="n")
      }
      else if(input$quickDataType == "Nutrients"){
        matplot(x=ppSim()[1], y=ppSim()[5], type="p",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title(main=input$quickDataType)
        legend("topleft", input$quickDataType, lty=c(1, 2), col=(1), bty="n")
      }
      else if(input$quickDataType == "Augmentation Value"){
        matplot(x=ppSim()[1], y=ppSim()[6], type="p",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title(main=input$quickDataType)
        legend("topleft", input$quickDataType, lty=c(1, 2), col=(1), bty="n")
      }
      else if(input$quickDataType == "Food Amount"){
        matplot(x=ppSim()[1], y=ppSim()[7], type="p",
                xlab=input$xaxis, ylab=input$yaxis, pch=1)
        title(main=input$quickDataType)
        legend("topleft", input$quickDataType, lty=c(1, 2), col=(1), bty="n")
      }

      ### end: show simulation plot based on selector ###

      ### start: draw breakpoint lines on main plot ###

      # check if breakpoint lines and ews lines can be drawn
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      else if(is.null(input$breakpointsCheckbox)){
       return()
      }
      else if(is.null(input$radioButtons)){
       return()
      }
      # indicates breakpoint lines can be drawn
      else if(input$breakpointsCheckbox == TRUE) {
        # include breakpoint lines
        abline(v=quickTP()[[2]], col="blue")
        # update plot legend
        legend("topleft", c(input$quickDataType, "Breakpoints"), lty=c(1, 2),
               col=c(1, "blue"), bty="n")
      }

      ### end: draw breakpoint lines on main plot ###

      ### start: update plot and legend with ews line ###

      # display default plot attributes if there are no ews lines selected
      if(input$radioButtons == "Show all"){
        return()
      }
      # draw ews line based on radio button selection
      else if(input$radioButtons == "Standard Deviation"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[3]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Skewness"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[4]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Kurtosis"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[5]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Coefficient of Variation"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[6]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Return Rate"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[7]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Density Ratio"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[8]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Autocorrelation at First Lag"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[9]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
        }
      }

      else if(input$radioButtons == "Autoregressive Coefficient"){
        # adjust starting point to accomodate rolling window size (10%)
        temp <- quickGeneric()[2]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          temp <- rbind(NA, temp)
        }

        # draw ews line
        matlines(temp, type='l', col="green")

        if(input$breakpointsCheckbox == TRUE) {
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          # update plot legend with ews and breakpoint lines
          legend("topleft",c(input$quickDataType, "Breakpoints",
                             input$radioButtons),
                 lty=c(1, 2), col=c(1, "blue", "green"), bty="n")
        }
        else {
          # update plot legend with only ews line
          legend("topleft", c(input$quickDataType, input$radioButtons),
                 lty=c(1, 2), col=c(1, "green"), bty="n")
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
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
        return()
      }
      else if(input$radioButtons == "Show all"){
        return()
      }

      plotOutput("ewsMainPlot")
    })

    output$ewsMainPlot <- renderPlot({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
        return()
      }
      else if(input$radioButtons == "Show all"){
        return()
      }

      if(input$radioButtons == "Standard Deviation"){
        x <- quickGeneric()[3]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Skewness"){
        x <- quickGeneric()[4]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Kurtosis"){
        x <- quickGeneric()[5]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Coefficient of Variation"){
        x <- quickGeneric()[6]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Return Rate"){
        x <- quickGeneric()[7]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Density Ratio"){
        x <- quickGeneric()[8]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Autocorrelation at First Lag"){
        x <- quickGeneric()[9]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
          # include breakpoint lines
          abline(v=quickTP()[[2]], col="blue")
          legend("topleft", c("Breakpoints"), lty=c(1, 2), col=c("blue"),
                 bty="n")
        }
      }

      else if(input$radioButtons == "Autoregressive Coefficient"){
        x <- quickGeneric()[2]
        tempTime = input$days * 1440
        for(i in 1:(tempTime * 0.1)){
          x <- rbind(NA, x)
        }

        # display ews plot
        matplot(x, type='l', col="green", ylab=input$radioButtons,
                xlab="Time (minutes)")

        # draw breakpoint lines if checkbox button is selected
        if(input$breakpointsCheckbox == TRUE){
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
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }

      # loading bar
      withProgress(message="Determining Breakpoints", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          # for oxygen
          if(input$quickDataType == "Oxygen"){
            CE.Normal(ppSim()[2], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quickDataType == "Photosynthesis"){
            CE.Normal(ppSim()[3], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quickDataType == "Biological Oxygen Demand"){
            CE.Normal(ppSim()[4], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quickDataType == "Nutrients"){
            CE.Normal(ppSim()[5], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quickDataType == "Augmentation Value"){
            CE.Normal(ppSim()[6], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }
          else if(input$quickDataType == "Food Amount"){
            CE.Normal(ppSim()[7], distyp=1, parallel=FALSE, Nmax=10,
                      eps=0.01, rho=0.05, M=200, h=5, a=0.8, b=0.8)
          }

        }) # withProgress
      }) # withProgress
    })

    # reactive for dynamic updates
    quickGeneric <- reactive({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }

      # loading bar
      withProgress(message="Performing EWS Analysis", value=0,{
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quickDataType == "Oxygen"){
            generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quickDataType == "Photosynthesis"){
            generic_ews(timeseries=subset(ppSim(), select="Photosynthesis"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quickDataType == "Biological Oxygen Demand"){
            generic_ews(timeseries=subset(ppSim(),
                          select="Biological Oxygen Demand"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quickDataType == "Nutrients"){
            generic_ews(timeseries=subset(ppSim(), select="Nutrients"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quickDataType == "Augmentation Value"){
            generic_ews(timeseries=subset(ppSim(), select="Augmentation Value"),
                        detrending="gaussian", winsize=10)
          }
          else if(input$quickDataType == "Food Amount"){
            generic_ews(timeseries=subset(ppSim(), select="Food Amount"),
                        detrending="gaussian", winsize=10)
          }

        }) # withProgress
      }) # withProgress
    })

    ### end: predetermined (quick) breakpoint and ews analyses ###

    ### start: (quick) breakpoint analysis and output ###

    # display "Number of breakpoints detected:" text
    output$quickNumBreakpoints <- renderText({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }

      # display text only if breakpoints are detected
      if(length(quickTP()) > 1){
        c("Number of breakpoints detected:", quickTP()[[1]])
      }
    })

    # display "Location:" text
    output$quickLocationText <- renderText({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }

      # display text only if breakpoints are detected
      if(length(quickTP()) > 1){
        "Location(s):"
      }
    })

    # display locations of detected breakpoints
    output$quickTPanalysis <- renderText({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
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
    output$breakpointsCheckboxSlot <- renderUI({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }

      # display only if breakpoints are detected
      if(length(quickTP()) > 1){
        checkboxInput("breakpointsCheckbox", "Draw Breakpoint Lines",
                      value=FALSE)
      }
    })

    ### end: (quick) breakpoint analysis and output ###

    ### start: (quick) ews analysis and output ###

    # display ews radio buttons
    output$radioButtonSlot <- renderUI({
        # check required information
        if(is.null(input$quickDataType) || input$quickDataType == " "){
          return()
        }

        radioButtons("radioButtons", "View Early Warning Signal Analysis:",
                     c("Show all", "Standard Deviation", "Skewness", "Kurtosis",
                       "Coefficient of Variation", "Return Rate", "Density Ratio",
                       "Autocorrelation at First Lag",
                       "Autoregressive Coefficient"), selected=NULL, inline=FALSE)
    })

    # display aggregate plot matrix from generic_ews
    output$quickGenericPlot <- renderPlot({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
        return()
      }
      else if(input$radioButtons != "Show all"){
        return()
      }

      # loading bar
      withProgress(message="Plotting Data", value=0, {
        withProgress(message="...", detail="Please Wait", value=0, {

          if(input$quickDataType == "Oxygen"){
            plot_generic_ews(timeseries=subset(ppSim(), select="Oxygen"),
                             detrending="gaussian", winsize=10)
          }

        }) # withProgress
      }) # withProgress
    })

    output$downloadQuickTableSlot <- renderUI({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
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

    # display ews breakdown table
    output$quickMainTableSlot <- renderUI({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
        return()
      }
      else if(input$radioButtons == "Show all"){
        return()
      }

      dataTableOutput("quickMainTable")
    })

    # fill ews breakdown table with appropriate data based on radio buttons
    output$quickMainTable <- renderDataTable({
      # check required information
      if(is.null(input$quickDataType) || input$quickDataType == " "){
        return()
      }
      if(is.null(input$radioButtons)){
        return()
      }
      else if(input$radioButtons == "Show all"){
        return()
      }

      # check radio buttons value
      if(input$radioButtons == "Standard Deviation"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[3])
        colnames(table) <- c("Time Index", "Standard Deviation")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Skewness"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[4])
        colnames(table) <- c("Time Index", "Skewness")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Kurtosis"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[5])
        colnames(table) <- c("Time Index", "Kurtosis")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Coefficient of Variation"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[6])
        colnames(table) <- c("Time Index", "Coefficient of Variation")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Return Rate"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[7])
        colnames(table) <- c("Time Index", "Return Rate")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Density Ratio"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[8])
        colnames(table) <- c("Time Index", "Density Ratio")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Autocorrelation at First Lag"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[9])
        colnames(table) <- c("Time Index", "Autocorrelation at First Lag")

        # return table with updated column names
        return(table)
      }
      else if(input$radioButtons == "Autoregressive Coefficient"){
        # use ews time-index determination and associated value
        table <- cbind(timeIndex=quickGeneric()[1], quickGeneric()[2])
        colnames(table) <- c("Time Index", "Autoregressive Coefficient")

        # return table with updated column names
        return(table)
      }
    }, options=list(pageLength=10))

  ### end: (quick) ews analysis and output ###

################################################################################

  } ## end server ##
)
