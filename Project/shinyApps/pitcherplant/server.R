### Pitcher Plant model
## By: Nathan Justice
# Last edited: 29June2015

### Pitcher Plant Simulation ###

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

############## Display dynamic plot and table of the simulation ################

    # simulation plot
    output$mainPlot <- renderPlot({
      if(input$plotSelector == "Oxygen"){
        matplot(x=ppSim()[1], y=ppSim()[2], type="l",
                xlab="Time (minutes)", ylab="O2 level", pch=1)
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Photosynthesis"){
        matplot(x=ppSim()[1], y=ppSim()[3], type="p",
                xlab="Time (minutes)", ylab="O2 level", pch=1)
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Biological Oxygen Demand"){
        matplot(x=ppSim()[1], y=ppSim()[4], type="p",
                xlab="Time (minutes)", ylab="O2 level", pch=1)
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Nutrients"){
        matplot(x=ppSim()[1], y=ppSim()[5], type="p",
                xlab="Time (minutes)", ylab="Nutrient level", pch=1)
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Augmentation"){
        matplot(x=ppSim()[1], y=ppSim()[6], type="p",
                xlab="Time (minutes)", ylab="Augmentation value", pch=1)
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Food Amount"){
        matplot(x=ppSim()[1], y=ppSim()[7], type="p",
                xlab="Time (minutes)", ylab="Food level", pch=1)
        title(main=input$plotSelector)
      }
    })

    # simulation data table (main table)
    output$mainTable <- renderDataTable({
      ppSim()
    })

    # download main table
    output$downloadMainTable <- downloadHandler(
      filename = function() { paste("PitcherPlant", '.csv', sep='') },
      content = function(file) {
        write.csv(ppSim(), file)
      }
    )
################################################################################

  } ## end server ##
)
