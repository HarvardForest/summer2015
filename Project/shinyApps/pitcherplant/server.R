### Pitcher Plant model
## By: Nathan Justice
# Last edited: 17June2015

### Pitcher Plant Simulation ###

## start server ##
shinyServer(
  function(input, output, session){

    # declare instance of the simulation
    ppSim <- reactive({
      pitcherPlantSim(days=3, feedingTime=720, foodWeight=5, beta=0.0005, k=1,
        Bscaler=10, aMax=10, aMin=1, s=10, d=0.5, c=100)
    })
############## Display dynamic plot and table of the simulation ################

    # simulation plot
    output$mainPlot <- renderPlot({
      if(input$plotSelector == "Oxygen"){
        matplot(x=ppSim()[1], y=ppSim()[2], type="p",
                xlab="Time (minutes)", ylab="O2 level")
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Photosynthesis"){
        matplot(x=ppSim()[1], y=ppSim()[3], type="p",
                xlab="Time (minutes)", ylab="O2 level")
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Biological Oxygen Demand"){
        matplot(x=ppSim()[1], y=ppSim()[4], type="p",
                xlab="Time (minutes)", ylab="O2 level")
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Nutrients"){
        matplot(x=ppSim()[1], y=ppSim()[5], type="p",
                xlab="Time (minutes)", ylab="Nutrient level")
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Augmentation"){
        matplot(x=ppSim()[1], y=ppSim()[6], type="p",
                xlab="Time (minutes)", ylab="Augmentation value")
        title(main=input$plotSelector)
      }
      else if(input$plotSelector == "Food Amount"){
        matplot(x=ppSim()[1], y=ppSim()[7], type="p",
                xlab="Time (minutes)", ylab="Food level")
        title(main=input$plotSelector)
      }
    })

################################################################################

  } ## end server ##
)
