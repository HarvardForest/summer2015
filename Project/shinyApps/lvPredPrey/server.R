### Lotka-Volterra predator-prey model
# server.R

shinyServer(
  function(input, output, session){

    theModel <- reactive({
     lotVpredPrey(seq(0, 100, by=1),
                  c(prey=input$prey, predator=input$predators),
                  c(alpha=input$alpha, beta=input$beta, delta=input$delta,
                    gamma=input$gamma))
    })

    output$plot <- renderPlot({
      matplot(theModel(), type="l", xlab="Time", ylab="Population")
      legend("topleft", c("Prey", "Predator"), lty=c(1, 2), col=c(1, 2),
             bty="n")
    })
  }
)
