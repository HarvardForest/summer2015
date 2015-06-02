### Lotka-Volterra predator-prey model
# ui.R

shinyUI(fluidPage(
  titlePanel("Lotka-Volterra predator-prey model"),

  sidebarLayout(position="left",
    sidebarPanel(
      numericInput("initPrey",
        label=h4("Number of Prey:"),
        value=0
      ),
      numericInput("initPredator",
        label=h4("Number of Predators:"),
        value=0
      ),
      sliderInput("alpha",
        label=h4("The growth rate of prey:"),
        min=0, max=10, value=0, step=0.001
      ),
      sliderInput("beta",
        label=h4("The rate at which predators kill prey:"),
        min=0, max=10, value=0, step=0.001
      ),
      sliderInput("delta",
        label=h4("The death rate of predators:"),
        min=0, max=10, value=0, step=0.001
      ),
      sliderInput("gamma",
        label=h4("The rate at which predators increase by consuming prey:"),
        min=0, max=10, value=0, step=0.001
      )
    ),
    mainPanel("main panel",
      plotOutput("plot")
    )
  )
))
