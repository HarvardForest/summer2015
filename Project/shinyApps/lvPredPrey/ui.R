### Lotka-Volterra predator-prey model
# ui.R

shinyUI(fluidPage(
  titlePanel("Lotka-Volterra predator-prey model"),

  sidebarLayout(position="left",
    sidebarPanel(
      numericInput("prey",
        label=h4("Number of Prey:"),
        value=500
      ),
      numericInput("predators",
        label=h4("Number of Predators:"),
        value=10
      ),
      numericInput("time",
        label=h4("Time:"),
        value=100
      ),
      sliderInput("alpha",
        label=h4("The growth rate of prey:"),
        min=0, max=5, value=1.5, step=0.001
      ),
      uiOutput("alpha2"),
      sliderInput("beta",
        label=h4("The rate at which predators kill prey:"),
        min=0, max=3, value=0.02, step=0.001
      ),
      uiOutput("beta2"),
      sliderInput("delta",
        label=h4("The death rate of predators:"),
        min=0, max=3, value=0.4, step=0.001
      ),
      uiOutput("delta2"),
      sliderInput("gamma",
        label=h4("The rate at which predators increase by consuming prey:"),
        min=0, max=3, value=0.01, step=0.001
      ),
      uiOutput("gamma2")
    ),
    mainPanel(
      plotOutput("plot"),
      fluidRow(
        column(12, align="center",
          h3("Customize graph:"),
          textInput("yaxis", "y-axis", value="Population"),
          textInput("xaxis", "x-axis", value="Time"),
          textInput("preyLabel", "Prey", value="Prey"),
          textInput("predatorLabel", "Predator", value="Predator")
        )
      )
    )
  )
))
