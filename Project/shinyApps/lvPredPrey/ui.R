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
        min=0, max=10, value=1.5, step=0.001
      ),
      uiOutput("alpha2"),
      sliderInput("beta",
        label=h4("The rate at which predators kill prey:"),
        min=0, max=10, value=0.02, step=0.001
      ),
      uiOutput("beta2"),
      sliderInput("delta",
        label=h4("The death rate of predators:"),
        min=0, max=10, value=0.4, step=0.001
      ),
      uiOutput("delta2"),
      sliderInput("gamma",
        label=h4("The rate at which predators increase by consuming prey:"),
        min=0, max=10, value=0.01, step=0.001
      ),
      uiOutput("gamma2"),
      fluidRow(
        br(),
        column(12,
          downloadButton("downloadGraph", "Download Graph"),
          downloadButton("downloadData", "Download Data")
        ) # column
      ) # fluidRow
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(title="Graph",
          fluidRow(
            plotOutput("plot"),
            column(12, align="center",
              tabsetPanel(
                tabPanel(title="Tipping Point analysis"), # tabPanel
                tabPanel(title="Early Warning Signal analysis"),
                tabPanel(title="Customize graph",
                    br(),
                    br(),
                    textInput("plotTitle", "Plot Titile", value="Predator-Prey Model"),
                    textInput("yaxis", "y-axis", value="Population"),
                    textInput("xaxis", "x-axis", value="Time"),
                    textInput("preyLabel", "Prey", value="Prey"),
                    textInput("predatorLabel", "Predator", value="Predator")
                ) # tabPanel - Customize graph
              ) # tabsetPanel
            ) # column
          ) # fluidRow
        ), # tabPanel - Graph
        tabPanel(title="Data Table",
          fluidRow(
            dataTableOutput("table")
          ) # fluidRow
        ) # tabPanel - Data Table
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
))
