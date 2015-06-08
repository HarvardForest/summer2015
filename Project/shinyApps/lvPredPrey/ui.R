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
                tabPanel(title="Tipping Point analysis",
                  fluidRow(
                    column(5, align="left",
                      br(),
                      helpText(a("Click here to view the R 'breakpoint' Package documentation.",
                                  href="http://cran.r-project.org/web/packages/breakpoint/breakpoint.pdf",
                                  target="_blank")),
                      br(),
                      selectInput("dataType", "Choose Data:", choices=c(" ", "Prey", "Predator")),
                      uiOutput("tpOne"),
                      uiOutput("tpTwo"),
                      uiOutput("tpRun")
                    ), # column
                    column(5, offset=1, align="center",
                      br(),
                      h4("Number of breakpoints detected:"),
                      h3(textOutput("tpAnalysis_one")),
                      h3(textOutput("location")),
                      h4(textOutput("tpAnalysis_two")),
                      br(),
                      uiOutput("plotLinesButton")
                    ) # column
                  ), # fluidRow
                  plotOutput("plot2")
                ), # tabPanel - Tipping Point analysis
                tabPanel(title="Early Warning Signal analysis",
                  fluidRow(
                    column(5, align="left",
                      br(),
                      helpText(a("Click here to view the R 'earlywarnings' Package documentation.",
                                href="http://cran.r-project.org/web/packages/earlywarnings/earlywarnings.pdf",
                                target="_blank")),
                      helpText(a("Click here to visit the Early Warnings Signals Toolbox website.",
                                href="http://www.early-warning-signals.org/",
                                target="_blank")),
                      br(),
                      selectInput("dataType_2", "Choose Data:", choices=c(" ", "Prey", "Predator")),
                      uiOutput("ewsOne"),
                      uiOutput("e1"),
                      uiOutput("e2"),
                      uiOutput("e3")
                    ), # column
                    column(5, offset=1, align="center",
                           br(),
                           br(),
                           uiOutput("e4"),
                           uiOutput("e5"),
                           uiOutput("e6"),
                           uiOutput("e7"),
                           uiOutput("e8"),
                           uiOutput("e9"),
                           uiOutput("e10"),
                           uiOutput("e11"),
                           uiOutput("ewsRun"),
                           br()
                    )
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      plotOutput("ewsPlot"),
                      hr(),
                      dataTableOutput("ewsTable")
                    ) #column
                  ) #fluidRow
                ), # tabPanel - Early Warning Signal analysis
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
        ), # tabPanel - Data Table
        tabPanel(title="Model",
          h2("Lotka-Volterra predator-prey model"),
          br(),
          h4("Ordinary Differential Equation:"),
          br(),
          h3("dx <- (alpha * prey) - (beta * prey * predator)"),
          h3("dy <- (gamma * prey * predator) - (delta * predator)"),
          br(),
          h5("alpha = the growth rate of prey"),
          h5("beta = the rate at which predators kill prey"),
          h5("delta = the death rate of predators"),
          h5("gamma = the rate at which predators increase by consuming prey")
        ) # tabPanel - Source
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
))
