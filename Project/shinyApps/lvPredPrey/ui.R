### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Last edited: 09June2015

# ui.R #

# load dependencies
library(shiny)
library(shinyapps)
library(deSolve)
library(breakpoint)
library(earlywarnings)
library(ggplot2)
library(Cairo)
options(shiny.usecairo=T)
source("global.R", local=TRUE)

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
          h4(helpText("Data tables can be copied and pasted into spreadsheet software!"))
        ) # column
      ) # fluidRow
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(title="Graph",
          fluidRow(
            plotOutput("mainPlot"),
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
                      uiOutput("tp1"),
                      uiOutput("tp2"),
                      uiOutput("tp3"),
                      uiOutput("tp4"),
                      uiOutput("tp5")
                    ), # column
                    column(5, offset=1, align="center",
                      br(),
                      uiOutput("tp6"),
                      uiOutput("tp7"),
                      uiOutput("tp8"),
                      uiOutput("tp9"),
                      uiOutput("tpRun"),
                      br(),
                      br(),
                      h4("Number of breakpoints detected:"),
                      h3(textOutput("tpAnalysis1")),
                      h3(textOutput("location")),
                      h4(textOutput("tpAnalysis2")),
                      br(),
                      uiOutput("plotLinesButton"),
                      br(),
                      uiOutput("tpProfileButtonSlot")
                    ) # column
                  ), # fluidRow
                  plotOutput("breakpointPlot"),
                  plotOutput("tpProfilePlot")
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
                      selectInput("ewsDataType", "Choose Data:", choices=c(" ", "Prey", "Predator")),
                      uiOutput("ews1"),
                      uiOutput("ews2"),
                      uiOutput("ews3"),
                      uiOutput("ews4"),
                      uiOutput("ews5")
                    ), # column
                    column(5, offset=1, align="center",
                           br(),
                           br(),
                           uiOutput("ews6"),
                           uiOutput("ews7"),
                           uiOutput("ews8"),
                           uiOutput("ews9"),
                           uiOutput("ews10"),
                           uiOutput("ews11"),
                           uiOutput("ews12"),
                           uiOutput("ewsRun"),
                           br(),
                           uiOutput("ewsPlotButtonSlot"),
                           br()
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      verbatimTextOutput("ewsTableGuide"),
                      dataTableOutput("ewsTable")
                    )
                  ), # fluidRow
                  fluidRow(
                    column(12, align="left",
                      br(),
                      plotOutput("ewsPlot", width="100%", height="100%")
                    ) # column
                  ) # fluidRow
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
            dataTableOutput("mainTable")
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
        ), # tabPanel - Model
        tabPanel(title="References",
          br(),
          tags$ul(
            tags$li(p("Priyadarshana, W. J. R. M. and Sofronov, G. (2012a) A Modified Cross- Entropy Method for Detecting Multiple Change-Points in DNA Count Data, In Proc. of the IEEE Conference on Evolutionary Computation (CEC), 1020-1027, DOI: 10.1109/CEC.2012.6256470.")),
            tags$li(p("Priyadarshana, W. J. R. M. and Sofronov, G. (2012b) The Cross-Entropy Method and Multiple Change-Points Detection in Zero-Inflated DNA read count data, In: Y. T. Gu, S. C. Saha (Eds.) The 4th International Conference on Computational Methods (ICCM2012), 1-8, ISBN 978-1-921897-54-2.")),
            tags$li(p("Rubinstein, R., and Kroese, D. (2004) The Cross-Entropy Method: A Unified Approach to Combinatorial Optimization, Monte-Carlo Simulation and Machine Learning. Springer-Verlag, New York.")),
            tags$li(p("Schwarz, G. (1978) Estimating the dimension of a model, The Annals of Statistics, 6(2), 461-464.")),
            tags$li(p("Priyadarshana, W. J. R. M., Sofronov G. (2014) Multiple Break-Points Detection in array CGH Data via the Cross-Entropy Method, IEEE/ACM Transactions on Computational Biology and Bioinformatics, no. 1, pp. 1, PrePrints, doi:10.1109/TCBB.2014.2361639, ISSN: 1545-5963.")),
            tags$li(p("Zhang, N.R., and Siegmund, D.O. (2007) A modified Bayes information criterion with applications to the analysis of comparative genomic hybridization data. Biometrics, 63, 22-32.")),
            tags$li(p("Priyadarshana, W. J. R. M. and Sofronov, G. (2012b) The Cross-Entropy Method and Multiple Change-Points Detection in Zero-Inflated DNA read count data, In: Y. T. Gu, S. C. Saha (Eds.) The 4th International Conference on Computational Methods (ICCM2012), 1-8, ISBN 978-1-921897-54-2.")),
            tags$li(p("Schwarz, G. (1978) Estimating the dimension of a model, The Annals of Statistics, 6(2), 461-464.")),
            tags$li(p("Ives, A. R. (1995). 'Measuring resilience in stochastic systems.' Ecological Monographs 65: 217-233")),
            tags$li(p("Dakos, V., et al (2008). 'Slowing down as an early warning signal for abrupt climate change.' Proceedings of the National Academy of Sciences 105(38): 14308-14312")),
            tags$li(p("Dakos, V., et al (2012).'Methods for Detecting Early Warnings of Critical Transitions in Time Series Illustrated Using Simulated Ecological Data.' PLoS ONE 7(7): e41010. doi:10.1371/journal.pone.0041010"))
          ) # unordered list
        ) # tabPanel
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
))
