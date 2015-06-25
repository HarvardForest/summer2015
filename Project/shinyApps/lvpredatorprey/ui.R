### Lotka-Volterra predator-prey model
## By: Nathan Justice
# Last edited: 26June2015

### User Interface ###

# load dependencies
source("global.R", local=TRUE)

shinyUI(fluidPage(
  useShinyjs(),
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
      uiOutput("gamma2")
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(title="Graph",
          fluidRow(
            plotOutput("mainPlot"),
            column(12, align="center",
              tabsetPanel(
                tabPanel(title="Quick Analysis",
                  fluidRow(
                    column(4,
                      br(),
                      br(),
                      selectInput("quickDataType", "Choose Data",
                                  choices=c(" ", "Prey", "Predator")),
                      h3(textOutput("quickNumBreakpoints")),
                      h3(textOutput("quickLocationText")),
                      h5(textOutput("quickTPAnalysis2")),
                      uiOutput("breakpointsCheckboxSlot")
                    ), # column
                    column(8,
                      br(),
                      uiOutput("radioButtonSlot")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      br(),
                      uiOutput("quickMainTableSlot"),
                      plotOutput("quickGenericPlotSlot")
                    ) # column
                  ) # fluidRow
                ), # tabPanel - Quick Analysis
                tabPanel(title="Advanced Analysis",
                  tabsetPanel(
                    tabPanel(title="Advanced Tipping Point",
                      fluidRow(
                        column(5, align="left",
                          br(),
                          helpText(
                            a("Click here to view the R 'breakpoint' Package
                              documentation.",
                              href="http://cran.r-project.org/web/packages/breakpoint/breakpoint.pdf",
                              target="_blank")
                          ), # helpText
                          br(),
                          selectInput("dataType", "Choose Data:",
                                      choices=c(" ", "Prey", "Predator")),
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
                          h4(textOutput("numBreakpointsText")),
                          h3(textOutput("tpAnalysis1")),
                          h3(textOutput("locationText")),
                          h4(textOutput("tpAnalysis2"))
                        ) # column
                      ), # fluidRow
                      br(),
                      uiOutput("breakpointPlotSlot"),
                      br(),
                      uiOutput("profilePlotTitleSlot"),
                      uiOutput("profilePlotSlot"),
                      br()
                    ), # tabPanel - Tipping Point analysis
                    tabPanel(title="Advanced Early Warning Signal",
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
                          selectInput("ewsDataType", "Choose Data:",
                                      choices=c(" ", "Prey", "Predator")),
                          uiOutput("ews1"),
                          uiOutput("ews2"),
                          uiOutput("ews3"),
                          uiOutput("ews4")
                        ), # column
                        column(5, offset=1, align="center",
                               br(),
                               br(),
                               uiOutput("ews5"),
                               uiOutput("ews6"),
                               uiOutput("ews7"),
                               uiOutput("ews8"),
                               uiOutput("ews9"),
                               uiOutput("ews10"),
                               uiOutput("ews11"),
                               uiOutput("ews12"),
                               uiOutput("ews13"),
                               uiOutput("ews14"),
                               uiOutput("ews15"),
                               uiOutput("ews16"),
                               uiOutput("ews17"),
                               uiOutput("ewsRun"),
                               br()
                        ) # column
                      ), # fluidRow
                      fluidRow(
                        column(12,
                          uiOutput("generic_ewsTableGuideSlot"),
                          uiOutput("generic_ewsTableSlot")
                        )
                      ), # fluidRow
                      fluidRow(
                        column(12, align="left",
                          br(),
                          uiOutput("generic_ewsPlotSlot"),
                          br(),
                          uiOutput("qda_ewsDetailSlot"),
                          br(),
                          uiOutput("qda_ewsPlot1Slot"),
                          br(),
                          uiOutput("qda_ewsData1Slot"),
                          br(),
                          uiOutput("qda_ewsPlot2Slot"),
                          br(),
                          uiOutput("qda_ewsData2Slot"),
                          br(),
                          uiOutput("qda_ewsPlot3Slot")
                        ) # column
                      ) # fluidRow
                    ) # tabPanel - Early Warning Signal analysis
                  ) # tabsetPanel
                ), # tabPanel- Advanced Analysis
                tabPanel(title="Customize Graph",
                    br(),
                    br(),
                    textInput("plotTitle", "Plot Titile",
                              value="Predator-Prey Model"),
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
            br(),
            h4(helpText("Data tables can be copied and pasted into spreadsheet software!")),
            br(),
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
          h3("R:"),
          p("R Core Team (2015). R: A language and environment for statistical computing.
              Foundation for Statistical Computing, Vienna, Austria.
              URL http://www.R-project.org/."),
          h3("shiny Package:"),
          p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2015).
            shiny: Web Application Framework for R. R package version 0.12.0.
            http://CRAN.R-project.org/package=shiny"),
          h3("shinyapps Package:"),
          p("JJ Allaire (2013). shinyapps: Interface to ShinyApps. R package version 0.3.64."),
          h3("breakpoint (Tipping Point) Package:"),
          p("Priyadarshana W.J.R.M. and Georgy Sofronov (2014). breakpoint: Multiple
              Break-Point Detection via the Cross-Entropy Method. R package version 1.1.
              http://CRAN.R-project.org/package=breakpoint"),
          h3("earlywarnings Package:"),
          p("Vasilis Dakos et al. Methods for detecting early warnings of critical transitions
              in time series illustrated using simulated ecological dataPLoS One 7(7):e41010, 2012. See
              http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0041010"),
          h3("deSolve Package (for dfferential equations):"),
          p("Karline Soetaert, Thomas Petzoldt, R. Woodrow Setzer (2010). Solving Differential Equations in R:
            Package deSolve Journal of Statistical Software, 33(9), 1--25.
            URL http://www.jstatsoft.org/v33/i09/."),
          h3("ggplot2:"),
          p("H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.")
        ) # tabPanel
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
))
