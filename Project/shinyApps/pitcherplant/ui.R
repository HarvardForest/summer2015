### Pitcher Plant model
## By: Nathan Justice
# Last edited: 30June2015

### User Interface ###

# load dependencies
source("global.R", local=TRUE)

shinyUI(fluidPage(
  titlePanel("Pitcher Plant Simulation"),

  sidebarLayout(position="left",
    sidebarPanel(
      sliderInput("days", label=h4("Number of Days:"), value=5, min=1, max=50, step=1),
      uiOutput("days2"),
      sliderInput("feedingTime", label=h4("Feeding Time (in minutes):"), value=720, min=1, max=1440, step=1),
      uiOutput("feedingTime2"),
      sliderInput("foodWeight", label=h4("Food Weight:"), value=5, min=1, max=100),
      uiOutput("foodWeight2"),
      sliderInput("beta", label=h4("Beta Value:"), value=0.0005, min=0.00000000001, max=0.09, step=0.000001),
      uiOutput("beta2"),
      sliderInput("aMax", label=h4("Maximum Value of Augmentation:"), value=10, min=1, max=50),
      uiOutput("aMax2"),
      sliderInput("aMin", label=h4("Minimum Value of Augmentation:"), value=1, min=0, max=50),
      uiOutput("aMin2"),
      sliderInput("Bscaler", label=h4("Scale Biological Oxygen Demand Value by:"), value=1, min=1, max=100),
      uiOutput("Bscaler2"),
      sliderInput("k", label=h4("k Value:"), value=1, min=1, max=10),
      uiOutput("k2"),
      sliderInput("s", label=h4("s Value:"), value=10, min=1, max=50),
      uiOutput("s2"),
      sliderInput("d", label=h4("d Value:"), value=0.5, min=0, max=5),
      uiOutput("d2"),
      sliderInput("c", label=h4("c Value:"), value=100, min=1, max=1000),
      uiOutput("c2")
    ), # sidebarPanel

    mainPanel(
      tabsetPanel(
        tabPanel(title="Graph",
          fluidRow(
            column(12,
              plotOutput("mainPlot"),
              uiOutput("ewsMainPlot")
            ) # column
          ), # fluidRow
          fluidRow(
            column(12, align="center",
              tabsetPanel(
                tabPanel(title="Quick Analysis",
                  fluidRow(
                    column(4,
                      br(),
                      br(),
                      selectInput("quick_dataType", "Choose Data",
                        choices=c(" ", "Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount")
                      ),
                      h3(textOutput("quick_numBreakpoints")),
                      h3(textOutput("quick_locationText")),
                      h5(textOutput("quick_tpOutput")),
                      uiOutput("breakpointCheckbox")
                    ), # column
                    column(8,
                      br(),
                      uiOutput("ewsRadioButton"),
                      br(),
                      uiOutput("quick_downloadTable")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      br(),
                      uiOutput("quick_ewsTable"),
                      plotOutput("quickGenericPlot")
                    ) # column
                  ) # fluidRow
                ), # tabPanel - Quick Analysis

                tabPanel(title="Advanced Analysis",
                  tabsetPanel(
                    tabPanel(title="Advanced Tipping Point",
                      fluidRow(
                        column(5, align="left",
                          br(),
                          helpText(a("Click here to view the R 'breakpoint' Package
                            documentation.", href="http://cran.r-project.org/web/packages/breakpoint/breakpoint.pdf",
                            target="_blank")
                          ), # helpText
                          br(),
                          selectInput("tpDataType", "Choose Data",
                            choices=c(" ", "Oxygen", "Photosynthesis",
                                      "Biological Oxygen Demand", "Nutrients",
                                      "Augmentation Value", "Food Amount")
                          ),
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
                            target="_blank")
                          ),
                          helpText(a("Click here to visit the Early Warnings Signals Toolbox website.",
                            href="http://www.early-warning-signals.org/", target="_blank")
                          ),
                          br(),
                          selectInput("ewsDataType", "Choose Data:",
                            choices=c(" ", "Prey", "Predator")
                          ),
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
                  textInput("yaxis", "y-axis", value="Value"),
                  textInput("xaxis", "x-axis", value="Time (minutes)")
                ) # tabPanel - Customize graph
              ) # tabsetPanel
            ) # column
          ) # fluidRow
        ), # tabPanel - Graph

        tabPanel(title="Data Table",
          fluidRow(
            br(),
            downloadButton('downloadMainTable', 'Download Table'),
            br(),
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
)) # end UI


