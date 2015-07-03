### Pitcher Plant model
## By: Nathan Justice
# Last edited: 01July2015

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
      sliderInput("beta", label=h4("Beta Value:"), value=0.0005, min=0.00001, max=0.1),
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
              uiOutput("ewsMainPlotSlot")
            ) # column
          ), # fluidRow
          fluidRow(
            column(12, align="center",
              tabsetPanel(id="tabset_analyses",
                tabPanel(title="Quick Analysis",
                  fluidRow(
                    column(5,
                      br(),
                      br(),
                      selectInput("quick_dataType", "Choose Data:",
                        choices=c(" ", "Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount")
                      ),
                      numericInput("quick_Nmax",
                        "Maximum number of breakpoints for Tipping Point analysis:",
                        value=5
                      ),
                      numericInput("quick_winsize",
                                   "Size of the rolling window used in the Early Warning Signals
                                    analysis (expressed as a percentage of the timeseries):",
                                   value=50
                      ),
                      h3(textOutput("quick_numBreakpoints")),
                      h3(textOutput("quick_locationText")),
                      h5(textOutput("quick_tpOutput")),
                      uiOutput("quick_breakpointsCheckboxSlot")
                    ), # column
                    column(7,
                      br(),
                      uiOutput("quick_ewsRadioButtonSlot"),
                      br(),
                      uiOutput("quick_downloadTable")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      br(),
                      dataTableOutput("quick_ewsTable")
                    ) # column
                  ) # fluidRow
                ), # tabPanel - Quick Analysis

                tabPanel(title="Advanced Analysis",
                  fluidRow(
                    column(12, align="center",
                      br(),
                      selectInput("dataType", "Choose Data:",
                        choices=c(" ", "Oxygen", "Photosynthesis",
                                  "Biological Oxygen Demand", "Nutrients",
                                  "Augmentation Value", "Food Amount")
                      ),
                      uiOutput("runButtonSlot")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(7, align="left",
                      h3(textOutput("numBreakpoints")),
                      h3(textOutput("locationText")),
                      h5(textOutput("tpOutput")),
                      uiOutput("breakpointsCheckboxSlot")
                    ), # column
                    column(5, align="center",
                      uiOutput("ewsRadioButtonSlot"),
                      br(),
                      uiOutput("ewsTableCheckboxSlot"),
                      uiOutput("downloadEWStableSlot")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(12,
                      br(),
                      dataTableOutput("ewsTable")
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(6, align="left",
                      h3(textOutput("tpParametersText")),
                      uiOutput("breakpointTypeSlot"),
                      uiOutput("NmaxSlot"),
                      uiOutput("distributionTypeSlot"),
                      uiOutput("epsSlot"),
                      uiOutput("rhoSlot"),
                      uiOutput("MSlot"),
                      uiOutput("hSlot"),
                      uiOutput("aSlot"),
                      uiOutput("bSlot"),
                      uiOutput("breakpointDocumentation")
                    ), # column
                    column(6, align="center",
                      h3(textOutput("ewsParametersText")),
                      uiOutput("winsizeSlot"),
                      uiOutput("detrendingSlot"),
                      uiOutput("bandwidthSlot"),
                      uiOutput("spanSlot"),
                      uiOutput("degreeSlot"),
                      uiOutput("logtransformSlot"),
                      uiOutput("interpolateSlot"),
                      uiOutput("AR_nSlot"),
                      uiOutput("powerspectrumSlot"),
                      uiOutput("ewsDocumentation1"),
                      uiOutput("ewsDocumentation2")
                    ) # column
                  ) # fluidRow
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
          h1("HELLO WORLD")
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


