################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 20July2015 ##################################
################################################################################
################################################################################


##### Shiny user interface #####

### start: interface ###

shinyUI(fluidPage(
  theme=shinytheme("flatly"),
  headerPanel(title="Lotka-Volterra Predator-Prey model",
              windowTitle="Predator-Prey model"),

  sidebarLayout(position="left",

    ### start: sidebar ###

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

    ### end: sidebar ###

    ### start: mainpanel ###

    mainPanel(
      tabsetPanel(

        ### start: simulation panel ###

        tabPanel(title="Simulation",
          fluidRow(
            column(12,
              plotOutput("mainPlot")
            ) # column
          ), # fluidRow
          fluidRow(
            column(12, offset=8,
              uiOutput("plotOptionsSlot")
            ) # column
          ),
          fluidRow(
            column(12, align="center",
              tabsetPanel(id="tabset_analyses",

                ### start: quick analysis panel ###

                tabPanel(title="Quick Analysis",
                  fluidRow(
                    column(4,
                      br(),
                      br(),
                      selectInput("quick_dataType", "Choose Data for Analysis:",
                        choices=c(" ", "Prey", "Predator")
                      ),
                      uiOutput("quick_frequencySlot"),
                      uiOutput("quick_decomposeOptionsSlot"),
                      uiOutput("quick_NmaxSlot"),
                      uiOutput("quick_winsizeSlot"),
                      uiOutput("quick_runButtonSlot")
                    ), # column
                    column(8,
                      br(),
                      h4(textOutput("quick_numBreakpoints")),
                      h4(textOutput("quick_locationText")),
                      h5(textOutput("quick_tpOutput")),
                      uiOutput("quick_breakpointsCheckboxSlot"),
                      uiOutput("quick_ewsRadioButtonSlot"),
                      uiOutput("quick_downloadTable"),
                      uiOutput("quick_ewsTableCheckboxSlot"),
                      dataTableOutput("quick_ewsTable"),
                      uiOutput("quick_decomposePlotSlot")
                    ) # column
                  ) # fluidRow
                ), # tabPanel - Quick Analysis

                ### end: quick analysis panel ###

                ### start: advanced analysis panel ###

                tabPanel(title="Advanced Analysis",
                  fluidRow(
                    column(12, align="left",
                      br(),
                      helpText("Check that all input-boxes have valid values."),
                      br()
                    ) # column
                  ), # fluidRow
                  fluidRow(
                    column(4, align="left",
                      selectInput("dataType", "Choose Data for Analysis:",
                        choices=c(" ", "Prey", "Predator")
                      ),
                      uiOutput("frequencySlot"),
                      uiOutput("decomposeOptionsSlot"),
                      uiOutput("runButtonSlot")
                    ), # column
                    column(8, align="center",
                      h4(textOutput("numBreakpoints")),
                      h4(textOutput("locationText")),
                      h5(textOutput("tpOutput")),
                      uiOutput("breakpointsCheckboxSlot"),
                      uiOutput("ewsRadioButtonSlot"),
                      uiOutput("downloadEWStableSlot"),
                      uiOutput("ewsTableCheckboxSlot")
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
                      h3("Tipping Point parameters"),
                      selectInput("breakpointType", "Analysis Type:",
                        choices=c("for Continuous Data",
                          "with Negative Binomial Distribution",
                          "with Zero-Inflated Negative Binomial Distribution")
                      ),
                      numericInput("Nmax", "Maximum number of breakpoints:", value=10),
                      selectInput("distributionType", "Distribution to simulate break-point locations:",
                        choices=c("Four Parameter Beta Distribution",
                          "Truncated Normal Distribution")
                      ),
                      numericInput("eps",
                        "the cut-off value for the stopping criterion in the CE method:",
                        value=0.01
                      ),
                      numericInput("rho",
                        "The fraction which is used to obtain the best performing set of sample solutions (i.e., elite sample):",
                        value=0.05
                      ),
                      numericInput("M",
                        "Sample size to be used in simulating the locations of break-points:",
                        value=200
                      ),
                      numericInput("h", "Minimum aberration width:", value=5),
                      numericInput("a",
                        "Used in the four parameter beta distribution to smooth both shape parameters.
                          When simulating from the truncated normal distribution,
                          this value is used to smooth the estimates of the mean values:",
                        value=0.8
                      ),
                      numericInput("b", "A smoothing parameter value. It is used in the truncated
                        normal distribution to smooth the estimates of the standard deviation:",
                        value=0.8
                      ),
                      helpText(a("Click here to view the R 'breakpoint' Package documentation.",
                        href="http://cran.r-project.org/web/packages/breakpoint/breakpoint.pdf",
                        target="_blank")
                      )
                    ), # column
                    column(6, align="center",
                      h3("Early Warning Signals parameters"),
                      numericInput("winsize", "The size of the rolling window expressed as
                        percentage of the timeseries length (must be numeric between 0 and 100):",
                        value=50
                      ),
                      numericInput("bandwidth",
                        "Bandwidth used for the Gaussian kernel when gaussian filtering s applied.
                        It is expressed as percentage of the timeseries length (must be numeric between 0 and 100):",
                        value=5
                      ),
                      selectInput("detrending", "Detrended/filtered prior to analysis:",
                        choices=c("gaussian", "loess", "linear", "first-diff", "no")
                      ),
                      numericInput("span",
                        "Parameter that controls the degree of smoothing (numeric between 0 and 100):",
                        value=25
                      ),
                      numericInput("degree",
                        "The degree of polynomial to be used for when loess fitting is applied, normally 1 or 2:",
                        value=2
                      ),
                      selectInput("logtransform",
                        "If TRUE data are logtransformed prior to analysis as log(X+1):",
                        choices=c(FALSE, TRUE)
                      ),
                      selectInput("interpolate",
                        "If TRUE linear interpolation is applied to produce a timeseries of
                          equal length as the original. (FALSE assumes there are no gaps in the timeseries):",
                        choices=c(FALSE, TRUE)
                      ),
                      selectInput("AR_n",
                        "If TRUE the best fitted AR(n) model is fitted to the data:",
                        choices=c(FALSE, TRUE)
                      ),
                      selectInput("powerspectrum",
                        "If TRUE the power spectrum within each rolling window is plotted:",
                        choices=c(FALSE, TRUE)
                      ),
                      helpText(a("Click here to view the R 'earlywarnings' Package documentation.",
                        href="http://cran.r-project.org/web/packages/earlywarnings/earlywarnings.pdf",
                        target="_blank")
                      ),
                      helpText(a("Click here to visit the Early Warnings Signals Toolbox website.",
                        href="http://www.early-warning-signals.org/", target="_blank")
                      )
                    ) # column
                  ) # fluidRow
                ), # tabPanel- Advanced Analysis

                ### end: advanced analysis panel ###

                ### start: customize graph panel ###

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

                ### end: customize graph panel ###

              ) # tabsetPanel - "tabset_analyses"
            ) # column
          ) # fluidRow
        ), # tabPanel - Simulation

        ### end: simulation panel ###

        ### start: data table panel ###

        tabPanel(title="Data Table",
          fluidRow(
            br(),
            h2("Data Table for Model Simulation:"),
            br(),
            downloadButton('downloadMainTable', 'Download Table'),
            br(),
            br(),
            dataTableOutput("mainTable")
          ) # fluidRow
        ), # tabPanel - Data Table

        ### end: data table panel ###

        ### start: model panel ###

        tabPanel(title="Model",
          h2("Lotka-Volterra predator-prey model"),
          br(),
          h4("Ordinary Differential Equation:"),
          h3("dx <- (alpha * prey) - (beta * prey * predator)"),
          h3("dy <- (gamma * prey * predator) - (delta * predator)"),
          br(),
          h5("alpha = the growth rate of prey"),
          h5("beta = the rate at which predators kill prey"),
          h5("delta = the death rate of predators"),
          h5("gamma = the rate at which predators increase by consuming prey"),
          h3("R code:"),
          pre(code(textOutput("codeText")))
        ), # tabPanel - Model

        ### end: model panel ###

        ### start: R code panel ###

        tabPanel(title="R Code",
          h3("Read-only"),
          aceEditor("ace", value=" "),
          fluidRow(
            column(12, align="center",
              downloadButton("aceDownloadButton", "Download Script")
            )
          ),
          textOutput("Temp")
        ), # tabPanel - R

        ### end: R code panel ###

        ### start references panel ###

        tabPanel(title="References",
          br(),
          h3("R:"),
          p("R Core Team (2015). R: A language and environment for statistical computing.
              Foundation for Statistical Computing, Vienna, Austria.
              URL http://www.R-project.org/."),
          h3("'shiny' Package:"),
          p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2015).
            shiny: Web Application Framework for R. R package version 0.12.0.
            http://CRAN.R-project.org/package=shiny"),
          h3("'shinyapps' Package:"),
          p("JJ Allaire (2013). shinyapps: Interface to ShinyApps. R package version 0.3.64."),
          h3("'shinythemes' Package:"),
          p("Winston Chang (2015). shinythemes: Themes for Shiny. R package version 1.0.1.
            http://CRAN.R-project.org/package=shinythemes"),
          h3("'shinyAce' Package:"),
          p("Trestle Technology and LLC. (2013). shinyAce: Ace editor bindings for Shiny. R package version 0.1.0.
            http://CRAN.R-project.org/package=shinyAce"),
          h3("'breakpoint' Package:"),
          p("Priyadarshana W.J.R.M. and Georgy Sofronov (2014). breakpoint: Multiple
              Break-Point Detection via the Cross-Entropy Method. R package version 1.1.
              http://CRAN.R-project.org/package=breakpoint"),
          h3("'earlywarnings' Package:"),
          p("Vasilis Dakos et al. Methods for detecting early warnings of critical transitions
              in time series illustrated using simulated ecological dataPLoS One 7(7):e41010, 2012. See
              http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0041010"),
          h3("'deSolve' Package:"),
          p("Karline Soetaert, Thomas Petzoldt, R. Woodrow Setzer (2010). Solving Differential Equations in R:
            Package deSolve Journal of Statistical Software, 33(9), 1--25.
            URL http://www.jstatsoft.org/v33/i09/."),
          h3("'plotrix' Package:"),
          p("Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.")
        ) # tabPanel

        ### end: references panel ###

      ) # tabsetPanel
    ) # mainPanel

    ### end: mainpanel ###

  ) # sidebarLayout
)) # end UI

### end: interface ###
