################################################################################
################################################################################
################## Lotka-Volterra Predator-Prey ################################
####################### By: Nathan Justice #####################################
##################### Last edited: 31July2015 ##################################
################################################################################
################################################################################


##### Shiny user interface #####

### start: interface ###

shinyUI(fluidPage(
  theme=shinytheme("flatly"),
  headerPanel(title="EcoApps: Lotka-Volterra Predator-Prey model",
              windowTitle="Predator-Prey model"),

  sidebarLayout(position="left",

    ### start: sidebar ###

    sidebarPanel(
      numericInput("prey", label=h4("Number of Prey:"), value=500),
      numericInput("predators", label=h4("Number of Predators:"), value=10),
      numericInput("time", label=h4("Time:"), value=100),
      numericInput("alpha", label=h4("The growth rate of prey:"), value=1.5),
      numericInput("beta", label=h4("The rate at which predators kill prey:"), value=0.02),
      numericInput("delta", label=h4("The death rate of predators:"), value=0.4),
      numericInput("gamma", label=h4("The rate at which predators increase by consuming prey:"),
                   value=0.01
      )
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
                      uiOutput("quick_cpmTypeSlot"),
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
                      uiOutput("ewsTableCheckboxSlot"),
                      uiOutput("decomposePlotSlot")
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
                      selectInput("cpmType", "Change point model type:",
                                  choices=c("Gaussian sequence", "Exponential distribution")),
                      numericInput("startup", "The number of observations after which monitoring begins.
                                   No breakpoints will be flagged during this period.
                                   (the value should be at least 20):",
                                   value=20),
                      helpText(a("Click here to view the R 'cpm' Package documentation.",
                        href="https://cran.r-project.org/web/packages/cpm/cpm.pdf",
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
          h3("'cpm' Package:"),
          p(" Gordon J. Ross (2015). Parametric and Nonparametric Sequential Change Detection in R: The cpm Package.
            Journal of Statistical Software."),
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
