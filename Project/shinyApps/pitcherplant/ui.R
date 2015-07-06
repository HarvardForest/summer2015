### Pitcher Plant model
## By: Nathan Justice
# Last edited: 03July2015

### User Interface ###

# load dependencies
source("global.R", local=TRUE)

shinyUI(fluidPage(
  titlePanel("Pitcher Plant Simulation"),

  sidebarLayout(position="left",
    sidebarPanel(
      sliderInput("days", label=h4("Number of Days:"), value=2, min=1, max=50, step=1),
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
                      )
                    ), # column
                    column(7,
                      br(),
                      h3(textOutput("quick_numBreakpoints")),
                      h3(textOutput("quick_locationText")),
                      h5(textOutput("quick_tpOutput")),
                      uiOutput("quick_breakpointsCheckboxSlot"),
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
                      )
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
                      h3("Tipping Point parameters"),
                      selectInput("breakpointType", "Analysis Type:",
                        choices=c("for Continuous Data",
                          "with Negative Binomial Distribution",
                          "with Zero-Inflated Negative Binomial Distribution")
                      ),
                      numericInput("Nmax", "Maximum number of breakpoints:", value=5),
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

        tabPanel(title="Model (in R)",
          pre(code("

            light <- function(days){
              out <- sin(2*pi*(1:720)/1440)
              out[out < 0] <- 0
              out <- c(rep(0,720/2), out, rep(0,720/2))
              rep(out, days)
            }

            PAR <- function(days, rise=6, set=18){
              out <- rep(0, 1440)
              out[(rise*60):(set*60)] <- 1
              rep(out, days)
            }

            pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=5, beta=0.005, k=1, Bscaler=1,
                                        aMax=10, aMin=1, s=10, d=0.5, c=100) {

              minute <- vector(mode='numeric') # t/time variable
              x <- vector(mode='numeric') # amount of o2
              a <- vector(mode='numeric') # augmentation function
              P <- vector(mode='numeric') # photosynthesis
              B <- vector(mode='numeric') # biological o2 demand
              n <- vector(mode='numeric') # amount of nutrients
              w <- vector(mode='numeric') # amount of food


              ## Initialization ##

              # simulate photosynthesis as fixed values
              P <- light(days)*PAR(days=days)

              # initial nutrient value
              n <- 0

              # initial augmentation value
              a <- ((aMax-aMin)/(1+exp((-s*n)-d)))+aMin

              # initial biological o2 demand
              B <- 0/(k+0)

              # o2 at minute=0, P=0 b/c unable to index at minute=0
              x <- (a*0)-B

              # simulate until food is first added
              # loop runs until feedingTime-2 b/c food is added AT the minute
              for(i in 1:(feedingTime-2)){
                # augmentation function - default value
                a <- c(a, ((aMax-aMin)/(1+exp((-s*n[i])-d)))+aMin)

                # biological oxygen demand - default value (no food = no microbes)
                B <- c(B, 0/(k+0))

                # calculate o2 amount - product of photosynthesis alone (no food)
                x <- c(x, (a[i]*P[i])-B[i])

                # amount of food - no food
                w <- c(w, 0)

                # amount of nutrients - no nutrients
                n <- c(n, 0)

                # adjust minute
                minute <- c(minute, i)
              }

              # adjust minute
              minute <- c(minute, length(minute)+1)

              # adjust amount of food
              w <- c(w, w[length(w)])

              for(z in 1:days){
                # add food
                w <- c(w, w[length(w)]+foodWeight)

                # run simulation for a full day
                for(j in 1:1440){
                  # adjust minute
                  minute <- c(minute, length(minute)+1)

                  # adjust biological o2 demand
                  B <- c(B, (w[length(minute)]/(k+w[length(minute)]))*Bscaler)

                  # adjust amount of nutrients
                  n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

                  # adjust augmentation value
                  a <- c(a, ((aMax-aMin)/(1+exp((-s*n[length(minute)])-d)))+aMin)

                  # adjust o2 amount
                  tempO2 <- (a[length(minute)]*P[length(minute)])-B[length(minute)]
                  if(is.na(tempO2) == FALSE && tempO2 > 0){
                    x <- c(x, tempO2)
                  }
                  else{
                    x <- c(x, 0)
                  }

                  if(j < 1440){
                      ## adjust amount of food
                      w <- c(w, w[length(w)]*exp(-beta*(1)))
                  }
                }
              }

              # trim objects to appropriate time
                # omitted values aren't relevant
              minute <- minute[1:length(P)]
              B <- B[1:length(P)]
              n <- n[1:length(P)]
              a <- a[1:length(P)]
              x <- x[1:length(P)]
              w <- w[1:length(P)]

              data <- data.frame(minute, x, P[1:length(x)], B, n, a, w)
              colnames(data) <- c('Minute', 'Oxygen', 'Photosynthesis',
                                  'Biological Oxygen Demand', 'Nutrients',
                                  'Augmentation Value', 'Food Amount')
              return(data)

            }"

          )) # pre & code
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


