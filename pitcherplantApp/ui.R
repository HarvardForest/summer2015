################################################################################
################################################################################
#################### Pitcher Plant Simulation ##################################
####################### By: Nathan Justice #####################################
##################### Last edited: 31July2015 ##################################
################################################################################
################################################################################


##### Shiny user interface #####

### start: interface ###

shinyUI(fluidPage(
  theme=shinytheme("flatly"),
  titlePanel("EcoApps: Pitcher Plant Simulation"),

  sidebarLayout(position="left",

    ### start: sidebar ###

    sidebarPanel(
      numericInput("days", label=h4("Number of Days:"), value=3),
      numericInput("feedingTime", label=h4("Feeding Time (in minutes):"), value=720),
      numericInput("foodWeight", label=h4("Food Weight:"), value=5),
      numericInput("beta", label=h4("Beta Value:"), value=0.001),
      numericInput("aMax", label=h4("Maximum Value of Augmentation:"), value=10),
      numericInput("aMin", label=h4("Minimum Value of Augmentation:"), value=1),
      numericInput("Bscaler", label=h4("Scale Biological Oxygen Demand Value by:"), value=1),
      numericInput("k", label=h4("k Value:"), value=1),
      numericInput("s", label=h4("s Value:"), value=10),
      numericInput("d", label=h4("d Value:"), value=1),
      numericInput("c", label=h4("c Value:"), value=100)
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
            column(12, align="left",
              uiOutput("plotOptionsSlot")
            ) # column
          ), # fluidRow
          fluidRow(
            column(12, align="center",
              tabsetPanel(id="tabset_analyses",

                ### start: quick analysis panel ###

                tabPanel(title="Quick Analysis",
                  fluidRow(
                    column(4,
                      br(),
                      br(),
                      selectInput("quick_dataType", "Choose Data:",
                        choices=c(" ", "Oxygen", "Photosynthesis",
                          "Biological Oxygen Demand", "Nutrients",
                          "Augmentation Value", "Food Amount")
                      ),
                      uiOutput("quick_decomposeOptionsSlot"),
                      uiOutput("quick_cpmType"),
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
                      selectInput("dataType", "Choose Data:",
                        choices=c(" ", "Oxygen", "Photosynthesis",
                                  "Biological Oxygen Demand", "Nutrients",
                                  "Augmentation Value", "Food Amount")
                      ),
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
                  textInput("yaxis", "y-axis", value="Value"),
                  textInput("xaxis", "x-axis", value="Time")
                ) # tabPanel - Customize graph

                 ### end: customize graph panel ###

              ) # tabsetPanel
            ) # column
          ) # fluidRow
        ), # tabPanel - Graph

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
          h3("R code:"),
          pre(code(p(
"
## Functions ##

# transform the values of x so that the range of x is equal to the range of y
rescale <- function(x,y){
    x.range <- range(x)
    y.range <- range(y)
    x <- ((x - x.range[1])* (diff(y.range))) / diff(x.range) + y.range[1]
    if (any(range(x) != range(y))){
        warning('Ranges do not match.')
    }else{
        return(x)
    }
}

# 6:00 sunrise = 360
# 12:00 noon = 720
# 18:00 sunset = 1080

PAR <- function(days=3,start=0,amp=100){
    amp * sin(2 * pi * rep((1:1440 + 1080 + start),days) * (1/1440))
}

photo <- function(days=3,Amax=4,Aqe=0.3,LCP=0,start=0,amp=50){
    out <- Amax * (1 - exp(-Aqe * (PAR(days,start,amp) - LCP)))
    out[out < LCP] <- 0
    return(out)
}

pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=5, beta=0.001, k=1, Bscaler=10,
                            aMax=10, aMin=1, s=10, d=1, c=100) {

minute <- vector(mode='numeric') # t/time variable
x <- vector(mode='numeric') # amount of o2
a <- vector(mode='numeric') # augmentation function
P <- vector(mode='numeric') # photosynthesis
B <- vector(mode='numeric'') # biological o2 demand
n <- vector(mode='numeric') # amount of nutrients
w <- vector(mode='numeric') # amount of food

if (length(foodWeight) < days){
    foodWeight <- rep(foodWeight,days)[1:days]
}

## Initialization ##

# simulate photosynthesis as fixed values
P <- photo(days)

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
  w <- c(w, w[length(w)]+foodWeight[z])

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
}")))
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
          h3("'plotrix' Package:"),
          p("Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12."),
          h3("'moments' Package:"),
          p("Lukasz Komsta and Frederick Novomestky (2015). moments: Moments, cumulants, skewness, kurtosis and related tests.
              R package version 0.14. http://CRAN.R-project.org/package=moments")
        ) # tabPanel

        ### end: references panel ###

      ) # tabsetPanel
    ) # mainPanel

    ### end: mainpanel

  ) # sidebarLayout
)) # end UI

### end: interface ###


