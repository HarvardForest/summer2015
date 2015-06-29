### Pitcher Plant model
## By: Nathan Justice
# Last edited: 29June2015

### User Interface ###

# load dependencies
source("global.R", local=TRUE)

shinyUI(fluidPage(
  titlePanel("Pitcher Plant Simulation"),

  sidebarLayout(position="left",
    sidebarPanel(
      sliderInput("days", label=h4("Number of Days:"), value=3, min=1, max=50,
                  step=1),
      uiOutput("days2"),
      sliderInput("feedingTime", label=h4("Feeding Time (in minutes):"),
                  value=720, min=1, max=1440, step=1),
      uiOutput("feedingTime2"),
      sliderInput("foodWeight", label=h4("Food Weight:"), value=5, min=1,
                  max=100),
      uiOutput("foodWeight2"),
      sliderInput("beta", label=h4("Beta Value:"), value=0.0005, min=0.000001,
                  max=0.9, step=0.00001),
      uiOutput("beta2"),
      sliderInput("k", label=h4("k value:"), value=1, min=1, max=10),
      uiOutput("k2"),
      sliderInput("Bscaler",
                   label=h4("Scale Biological Oxygen Deman Values by:"),
                   value=10, min=1, max=100),
      uiOutput("Bscaler2"),
      sliderInput("aMax", label=h4("Maximum value of Augmentation:"), value=10,
                  min=1, max=50),
      uiOutput("aMax2"),
      sliderInput("aMin", label=h4("Minimum value of Augmentation:"), value=1,
                  min=0, max=50),
      uiOutput("aMin2"),
      sliderInput("s", label=h4("s value:"), value=10, min=1, max=50),
      uiOutput("s2"),
      sliderInput("d", label=h4("d value:"), value=0.5, min=0, max=5),
      uiOutput("d2"),
      sliderInput("c", label=h4("c value:"), value=100, min=1, max=1000),
      uiOutput("c2")
    ), # sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(title="Graph",
          fluidRow(
            br(),
            selectInput("plotSelector", "State Variable:",
              choices=c("Oxygen", "Photosynthesis", "Biological Oxygen Demand",
                        "Nutrients", "Augmentation", "Food Amount")
            ),
            plotOutput("mainPlot"),
            column(12, align="center",
              tabsetPanel(
                tabPanel(title="Tipping Point analysis",
                  fluidRow(
                    column(5, align="left",
                      br(),
                      helpText(
                        a("Click here to view the R 'breakpoint' Packagedocumentation.",
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
                tabPanel(title="Early Warning Signal analysis",
                  fluidRow(
                    column(5, align="left",
                      br(),
                      helpText(a("Click here to view the R 'earlywarnings' Package documentation.",
                        href="http://cran.r-project.org/web/packages/earlywarnings/earlywarnings.pdf",
                        target="_blank")
                      ), # helpText
                      helpText(a("Click here to visit the Early Warnings Signals Toolbox website.",
                        href="http://www.early-warning-signals.org/",
                        target="_blank")
                      ), # helpText
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
                    ), # tabPanel - Early Warning Signal analysis
                    tabPanel(title="Customize graph",
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
                downloadButton('downloadMainTable', 'Download Table'),
                br(),
                br(),
                dataTableOutput("mainTable")
              ) # fluidRow
            ), # tabPanel - Data Table
            tabPanel(title="Model",
              h1("Pass")
            ), # tabPanel - Model
            tabPanel(title="References",
              h1("Pass")
            ) # tabPanel
          ) # tabsetPanel
        ) # mainPanel
      ) # sidebarLayout
    ))

