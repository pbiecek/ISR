library("shiny")
library("rbokeh")
library("htmlwidgets")

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 4, h2("Incurred Sample Reanalysis"), p("Link to the article, contact email")),
    column(width = 4,
           checkboxInput("button",label = "Example data"),
           fileInput('file1', 'Or upload your data in XLSX or CSV file',
                     accept=c('.csv', 'application/xlsx',
                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                              '.xlsx')),
           br(),
           downloadLink('downloadData', 'Download the onepager')
           ),
    column(width = 2,
           p("Other options:"),
           checkboxInput("whiskers", label = "Whiskers", value = TRUE),
           checkboxInput("logx", label = "Log values", value = FALSE),
           checkboxInput("smooth", label = "Smoothed trend", value = FALSE),
           checkboxInput("bland", label = "Average instead of Initial", value = FALSE)),
    column(width = 2,
           textInput("unit", label = "Units", value = "[ng/mL]"),
           selectInput("ciband", label = "CI interval", choices = c("none", "50%", "90%"), selected = "50%"),
           checkboxInput("showex", label = "Extreme samples", value = TRUE))
  ),
  fluidRow(
    column(width = 4, plotOutput('ggPlotDiffInit')),
    column(width = 4, plotOutput('ggPlotISR')),
    column(width = 4,  br(),  verbatimTextOutput('textSummary'))
  ),
  fluidRow(class = "myRow3",
           column(width = 12, p(""))
  ),
  fluidRow(
    column(width = 4, plotOutput('ggPlotDiffNo')),
    column(width = 4,  plotOutput('ggPlotHist')),
    column(width = 4,  plotOutput('ggPlotDiffECDF'))
  ),
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}
      .myRow3{height:3px; background-color: #dddddd;}
     "
  ))
)
