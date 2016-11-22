library("shiny")
library("rbokeh")
library("htmlwidgets")

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 4, h2("Incurred Sample Reanalysis"), p("Link to the article, contact email")),
    column(width = 4, 
           fileInput('file1', 'Upload an XLSX file with sheet named Arkusz1.',
                     accept=c('application/xlsx', 
                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 
                              '.xlsx'))),
    column(width = 4, 
           p("Other options:"),
           checkboxInput("whiskers", label = "Whiskers", value = TRUE))
  ),
  fluidRow(
    column(width = 4, p("Cummulative ISR"), plotOutput('ggPlotISR')),
    column(width = 4, p("Scatter Number, Difference"), plotOutput('ggPlotDiffNo')),
    column(width = 4, p("Scatter Number, Initial"), plotOutput('ggPlotDiffInit'))
  ),
  fluidRow(
    column(width = 4, p("Histogram of diffs"), plotOutput('ggPlotHist')),
    column(width = 4, p("Distribution of diffs"), plotOutput('ggPlotDiffECDF'))
  ), 
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}"
  ))
)
