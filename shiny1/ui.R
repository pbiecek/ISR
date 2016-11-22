library("shiny")
library("rbokeh")
library("htmlwidgets")

fluidPage(
  fluidRow(
    column(width = 4, h2("Incurred Sample Reanalysis")),
    column(width = 8, 
           fileInput('file1', 'Upload an XLSX file with sheet named Arkusz1.',
                     accept=c('application/xlsx', 
                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 
                              '.xlsx')))
  ),
  fluidRow(
    column(width = 4, p("Scatter Initial,Repeat"), plotOutput('ggPlotDots')),
    column(width = 4, p("Scatter Initial,Repeat-Initial"), plotOutput('ggPlotDots2')),
    column(width = 4, p("Histogram of diffs"), plotOutput('ggPlotHist'))
  ),
  fluidRow(
    column(width = 4, p("Cummulative ISR"), plotOutput('ggPlotISR')),
    column(width = 4, p("Scatter Number, Difference"), plotOutput('ggPlotDiffNo')),
    column(width = 4, p("Scatter Number, Difference"), plotOutput('ggPlotDiffNo2'))
  ),
  fluidRow(
    column(width = 4, p("Distribution of diffs"), plotOutput('ggPlotDiffECDF')),
    column(width = 4, p("Scatter Number, Initial"), plotOutput('ggPlotDiffInit')),
    column(width = 4, p("Scatter Number, Initial"), plotOutput('ggPlotDiffInit2'))
  )
)
