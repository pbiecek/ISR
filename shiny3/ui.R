library("shiny")
library("rbokeh")

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 4, 
           h2("Incurred Sample Reanalysis"),
           p("Link to the article"), 
           p("Contact email: p.rudzki(at)ifarm.eu"),
           a("Something is not working as expected? Report an issue", href="https://github.com/pbiecek/ISR/issues")),
    column(width = 4,
           fileInput('file1', 'Upload your data in XLSX file',
                     accept=c('.csv', 'application/xlsx',
                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                              '.xlsx')),
           checkboxInput("button",label = "Or use the example data set"),
           a("Download a template input file [XLSX]", href="ISR_template.xlsx"),br(),
           downloadLink('downloadData', 'Download results as the ISR_One_Page_Report')
           ),
    column(width = 2,
           p("Other options:"),
           checkboxInput("whiskers", label = "Whiskers", value = TRUE),
           checkboxInput("logx", label = "Log values", value = FALSE),
           checkboxInput("smooth", label = "Smoothed trend", value = FALSE),
           checkboxInput("bland", label = "Initial instead of mean", value = FALSE)),
    column(width = 2,
           textInput("unit", label = "Units", value = "ng/mL"),
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
    column(width = 4,  plotOutput('ggPlotDiffECDF')),
    column(width = 4,  plotOutput('ggPlotHist'))
  ),
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}
      .myRow3{height:3px; background-color: #dddddd;}
     "
  ))
)
