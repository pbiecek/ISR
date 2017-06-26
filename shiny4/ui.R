library("shiny")

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 3, 
           h2("Cummulative ISR"),
           selectInput("typep", "Type", choices = c( "barplot", "points"), "barplot"),
           checkboxInput("outline", "Add outline", TRUE)),
    column(width = 3,
           selectInput("datasets", "Select a dataset", choices = c( "RIS", "MEL", "IMT", "CET", "OML"), "RIS"),
           selectInput("subgroups", "Subgroups", choices = c( "3", "2"), "3")
    ),
    column(width = 3,
           selectInput("maxy", "Max on OY", choices = c( "1", "0.66", "0.5"), "1"),
           selectInput("scalerev", "Select OY scale", choices = c( "only 67%", "only 33%", "fractions", "standard"), "fractions")),
    column(width = 3,
           selectInput("colorscale", "Select color scale", choices = c( "1", "2", "3", "4"), "1"),
           selectInput("typey", "Select type of OY scale", choices = c( "errors", "correct"), "errors"))
  ),
  fluidRow(
    column(width = 9, plotOutput('ggPplot', width = "900px", height = "600px"))
  ),
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}
      .myRow3{height:3px; background-color: #dddddd;}
     "
  ))
)
