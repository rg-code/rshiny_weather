library(shiny)

# Define UI for application
shinyUI(fluidPage(

  # Application title
  titlePanel("Merced,CA 2014 Temperatures"),

  # Show a plot of the generated distribution
  plotOutput("MercedTemps"),

  hr(),

  # Sidebar with a slider input for the number of bins
  fluidRow(
    column(5,
      h4("Data Range (Start Year to 2017)"),
      sliderInput('start_year',
                  'Select StartYear',
                  min = 2000,
                  max = 2017,
                  value = 2008),
      offset=1
    ),
    column(5,
      checkboxInput('hide_hilows', 'Hide background daily record highs and lows bars'),
      checkboxInput('hide_avgs', 'Hide background average daily highs and lows bars'),
      offset=1
    )
  )
))
