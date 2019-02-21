library(shiny)

# Preprocessing and summarizing data
library(dplyr)

# Visualization development
library(ggplot2)

# For text graphical objects (to add text annotation)
library(grid)

source("viz_temps.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$DublinTemps <- renderPlot({
    viztemps(input$start_year,
             input$hide_hilows,
             input$hide_avgs
    )
  })
})
