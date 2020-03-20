# this will be loaded before ui/server loads.
library(shiny)
library(shinyjs)
library(shinydashboard)
library(hash)


## modules 

plotVolcanoScatterUI <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

plotVolcanoScatter <- function(input, output, session) {
 

  
  
  
}