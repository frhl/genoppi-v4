#' @title launch genoppi
#' @description launches genoppi in a seperte shiny application
#' @param local boolean. Launch locally on your machine or try connect to server (to be implemented)
#' @import shiny 
#'
#' @importFrom shinyjs info show html js click disable enable hide reset toggle hidden useShinyjs
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem sidebarMenu tabBox tabItem tabItems
#' @importFrom colourpicker colourInput
#' @importFrom plotly schema ggplotly plotlyOutput renderPlotly plot_ly
#' @export
#' 

launch_genoppi <- function(local=T) {
  appDir <- system.file("shiny-examples", "myapp", package = "genoppi")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `genoppi`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

