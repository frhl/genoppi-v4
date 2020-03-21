#' @title add hover markers
#' @description A function that takes a ggplot as input and 'plotlyfies' it so
#' that it becomes interactive by adding markers.
#' @param plt a ggplot
#' @note internal
#' @family shiny

add_markers_basic_scatterplot <- function(p, repA, repB){
  
  # modify base ggplot from an overlay
  if (!is.null(p$overlay)) p <- modify_ggplot_from_overlay(p)
  
  # plotlify
  p1 <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p1 <- add_markers(p1, data = p$data, x = ~p$data[[repA]], y = p$data[[repB]],
                    marker = list(size = 7, cmin = 0, cmax = 1, color = p$data$color, line = list(width=0.2, color='black')),
                    opacity = 0.9,
                    text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  p1$data <- p$data
  p1
}


#' @title add reactive dashed lines
#' @description Format plotly axes.
#' @param p a ggplot
#' @note internal
#' @family shiny
add_layout_html_axes_scatterplot <- function(p, repA, repB){
  
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = repA, range=~c((min(p$data[[repA]], p$data[[repB]]))-1, (max(p$data[[repA]], p$data[[repB]]))+1)), 
                    yaxis = list(title = repB, range=~c((min(p$data[[repA]], p$data[[repB]]))-1, (max(p$data[[repA]], p$data[[repB]]))+1)), 
                    title = '', titlefont = list(size=15))
  p
}





