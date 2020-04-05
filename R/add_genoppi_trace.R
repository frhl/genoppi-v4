#' @title add genoppi trace
#' @description adding a trace with new data to plotly is non-trivial. This function takes
#' care of adding data correctly with the correct coloring.
#' @param p a plot_ly() object. 
#' @param data a data.frame with columns dataset, gene, 
#' @param parameters a list of parameters or an environement generated with \code{environment()}.
#' @note global variable 'global_colors' must be specified using setNames().
#' @export

add_genoppi_trace <- function(p, data, parameters, size = 7, stroke_width = 0.2, legend = F){
  
  # pass previous environment to function
  x = parameters$x
  y = parameters$y
  global_colors = parameters$global_colors
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # add trace
  p1 <- add_trace(p, data = data, 
                  type = 'scatter',
                  mode = 'markers',
                  x = data[[x]], 
                  y = yf(data[[y]]), 
                  color = ~dataset, 
                  colors = global_colors,
                  key = ~dataset,
                  name = ~dataset,
                  text = ~gene,
                  #symbol = ~symbol,
                  opacity = 0.9,
                  marker = list(size = size,
                                cmin = 0,
                                cmax = 1, 
                                line = list(width=stroke_width, color = "black"), 
                                opacity = 1),
                  hoverinfo = "text", 
                  hovertemplate = ~paste(paste0(bold(gene), ", FDR=", signif(FDR, digits = 3))), #ifelse(!is.na(overlay$alt_label), alt_label, dataset), sep = "<br>"),
                  textposition = ~ifelse(logFC>0,"top right","top left"),
                  showlegend = legend)
  
  return(p1)
}
