#' @title add genoppi trace
#' @description adding a trace with new data to plotly is non-trivial. This function takes
#' care of adding data correctly with the correct coloring.
#' @param p a plot_ly() object. 
#' @param data a data.frame with columns dataset, gene, 
#' @param parameters a list of parameters or an environement generated with \code{environment()}.
#' @note global variable 'global_colors' must be specified using setNames().
#' @importFrom plotly add_trace
#' @export

add_genoppi_trace <- function(p, data, parameters, stroke_width = 0.2, legend = F, legend_group = ''){
  
  # pass previous environment to function
  x = parameters$x
  y = parameters$y
  volcano = parameters$volcano
  global_colors = parameters$global_colors
  global_symbols = parameters$global_symbols

  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)

  #
  #if ('InWeb (enriched)' %in% data$dataset) browser()
  
  # set legend order in trace
  if (!any(is.na(data$legend_order))) data$dataset = factor(data$dataset, levels = unique(data$dataset[data$legend_order]))
  
  #if (!volcano) browser()
  
  # make trace
  p1 <- add_trace(p, data = data, 
                  type = 'scatter',
                  mode = 'markers',
                  x = data[[x]], 
                  y = yf(data[[y]]), 
                  color = ~dataset, 
                  colors = global_colors,
                  symbol = ~dataset, 
                  symbols = global_symbols,
                  #size = ~size,
                  sort = F,
                  key = ~gene,
                  name = ~dataset,
                  text = ~gene,
                  opacity = 0.9,
                  marker = list(cmin = 0,
                                cmax = 1, 
                                line = list(width=stroke_width, color = "black"), 
                                size = data$size,
                                opacity = data$opacity),
                  hoverinfo = "text", 
                  hovertemplate = ~paste(paste0(bold(gene), ", FDR=", signif(FDR, digits = 3),'<br>',ifelse(!is.na(data$alt_label), alt_label, dataset), sep = "<br>")),
                  textposition = ~ifelse(logFC>0,"top right","top left"),
                  labels = ~gene,
                  legendgroup = legend_group,
                  showlegend = legend)
  
  return(p1)
}
