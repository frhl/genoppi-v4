#' @title add a regression line
#' @description adds a regression line to plotly object
#' @param p a plotly object
#' @param data the data object used to draw the plotly object. By
#' default, the function will look for the data object within the
#' plotly object.
#' @param x for linear model, the explanatory variable.
#' @param y for linear model, the response variable.
#' @param color color of line
#' @param width width of line
#' @param dash dashed or solid?
#' @param opacity opacity of line.
#' @export

add_line_lm <- function(p, data=NULL, x, y, color = 'black', width = 1, dash = 'dash', opacity = 0.9){
  
  
  if (is.null(data)) data = p$data
  stopifnot(!is.null(data))
  stopifnot(x %in% colnames(data))
  stopifnot(y %in% colnames(data))
  fit = lm(data[[y]] ~ data[[x]])
  p1 = add_trace(p, x = data[[x]], y = fitted(fit), 
                 type = 'scatter', 
                 mode = 'lines',
                 opacity = opacity,
                 line = list(color=color, width=width, dash=dash), showlegend=F)
  
  return(p1)
}
