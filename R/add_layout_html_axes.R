#' @title layout for scatterplot
#' @description format axes.
#' @param p a ggplot
#' @note internal
#' @family shiny
add_layout_html_axes_scatterplot <- function(p, repA, repB, title=''){
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = repA, range=~c((min(p$data[[repA]], p$data[[repB]]))-1, (max(p$data[[repA]], p$data[[repB]]))+1)), 
                    yaxis = list(title = repB, range=~c((min(p$data[[repA]], p$data[[repB]]))-1, (max(p$data[[repA]], p$data[[repB]]))+1)), 
                    title = title, titlefont = list(size=15),
                    height = 400, width = 450)
  p
}

#' @title layout for volcano plot.
#' @description Format axes.
#' @param p a ggplot
#' @note internal
#' @family shiny
add_layout_html_axes_volcano <- function(p, height = 400, width = 400){
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = "log<sub>2</sub>(Fold change)", range=~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5)),
                    yaxis = list(title = "-log<sub>10</sub>(<i>P</i>-value)", range=~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5)),
                    height = height, width = width)
  p
}




