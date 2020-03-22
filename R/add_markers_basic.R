#' @title Add genoppi markers
#' @description adds plotly markers for a plot thus making the plot interactive.
#' @param p a ggplot.
#' @param x a string corresponding to column name in \code{p$data}. For plotting x axis.
#' @param x a string corresponding to column name in \code{p$data}. For plottin y axis.
#' @param volcano boolean. Will apply -log10 to y-axis, i.e. \code{-log10(p$data$y)}.
#' @export

add_genoppi_markers <- function(p, x='logFC', y='pvalue', volcano = F){

  # function for mapping -log10
  yf <- function(x) if (volcano) return(-log10(x)) else return(x)
  
  # add markers to a ggplot
  if (!is.null(p$overlay)) p <- modify_ggplot_from_overlay(p)
  p1 <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p1 <- add_markers(p1, data = p$data, x = ~(p$data[[x]]), y = ~yf(p$data[[y]]),
                    marker = list(size = 7, cmin = 0, cmax = 1, color = p$data$color, line = list(width=0.2, color='black')),
                    opacity = 0.9, 
                    text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), 
                    hoverinfo = "text", name = "pull down")
  
  # save values in plot for future reference
  p1$overlay = p$overlay
  p1$data = p$data
  p1
}

#' @title Add genoppi markers
#' @description adds plotly markers for a plot thus making the plot interactive. Depends
#' on the data.frame \code{p$overlay} for the plotting.
#' @param p a ggplot or plotly object.
#' @param x a string corresponding to column name in \code{p$overlay}. For plotting x axis.
#' @param x a string corresponding to column name in \code{p$overlay}. For plottin y axis.
#' @param volcano boolean. Will apply -log10 to y-axis, i.e. \code{-log10(p$data$y)}.
#' @export
add_genoppi_markers_overlay <- function(p, x='logFC', y='pvalue', volcano = F){
  
  # function for mapping -log10
  yf <- function(x, volcano = volcano) if (volcano) return(-log10(x)) else return(x)
  
  # add overlay to plot
  if (!is.null(p$overlay)) p <- modify_ggplot_from_overlay(p)
  p1 <- add_markers(p, data = p$overlay, x = ~(p$overlay[[x]]), y= ~yf(p$overlay[[y]]),
                      marker = list(color = ifelse(p$overlay$significant, p$overlay$col_significant, p$overlay$col_other),
                                    size = 8, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                      mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                      text = ~paste(gene, sep = "<br>"), 
                      textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11))

  # save values in plot for future reference  
  p1$overlay = p$overlay
  p1$data = p$data
  p1



}

#' @title search volcano plots
#' @description takes a ggplot and genelist as input 
#' and searches the volcano plot.
#' @param p a ggplot
#' @param genes a vector of genes
#' @note internal
#' @family shiny

add_markers_search <- function(p, genes, x='logFC', y='pvalue', volcano = F){
  
  # function for mapping -log10
  yf <- function(x) if (volcano) return(-log10(x)) else return(x)
  
  # search data
  p$search = p$data[grepl(genes, p$data$gene), ]
  if (nrow(p$search) > 0){
    p <- add_markers(p, data = p$search, x = ~logFC, y = ~yf(pvalue),
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}


make_interactive <- function(p, x='logFC', y='pvalue', volcano = F){
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # deal with non-overlaid items
  data = p$data[p$data$gene %nin% p$overlay$gene, ]
  p1 <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p1 <- add_markers(p1, data = data, 
                    x = ~data[[x]], 
                    y = ~yf(data[[y]]),
                    marker = list(size = 7, cmin = 0, cmax = 1, color = data$color, line = list(width=0.2, color='black')),
                    opacity = 0.9, 
                    text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), 
                    hoverinfo = "text", name = "pull down")
  
  # plot overlayd items
  overlay = p$overlay
  p1 <- add_markers(p1, data = overlay, 
                    x = ~overlay[[x]], 
                    y = ~yf(overlay[[y]]),
                    marker = list(color = ifelse(overlay$significant, overlay$col_significant, overlay$col_other),
                                  size = 8, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                    mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                    text = ~paste(gene, sep = "<br>"), 
                    textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11))
  
  p1$overlay <- p$overlay
  p1$data <- p$data
  p1
}




