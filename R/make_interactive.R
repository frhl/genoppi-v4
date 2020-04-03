#' @title Make ggplot interactive using plotly
#' @description Takes a ggplot and turns it into 
#' an interactive plot.
#' @param x string. The x-column to be used from \code{p$data} and \code{p$overlay}.
#' @param y string. The y-column to be used from \code{p$data} and \code{p$overlay}.
#' @param volcano boolean. If True, will convert y-axis to \code{y=-log10(y)}.
#' @import plotly
#' @export

make_interactive <- function(p, x=NULL, y=NULL, source = NULL){
  
  # set initial paramaters depending on input
  stopifnot(!is.null(p$visual))
  x = ifelse(is.null(x), p$visual$x, x)
  y = ifelse(is.null(y), p$visual$y, y)
  volcano = ifelse(!is.null(p$visual$volcano), p$visual$volcano, F)
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # deal with non-overlaid items
  data = p$data[p$data$gene %nin% p$overlay$gene, ]
  p1 <- plot_ly(showlegend = TRUE, width = 550, height = 550, source = source) # width = height = 550
  p1 <- add_markers(p1, data = data, 
                    x = ~data[[x]], 
                    y = ~yf(data[[y]]),
                    key = data$gene,
                    marker = list(size = 7, cmin = 0, cmax = 1, color = data$color, line = list(width=0.2, color='black')),
                    opacity = 0.9, 
                    text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), 
                    hoverinfo = "text", name = "pull down")
  
  # add dynamic text when hovering over item
  overlay = p$overlay
  if (nrow(overlay) > 0){
    p1 <- add_markers(p1, data = overlay, 
                      x = ~overlay[[x]], 
                      y = ~yf(overlay[[y]]),
                      key = overlay$gene,
                      marker = list(color = ifelse(overlay$significant, 
                                                   as.character(overlay$col_significant), 
                                                   as.character(overlay$col_other)),
                                    symbol = overlay$symbol,
                                    #size = 9,
                                    size = overlay$size, #9, 
                                    line = list(width=1.0, color = "black"), 
                                    opacity = overlay$opacity),
                      mode = "marker+text",
                      hoverinfo = "text", 
                      legendgroup = "group3",
                      #name = apply(overlay[,c('dataset','significant')], 1, paste, collapse = '-'),
                      name = overlay$dataset,
                      text = ~gene, 
                      hovertemplate = ~paste(paste0(bold(gene), ", FDR=", signif(FDR, digits = 3)), ifelse(!is.na(overlay$alt_label), alt_label, dataset), sep = "<br>"),
                      textposition = ~ifelse(logFC>0,"top right","top left"))
                      #textfont = ifelse(overlay$label, list(size = 11), list(size=1)))
    
    # add annotations
    overlay_label <- overlay[overlay$label, ]
    if (nrow(overlay_label) > 0){
      p1 <- add_annotations(p1,
                            x = overlay_label[[x]],
                            y = yf(overlay_label[[y]]),
                            xref = "x",
                            yref = "y",
                            text = overlay_label$gene,
                            xanchor = ifelse(overlay_label$logFC < 0, 'right', 'left'),
                            yanchor = 'bottom',
                            showarrow = F)
    }
    p1$overlay <- p$overlay
  }
  p1$data <- p$data
  p1
}


#' @title search volcano plots
#' @description takes a ggplot and genelist as input 
#' and searches the volcano plot.
#' @param p a ggplot
#' @param genes a vector of genes
#' @note internal
#' @import plotly
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

