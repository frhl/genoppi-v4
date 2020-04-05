#' @title Make ggplot interactive using plotly
#' @description Takes a ggplot and turns it into 
#' an interactive plot.
#' @param x string. The x-column to be used from \code{p$data} and \code{p$overlay}.
#' @param y string. The y-column to be used from \code{p$data} and \code{p$overlay}.
#' @param volcano boolean. If True, will convert y-axis to \code{y=-log10(y)}.
#' @param legend boolean. Show legend for significant interactors?
#' @import plotly
#' @export

make_interactive <- function(p, x=NULL, y=NULL, source = NULL, legend = T){
  
  # set initial paramaters depending on input
  stopifnot(!is.null(p$visual))
  x = ifelse(is.null(x), p$visual$x, x)
  y = ifelse(is.null(y), p$visual$y, y)
  volcano = ifelse(!is.null(p$visual$volcano), p$visual$volcano, F)
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # change the dataset column so that it takes 'significant'
  p$data$dataset = 'Pulldown'
  data = list_to_df(list(A=combine_dataset_and_significance(p$data))) # list_to_df func should be re-worked
  overlay = combine_dataset_and_significance(p$overlay)
  overlay$color = ifelse(overlay$significant, 
                         as.character(overlay$col_significant), 
                         as.character(overlay$col_other))
  
  # rework this..
  data$symbol <- as.character(data$symbol)
  overlay$symbol <- as.character(overlay$symbol)
  
  global_colors = get_global_colors(data, overlay) 
  params = environment()
  
  # add basic plot
  p1 = plot_ly(source = source) %>% 
    add_genoppi_trace(data[data$gene %nin% overlay$gene,], params)
  
  # add overlay
  if (nrow(overlay) > 0){
    
    p1 = p1 %>% 
      add_genoppi_trace(overlay[overlay$significant, ], params, size = 9, stroke_width = 0.9, legend = legend) %>%
      add_genoppi_trace(overlay[!overlay$significant, ], params, size = 9, stroke_width = 0.9, legend = F) 
    p1$overlay = p$overlay
  }
  
  # add annotations
  if (nrow(overlay) > 0 & any(overlay$label)){
    overlay_label = overlay[overlay$label, ]
    p1 <- add_annotations(p1,
                          x = overlay_label[[x]],
                          y = yf(overlay_label[[y]]),
                          color = 'black',
                          xref = "x",
                          yref = "y",
                          text = overlay_label$gene,
                          xanchor = ifelse(overlay_label$logFC < 0, 'right', 'left'),
                          yanchor = 'bottom',
                          showarrow = F)
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

#' @title Combine data set and significance
#' @description In order to correctly draw plotly points
#' the identifier (dataset) must have a unique colors associated with it.
#' This function appends the dataset column with a significance text. 
#' @param data a data or overlay data.frame with \code{dataset} as column.
#' @param sig_text what should be the significance text?
#' @param insig_text what should be the insignificance text?
#' @export
combine_dataset_and_significance <- function(data, sig_text = '', insig_text = '(N)'){
  stopifnot(sig_text != insig_text)
  stopifnot('dataset' %in% colnames(data))
  data$sigtext = ifelse(data$significant, as.character(sig_text), as.character(insig_text))
  data$dataset = apply(data[,c('dataset','sigtext')], 1, paste, collapse = ' ')
  data$sigtext = NULL
  return(data)
}


#' @title get global colors
#' @description the colors supplied to plotly must be in a global colors
#' scheme with appropiate names (similar to a dict). This function generated
#' this mapping, so that the color scheme can be correctly plotted.
#' @param data a data.frame object.
#' @param overlay a data.frame object.
#' @export
get_global_colors <- function(data, overlay){
  
  # check
  stopifnot(all(c('color', 'dataset') %in% colnames(data)))
  stopifnot(all(c('color', 'dataset') %in% colnames(overlay)))
  
  # get pairs of colors
  tabl_data = data[,c('dataset','color')]
  tabl_data = as.data.frame(tabl_data[!duplicated(tabl_data),])
  tabl_overlay = overlay[,c('dataset','color')]
  tabl_overlay = as.data.frame(tabl_overlay[!duplicated(tabl_overlay),])
  tabl = rbind(tabl_data, tabl_overlay)
  
  # set colors
  global_colors <- setNames(tabl$color, tabl$dataset)
  
  return(global_colors)
  
}

