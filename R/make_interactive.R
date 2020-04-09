#' @title Make ggplot interactive using plotly
#' @description Takes a ggplot and turns it into 
#' an interactive plot.
#' 
#' @param x string. The x-column to be used from \code{p$data} and \code{p$overlay}.
#' @param y string. The y-column to be used from \code{p$data} and \code{p$overlay}.
#' @param volcano boolean. If True, will convert y-axis to \code{y=-log10(y)}.
#' @param legend boolean. Show legend for significant interactors?
#' 
#' @family interactivity
#' @importFrom plotly add_markers add_annotations plot_ly plotly
#' @export

make_interactive <- function(p, x=NULL, y=NULL, source = NULL, legend = T, sig_text = ''){
  
  # set initial paramaters depending on input
  stopifnot(!is.null(p$visual))
  x = ifelse(is.null(x), p$visual$x, x)
  y = ifelse(is.null(y), p$visual$y, y)
  volcano = ifelse(!is.null(p$visual$volcano), p$visual$volcano, F)
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # this following code needs to be reworked and corresponding functions
  # made more easy to understand and use.
  # change the dataset column so that it takes 'significant'
  p$data$dataset = 'Pulldown'
  data = list_to_df(list(A=combine_dataset_and_significance(p$data))) # list_to_df func should be re-worked
  data$size <- 7
  
  overlay = combine_dataset_and_significance(p$overlay, sig_text = sig_text)
  overlay$color = ifelse(overlay$significant, 
                         as.character(overlay$col_significant), 
                         as.character(overlay$col_other))
  
  # rework this..
  data$symbol <- as.character(data$symbol)
  overlay$symbol <- as.character(overlay$symbol)
  
  # get the global symbol and color mapping and save in local environemnt
  global_colors = set_names_by_dataset(data, overlay, marker = 'color') 
  global_symbols = set_names_by_dataset(data, overlay, marker= 'symbol') 
  params = environment()
  
  #print(global_symbols)
  #if (length(global_symbols) > 3) browser()
  
  # add basic plot
  p1 = plot_ly(source = source) %>% 
    add_genoppi_trace(data[data$gene %nin% overlay$gene,], params)
  
  # add overlay
  if (nrow(overlay) > 0){
    
    p1 = p1 %>% 
      add_genoppi_trace(overlay[overlay$significant, ], params, stroke_width = 0.9, legend = legend) %>%
      add_genoppi_trace(overlay[!overlay$significant, ], params, stroke_width = 0.9, legend = F) 
    p1$overlay = p$overlay
  }
  
  # add annotations
  if (nrow(overlay) > 0 & any(overlay$label)){
    
    overlay_label = overlay[overlay$label, ]
    p1 <- add_annotations(p1,
                          x = overlay_label[[x]],
                          y = yf(overlay_label[[y]]),
                          #font = list(size = 14), make font size interactive
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

