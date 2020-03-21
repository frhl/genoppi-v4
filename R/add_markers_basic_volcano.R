#' @title add basic hover markers
#' @description A function that takes a ggplot as input and 'plotlyfies' it so
#' that it becomes interactive by adding markers to all basic points.
#' @param p a ggplot
#' @note internal
#' @family shiny

add_markers_basic_volcano <- function(p){
  
  # modify base ggplot from an overlay
  if (!is.null(p$overlay)) p <- modify_ggplot_from_overlay(p)
  
  # plotlify
  p1 <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p1 <- add_markers(p1, data = p$data, x = ~logFC, y = ~-log10(pvalue),
                    marker = list(size = 7, cmin = 0, cmax = 1, color = p$data$color, line = list(width=0.2, color='black')),
                    opacity = 0.9,
                    text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  p1$data <- p$data
  p1$overlay <- p$overlay
  p1
}

#' @title add hover markers
#' @description A function that takes a ggplot as input and 'plotlyfies' it so
#' that it becomes interactive by adding markers to all special plots, e.g. 
#' a genelist or snplist.
#' @param p a ggplot
#' @note internal
#' @family shiny
add_markers_overlay_volcano <- function(p){
  
  # modify base ggplot from an overlay
  if (!is.null(p$overlay)) p <- modify_ggplot_from_overlay(p)
  # modify base ggplot from an overlay
  p1 <- add_markers(p, data = p$overlay, x = ~logFC, y= ~-log10(pvalue),
                      marker = list(color = ifelse(p$overlay$significant, p$overlay$col_significant, p$overlay$col_other),
                                    size = 12, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                      mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                      text = ~paste(gene, sep = "<br>"), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                      name = "SNP")
  p1$data <- p$data
  p1$overlay <- p$overlay
}


#' @title add hover lines
#' @description A function that adds reactive dashed lines to a plotly object.
#' @param p a ggplot
#' @param line_pvalue the pvalue threshold
#' @param line_logfc the logfc threshold 
#' @note internal
#' @family shiny
add_hover_lines_volcano <- function(p, line_pvalue, line_logfc){
  
  stopifnot(!is.null(p$data))
  
  p <- p %>%
  add_lines(x = ~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5), y = ~-log10(line_pvalue), line = list(dash = "dash", width = 0.5, color = "#2b333e"),
            name = '', hoverinfo = "text", text = paste0("pvalue = ", line_pvalue), showlegend = F) %>%
    add_lines(x = line_logfc[1], y = ~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
              name = '', hoverinfo = "text", text = paste0("logFC = ", line_logfc[1]), showlegend = F) %>%
    add_lines(x = line_logfc[2], y = ~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5), line = list(dash = "dash", width = 0.5, color = "#252525"),
              name = '', hoverinfo = "text", text = paste0("logFC = ", line_logfc[2]), showlegend = F)
  p
}

#' @title add reactive dashed lines
#' @description Format plotly axes.
#' @param p a ggplot
#' @note internal
#' @family shiny
add_layout_html_axes_volcano <- function(p){
  
  stopifnot(!is.null(p$data))
  p <- p %>% layout(xaxis = list(title = "log<sub>2</sub>(Fold change)", range=~c(min(p$data$logFC)-0.5, max(p$data$logFC)+0.5)),
         yaxis = list(title = "-log<sub>10</sub>(<i>P</i>-value)", range=~c(min(-log10(p$data$pvalue)-0.5), max(-log10(p$data$pvalue))+0.5)))
  p
}





