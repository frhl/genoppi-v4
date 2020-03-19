#' @title add hover markers
#' @description A function that takes a ggplot as input and 'plotlyfies' it so
#' that it becomes interactive by adding markers.
#' @param plt a ggplot
#' @family shiny

add_markers_basic <- function(p){
  
  p1 <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p1 <- add_markers(p1, data = p$data, x = ~logFC, y = ~-log10(pvalue), color = p$data$color,
                    marker = list(size = 7, cmin = 0, cmax = 1, line = list(width=0.2, color = "blue")),
                    opacity = 0.9,
                    text = ~paste0(all_gene_names, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  p1
}


