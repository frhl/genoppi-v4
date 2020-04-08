#' @title Draw venn diagrams
#' @description drwas a simple 1 by 1 overlap venn diagram.
#' @param x a list of two vectors that contain some overlapping strings.
#' @param colors color scheme.
#' @param main title, typically a p-value.
#' @importFrom VennDiagram venn.diagram
#' @importFrom grid grid.newpage grid.draw
#' @importFrom futile.logger flog.threshold ERROR
#' @export


draw_genoppi_venn <- function(x,main='',colors = c("cornflowerblue", "yellow1")){
  
  # check input
  stopifnot(length(x) == 2)
  stopifnot(length(colors) == 2)
  
  # call package to setuo draw
  v <- VennDiagram::venn.diagram(x,
                            col = colors, margin=0.05, filename = NULL, resolution = 900, height = 400, force.unique = T,
                            main = main,
                            sub = " ", sub.pos = c(0, 0), scaled = F,
                            cat.cex = 1.1, cex = 2, cat.pos = c(180,180), cat.dist = c(0.05,0.05),
                            fontfamily = 'sans', cat.fontfamily = 'sans', main.fontfamily = 'sans')
  # render the plot
  return(v)
}
