#' Genoppi GG theme
#' @description Set's up the classic genoppi theme for ggplot2.
#' @author April/Frederik
#' @import ggplot2
#' @export

theme_genoppi <- function(){
  p <- theme_bw() + 
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"),
          aspect.ratio=1)
  p
}

#' @title genoppi themed bar
#' @description genoppi themed bar for shiny
theme_genoppi_bar <- function(){
  p <- theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background=element_blank(),
        plot.title = element_text(size = rel(1)))
  p
}
