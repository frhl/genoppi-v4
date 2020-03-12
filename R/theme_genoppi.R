#' Genoppi GG theme
#' @description Set's up the classic genoppi theme for ggplot2.
#' @author April/Frederik
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


