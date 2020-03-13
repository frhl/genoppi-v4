#' @title Plot basic volcano
#' @description A function that draw a basic volcano plot.
#' @param df a data.frame with at least columns gene, logFC, pvalue and significant.
#' @param bait the gene name of the bait.
#' @param col_significant the color of significant proteins/rows.
#' @param col_other the color of non-significnt proteins/rows.
#' @usage takes in a data.frane with the columns gene, logFC, pvalue and significant 
#' to draw a volcano. Optionally, a column indicating 'color' (string) can be supplied to 
#' indicate the volcano color scheme and whether to draw names of specific proteins.
#' @export


plot_volcano <- function(df, bait, col_signficant = "#41AB5D", col_other = 'grey'){
  
  require(ggplot2)
  require(ggrepel)

  if (!all(c('gene','logFC', 'pvalue', 'significant') %in% colnames(df))) stop('data.frame does not contain some of logFC, pvalue and signifcant.')
  
  # no colors specified will result in standard color scheme
  if ('color' %nin% colnames(df)){
    df$color = NA
    df$color = ifelse(df$significant, col_signficant, col_other)
  }
  
  # start volcano plot
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue))) +
    geom_point(alpha=1, size=size_point, color=df$color, stroke = 0.6) +   
    # plot axes labels
    geom_hline(yintercept=0, color="black") + geom_vline(xintercept=0, color="black") +
    xlab(bquote(log[2]*"[fold change]")) + ylab(bquote(-log[10]*"["*italic(.("P"))*"-value]")) + 
    # minimal theme
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  return(p)
}