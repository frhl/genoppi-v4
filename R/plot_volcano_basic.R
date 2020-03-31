#' @title Plot basic volcano
#' @description takes in a data.frane with the columns gene, logFC, pvalue and significant 
#' to draw a volcano. Optionally, a column indicating 'color' (string) can be supplied to 
#' indicate the volcano color scheme and whether to draw names of specific proteins.
#' @param df a data.frame with at least columns gene, logFC, pvalue and significant.
#' @param bait the gene name of the bait.
#' @param col_significant the color of significant proteins/rows.
#' @param col_other the color of non-significnt proteins/rows.
#' @param size_point the size of the points. 
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' # artifical data 
#' df = data.frame(gene=letters, fdr=runif(26), pvalue=runif(26), logFC = rnorm(26), significant = c(rep(T,10), rep(F, 16)))
#' p = plot_volcano(df) + labs(title='Random generated data and gglabs')
#' }
#' 


plot_volcano_basic <- function(df, col_signficant = "#41AB5D", col_other = 'grey', size_point = 3){
  
  # check input
  stop_invalid_columns(df,'plot_volcano_basic',c('gene','logFC', 'pvalue', 'significant'))
  
  # map ping
  df$color <- ifelse(df$significant, col_signficant, col_other)
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue))) +
    geom_point(alpha=1, size=size_point, color=df$color, stroke = 0.6) +   
    geom_hline(yintercept=0, color="black") + 
    geom_vline(xintercept=0, color="black") +
    xlab(bquote(log[2]*"[fold change]")) + 
    ylab(bquote(-log[10]*"["*italic(.("P"))*"-value]")) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p$visual = list(volcano=T, x='logFC', y='pvalue')
  
  return(p)
}
