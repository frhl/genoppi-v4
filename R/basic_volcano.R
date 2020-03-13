#' @title Plot basic volcano
#' @description A function that draw a basic volcano plot.
#' @param df a data.frame with at least columns gene, logFC, pvalue and signifidant.
#' @param bait the gene name of the bait.
#' @param title the title of the plot \code{?ggplot2::labs}.
#' @usage takes in a data.frane with the columns gene, logFC, pvalue and significant 
#' to draw a volcano. Optionally, a column indicating 'color' (string) and 'label' (boolean),
#' can be supplied to indicate the volcano color scheme and whether to draw names of proteins.


basic_volcano <- function(df, bait, title = '',size_point = 3, size_text=3, color_alpha=0.8){
  
  
  require(ggplot2)
  require(ggrepel)

  if (!all(c('gene','logFC', 'pvalue', 'significant') %in% colnames(df))) stop('data.frame does not contain some of logFC, pvalue and signifcant.')
  
  # sum up the 
  n_total <- dim(df)[1]
  n_significant <- sum(df$significant==TRUE)
  
  # no colors specified will result in standard color scheme
  if ('color' %nin% colnames(df)){
    df$color = NA
    df[df$gene %nin% bait,]$color = ifelse(df[df$gene %nin% bait,]$significant, "#41AB5D", "grey")
    df[df$gene %in% bait,]$color = ifelse(df[df$gene %in% bait,]$significant, "red", "orange")
  }
  if ('label' %nin% colnames(df)){
    df$label = FALSE
    df[df$gene %in% bait,]$label = TRUE
  }

  # start volcano plot
  p <- ggplot(df, aes(x = logFC, y = -log10(pvalue))) +
    geom_point(alpha=1, size=size_point, color=df$color, stroke = 0.6) +   
    geom_point(subset(df, gene %in% bait & significant), mapping=aes(x=logFC, y=-log10(pvalue)), size=size_point, color=df[df$gene %in% bait,]$color) +
    geom_point(subset(df, gene %in% bait), mapping=aes(x=logFC, y=-log10(pvalue)), size=size_point, color="black", shape=1) +	
  
    # draw label for all plots if needed
    geom_text_repel(subset(df, df$label == TRUE), mapping=aes(label=gene), arrow=arrow(length=unit(0.1, 'npc')),
                    box.padding=unit(0.15, "lines"), point.padding=unit(0.2, "lines"), color="black", size=size_text) +
    
    # title (with significant count)
    labs(title = title, subtitle = paste(nTotal,'proteins detected.',nSig,'significant.')) + 
    geom_hline(yintercept=0, color="black") + geom_vline(xintercept=0, color="black") +
    xlab(bquote(log[2]*"[fold change]")) + ylab(bquote(-log[10]*"["*italic(.("P"))*"-value]")) + 
    
    # theme
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  # save bait
  p$bait <- bait
  return(p)
}