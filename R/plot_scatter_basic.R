#' @title Draw a scatter plot of replicates
#' @description Draws replicate correlation scatter plot(s)
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param bait the name of the bait
#' @param size_point size of point.
#' @param size_text size of the text label.
#' @return a list of ggplot objects and replicate correlations
#' @export
#' @examples 
#' \dontrun{
#' df = data.frame(gene = letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26), significant = T)
#' dat = plot_scatter_basic(df, bait = 'A')
#' dat
#' 
#' df = data.frame(gene = letters, rep1=rnorm(26), rep2=rnorm(26), significant = T)
#' dat = plot_scatter_basic(df, bait = 'A')
#' dat
#'  
#' }


plot_scatter_basic <- function(df, bait=NULL, size_point = 3, col_signficant = "#41AB5D", col_other = 'grey'){
  
  require(ggplot2)
  require(ggrepel)
  
  # inital checks of input
  stopifnot(any(grepl('rep', colnames(df))))
  stopifnot('significant' %in% colnames(df))
  col_rep <- as.vector(grepl('rep', colnames(df)) & unlist(lapply(df, is.numeric)))
  reps = regmatches(colnames(df), regexpr('rep[0-9]',colnames(df)))
  
  # no colors specified will result in standard color scheme
  if ('color' %nin% colnames(df)){
    df$color = NA
    df$color = ifelse(df$significant, col_signficant, col_other)
  }
  
  # store plots in list
  plts = list()
  
  # enumerate all combinations
  combinations = enumerate_replicate_combinations(length(reps))
  for (i in 1:nrow(combinations)){
    repA = reps[combinations$repA[i]]
    repB = reps[combinations$repB[i]]
    name = paste0(repA,'.',repB)
    correlation = cor(df[,repA], df[,repB])
    temp_df = df[,c(repA, repB, 'logFC', 'FDR', 'pvalue', 'significant', 'gene', 'color')]
    
    # add points
    p = ggplot(temp_df, mapping=aes_(x=as.name(repA), y=as.name(repB))) + 
        geom_point(alpha=1, size=size_point, color=ifelse(temp_df$significant, "#41AB5D", "grey"), stroke = 0.6) +
        
        # instead.. use plot_overlay() function
        #geom_point(subset(temp_df, gene %in% bait & significant), mapping=aes_(x=as.name(repA), y=as.name(repB)),size=size_point, color="red") + 
        #geom_point(subset(temp_df, gene %in% bait & !significant), mapping=aes_(x=as.name(repA), y=as.name(repB)),size=size_point, color="orange") +
        #geom_point(subset(temp_df, gene %in% bait), mapping=aes_(x=as.name(repA), y=as.name(repB)), size=size_point, color="black", shape=1) +	
        #geom_text_repel(subset(temp_df, gene %in% bait), mapping=aes(label=gene),
        #              arrow=arrow(length=unit(0.015, 'npc')), box.padding=unit(0.15, "lines"),
        #              point.padding=unit(0.2, "lines"), color="black", size=size_text) +
        
        # add theme and unit line
        geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2) +
        labs(title = paste("r =",format(correlation,digits=3))) + xlab(repA) + ylab(repB) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())
    
    # save plot to list
    plts[[name]] = list(ggplot = p, correlation = correlation)
  }
  
  return(plts)
}






