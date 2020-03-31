#' @title plot a basic scatte plot 
#' @description Draws a pair of specific replicates in a scatter plot.
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param repA string that is column in df.
#' @param repA string that is column in df.
#' @param size_point size of point.
#' @param size_text size of the text label.
#' @param col_significant color for significant protein interactors.
#' @param col_other color for other protein interactors.
#' @return a gg scatter plot.
#' @import ggplot2
#' @export

plot_scatter_basic <- function(df, repA='rep1', repB='rep2', size_point = 3, col_signficant = "#41AB5D", col_other = 'grey'){
  
  # plot singlebasic scatter plot
  correlation = cor(df[,repA], df[,repB])
  p = ggplot(df, mapping=aes_(x=as.name(repA), y=as.name(repB))) + 
    geom_point(alpha=1, size=size_point, color=ifelse(df$significant, "#41AB5D", "grey"), stroke = 0.6) +
    geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2) +
    labs(title = paste("r =",format(correlation,digits=3))) + xlab(repA) + ylab(repB) +
    theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank())
  
  # set parameters for downstream processing
  p$visual = list(volcano=F, x=repA, y=repB)
  p$correlation = correlation
  return(p)
}


#' @title plot a list of basic scatter plots
#' @description Draw qll pairs of replicates in multiple scatter plot.
#' @param df A data.frame containing at least gene, significant, replicate/triplicate columns.
#' @param repA string that is column in df.
#' @param repA string that is column in df.
#' @param size_point size of point.
#' @param size_text size of the text label.
#' @param col_significant color for significant protein interactors.
#' @param col_other color for other protein interactors.
#' @return a list of gg scatter plots.
#' @import ggplot2
#' @export
plot_scatter_basic_all <- function(df, size_point = 3, col_signficant = "#41AB5D", col_other = 'grey'){
  
  # check input
  expected_columns = c('logFC', 'FDR', 'pvalue', 'significant', 'gene')
  stop_invalid_columns(df, 'plot_scatte_basic_all', expected_columns)
  
  # no colors specified will result in standard color scheme
  df$color = ifelse(df$significant, col_signficant, col_other)
  
  # enumerate all combinations replicate
  reps = regmatches(colnames(df), regexpr('rep[0-9]',colnames(df)))
  combinations = enumerate_replicate_combinations(length(reps))
  plts = lapply(1:nrow(combinations), function(i){
    repA = reps[combinations$repA[i]]
    repB = reps[combinations$repB[i]]
    name = paste0(repA,'.',repB)
    p = plot_scatter_basic(df, repA, repB, size_point, col_signficant, col_other)
    return(list(name = name, ggplot = p, correlation = p$correlation))
  })
  
  # set names
  names(plts) <- unlist(lapply(plts, function(x) x$name))
  return(plts)
}






