#' @title Combine data set and significance
#' @description In order to correctly draw plotly points
#' the identifier (dataset) must have a unique colors associated with it.
#' This function appends the dataset column with a significance text. 
#' @param data a data or overlay data.frame with \code{dataset} as column.
#' @param sig_text what should be the significance text?
#' @param insig_text what should be the insignificance text?
#' @export
#' 
combine_dataset_and_significance <- function(data, sig_text = '(enriched)', insig_text = '(not enriched)'){
  stopifnot(sig_text != insig_text)
  stopifnot('dataset' %in% colnames(data))
  data$sigtext = ifelse(data$significant, as.character(sig_text), as.character(insig_text))
  data$dataset = apply(data[,c('dataset','sigtext')], 1, paste, collapse = ' ')
  data$sigtext = NULL
  return(data)
}

