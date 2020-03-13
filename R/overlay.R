#' @title plot_overlay
#' @description Overlays a ggplot with a reference data.set, e.g. a bait or a genelist


ref1= data.frame(id = 'basic', gene=c('b'),col_significant='red',col_other='orange',shape=NA)
ref2= data.frame(id = 'basic', gene=c('b'),col_significant='red',col_other='orange',shape=NA)
reference = list(ref1, ref2)


plot_overlay <- function(p, reference){
  
  # check data of p format of reference
  stopifnot(!is.null(p$data))
  
  # merge list of reference data.frames
  if (is.list(reference)){
    if (all(!is.null(names(reference)))) lapply(1:length(reference), function(x) reference[x]$id = names(reference)[x])
    
    
    do.call(rbind, lapply(reference, format_reference))
  }
    
  
}



#' @title format to standard reference
#' @description formats a reference data.frame to contain
#' all needed collumns so that they can be merged in the future.
#' @param ref a reference.data.frame or list of data.frames
standard_reference <- function(ref){
  cnames = colnames(ref)
  if ('id' %nin% cnames) ref$id = base::sample(1:10e6, 1)
  if ('shape' %nin% cnames) ref$shape = 20
  if ('label' %nin% cnames) ref$label = TRUE
  return(ref)
}
