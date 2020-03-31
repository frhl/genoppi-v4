#' @title plot overlay
#' @description Overlays a ggplot with a reference data set, e.g. a bait or a genelist. Can be applied iteratively.
#' @param p a plot object returned from ggplot.
#' @param reference a list of named data.frames.
#' @param point_expansion percentage expansion of genelist points.
#' @import ggplot2 ggrepel
#' @export
#' @examples
#' \dontrun{
#' ## generate a random plot
#' set.seed(3)
#' df = data.frame(gene=letters, FDR=runif(26), pvalue=runif(26), logFC = rnorm(26), significant = c(rep(T,10), rep(F, 16)))
#' p = plot_volcano(df) + labs(title='Random generated data and gglabs')
#'
#' ## Generate random dataset
#' ref1= data.frame(gene=c('b'),col_significant='brown',col_other='black')
#' ref2= data.frame(gene=c('d','e','f'),col_significant='blue',col_other='grey')
#' reference = list(ref1, ref2)
#' names(reference) = c('ASD genes', 'SCZ genes')
#' 
#' ## 
#' p1 = plot_overlay(p, reference)
#' p1
#' p2 = plot_overlay(p1, list(bait=data.frame(gene='x', col_significant = 'red', col_other = 'orange')))
#' p2
#' }

plot_overlay <- function(p, reference, x=NULL, y=NULL, point_expansion = 1.05){
  
  # check data of p format of reference
  stopifnot(!is.null(p$data))
  stopifnot(is.list(reference) & !is.data.frame(reference))
  
  # set initial paramaters depending on input
  x = ifelse(is.null(x), p$visual$x, x)
  y = ifelse(is.null(y), p$visual$y, y)
  volcano = ifelse(!is.null(p$visual$volcano), p$visual$volcano, F)
  
  # convert list to data.frame
  reference = validate_reference(list_to_df(reference))
  mymerge = merge(p$data[,unique(c('gene', 'logFC', 'pvalue','FDR','significant', x, y))], reference, by = 'gene')
  
  
  # function for mapping -log10 when volcano = T
  yf <- function(x, v = volcano) if (v) return(-log10(x)) else return(x)
  
  # add the new point
  p1 = p + 
    # add points without stroke
    geom_point(mymerge, 
                mapping=aes_(x=mymerge[[x]], y=yf(mymerge[[y]])), 
                size=ifelse('size' %in% colnames(mymerge), mymerge$size, p$plot_env$size_point*point_expansion),
                #shape = ifelse('shape' %in% colnames(mymerge), mymerge$shape, 21),
                color=ifelse(mymerge$significant, as.character(mymerge$col_significant), as.character(mymerge$col_other))) +
    
    # add stroke
    geom_point(mymerge[mymerge$stroke, ],
               mapping=aes_(x=mymerge[[x]], y=yf(mymerge[[y]])),
               size=ifelse('size' %in% colnames(mymerge), mymerge$size, p$plot_env$size_point*point_expansion),
               color = 'black',
               shape = 1)
  
  # annotate points
  mymerge_labels = mymerge[mymerge$label,]
  p1 =  p1 + geom_text_repel(mymerge_labels, 
                             mapping=aes(label=ifelse(is.na(mymerge_labels$alt_label), 
                                                      as.character(mymerge_labels$gene), 
                                                      paste(as.character(mymerge_labels$gene),
                                                            as.character(mymerge_labels$alt_label), sep = ','))), 
                             arrow=arrow(length=unit(0.1, 'npc')),
                             box.padding=unit(0.15, "lines"), point.padding=unit(0.2, "lines"), 
                             size=ifelse('label_size' %in% colnames(mymerge_labels), mymerge_labels$labelsize, 3), 
                             color="black")
  
  # save overlay and modify plotting data.frame
  if (!is.null(p1$overlay)) {p1$overlay = rbind(p1$overlay, mymerge)} else {p1$overlay = mymerge}
  
  return(p1)

  
}

#' @title concert genoppi genelist to data.frame
#' @description converts a named list of datasets to
#' a single data.frame that also contains shape, label and color.
#' @param lst a names reference list of data.frames
#' @return a data.frame
#' @note internal
list_to_df <- function(lst){
  # check input
  if (is.null(names(lst))) stop('lists must be named!')
  # check that the same columns are present in each data.frame
  expected_cols = unique(unlist(lapply(lst, function(x) colnames(x))))
  invalid_col = lapply(lst, function(x) any(expected_cols %nin% colnames(x)))
  if (any(unlist(invalid_col))) stop('ALL data.frames in list must have SAME column names.')
  # add columns to each data.frame
  tmp_lst = lapply(1:length(lst), function(i) {
    df = lst[[i]]
    stopifnot(is.data.frame(df))
    cnames = colnames(df)
    
    # expected columns
    if ('dataset' %nin% cnames) df$dataset <- names(lst)[i]
    if ('shape' %nin% cnames) df$shape <- 1 # ggplot2 specific
    if ('label' %nin% cnames) df$label <- TRUE
    if ('stroke' %nin% cnames) df$stroke <- TRUE
    if ('col_significant' %nin% cnames) df$col_significant <- 'yellow'
    if ('col_other' %nin% cnames) df$col_other <- 'grey'
    if ('alt_label' %nin% cnames) df$alt_label <- NA
    if ('pLI' %nin% cnames) df$pLI <- NA
    if ('symbol' %nin% cnames) df$symbol <- 'circle' # plotly specific

    return(df)
  })
  return(as.data.frame(do.call(rbind, tmp_lst)))
}



#' @title validate reference data.frame
#' @description A function that checks the column names 
#' of a data.frame to see whether they contain values
#' that can be used by ggplot.
#' @param df a data.frame
#' @param valid a vector of valid ggplot options
#' @return a data.frame
#' @note internal
validate_reference <- function(df, valid = c('gene','col_significant','col_other',
                                             'shape','dataset','stroke','alt_label',
                                            'label','size', 'symbol','pLI')){
  
  bool = colnames(df) %in% valid
  cols = colnames(df)[!bool]
  if (any(!bool)) warning(paste('columns:', paste(cols, collapse=', '),'from reference data.frame are not ggplot compatible and were ignored.'))
  return(df[bool])
}






