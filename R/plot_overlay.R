#' @title plot overlay
#' @description Overlays a ggplot with a reference data set, e.g. a bait or a genelist. Can be applied iteratively.
#' @param p a plot object returned from ggplot.
#' @param reference a list of named data.frames.
#' @param point_expansion percentage expansion of genelist points.
#' @export
#' @examples
#' \dontrun{
#' ## generate a random plot
#' set.seed(3)
#' df = data.frame(gene=letters, fdr=runif(26), pvalue=runif(26), logFC = rnorm(26), significant = c(rep(T,10), rep(F, 16)))
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



plot_overlay <- function(p, reference, point_expansion = 1.1){
  
  # check data of p format of reference
  stopifnot(!is.null(p$data))
  stopifnot(is.list(reference) & !is.data.frame(reference))
  
  # convert list to data.frame
   reference = list_to_df(reference)

  # merge genelist into data.frame and plot
  mymerge = merge(p$data[,c('gene','logFC','pvalue','fdr','significant')], reference, by = 'gene')
  print(mymerge)
  # add the new point
  p1 = p + geom_point(mymerge, 
                 mapping=aes(x=logFC, y=-log10(pvalue)), 
                 size=ifelse('size' %in% colnames(mymerge), mymerge$size, p$plot_env$size_point*point_expansion),
                 #shape = ifelse('shape' %in% colnames(mymerge), mymerge$shape, 21),
                 color=ifelse(mymerge$significant, as.character(mymerge$col_significant), as.character(mymerge$col_other))) +
          geom_point(mymerge[mymerge$stroke, ],
                 mapping=aes(x=logFC, y=-log10(pvalue)),
                 size=ifelse('size' %in% colnames(mymerge), mymerge$size, p$plot_env$size_point*point_expansion),
                 color = 'black',
                 shape = 1) +
          geom_text_repel(mymerge[mymerge$label,], mapping=aes(label=gene), arrow=arrow(length=unit(0.1, 'npc')),
                    box.padding=unit(0.15, "lines"), point.padding=unit(0.2, "lines"), 
                    size=ifelse('label_size' %in% colnames(mymerge), mymerge$labelsize, 3), 
                    color="black")
  
  return(p1)
}


#' @title concert genoppi genelist to data.frame
#' @description converts a named list of datasets to
#' a single data.frame that also contains shape, label and color.
#' @param lst a names reference list of data.frames
#' @return a data.frame
#' @note internal
list_to_df <- function(lst){
  if (is.null(names(lst))) stop('lists must be named!')
  # add columns to each data.frame
  tmp_lst = lapply(1:length(lst), function(i) {
    df = lst[[i]]
    stopifnot(is.data.frame(df))
    cnames = colnames(df)
    if ('dataset' %nin% cnames) df$dataset <- names(lst)[i]
    if ('shape' %nin% cnames) df$shape <- 21
    if ('label' %nin% cnames) df$label <- TRUE
    if ('stroke' %nin% cnames) df$stroke <- TRUE
    if ('col_significant' %nin% cnames) df$col_significant <- 'yellow'
    if ('col_other' %nin% cnames) df$col_other <- 'grey'
    # size
    # labelsize
    return(df)
  })
  return(as.data.frame(do.call(rbind, tmp_lst)))
}










