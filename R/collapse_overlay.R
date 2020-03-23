#' @title collaps overlay data
#' @description a data.frame used to overlay items in genoppi
#' will look for non-unqiue gene names and combine them into 
#' a single informative line using their alt_label (e.g. snp)
#' and their corrsponding dataset.
#' @param overlay a data.frame

collapse_labels <- function(overlay){
  
  stopifnot(is.data.frame(overlay))
  
  # which rows are non-unique?
  drows = unlist(lapply(overlay$gene, function(x) sum(overlay$gene == x))) > 1
  
  # combine overlapping overlays into a single lines 
  new = lapply(unique(overlay[drows, ]$gene), function(g) {
    z = overlay[overlay$gene %in% g, ]
    z$alt_label[is.na(z$alt_label)] <- ''
    z$alt_label = paste(apply(z[,c('dataset','alt_label')] , 1 , paste , collapse = " " ), collapse =' <br> ')
    # how do we deal with colors?
    return(z[1,])
    })
  
  # conbine the filtered new overlays with alt labels with old
  combined = as.data.frame(rbind(do.call(rbind, new), overlay[!drows, ]))
  
  return(combined)
}