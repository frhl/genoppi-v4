#' @title collaps overlay data
#' @description a data.frame used to overlay items in genoppi
#' will look for non-unqiue gene names and combine them into 
#' a single informative line using their alt_label (e.g. snp)
#' and their corrsponding dataset.
#' @param overlay a data.frame
#' @param dataset the main dataset name that is to be combined.
#' @param collapse the column that will contain the collapsed entries.
#' @param collapse_by what identifying colummn is the data to be collapsed by?
#' 
#' 

collapse_labels <- function(overlay, dataset = 'dataset', collapse = 'alt_label', collapse_by = 'gene', dataset_collapse_sep = ': ', item_sep = ' <br>'){
  
  # exepct parameters
  stopifnot(is.data.frame(overlay))
  stopifnot(dataset %in% colnames(overlay))
  stopifnot(collapse_by %in% colnames(overlay))
  if (collapse %nin% colnames(overlay)) overlay[[collapse]] <- ''
  
  # which rows are non-unique?
  drows = unlist(lapply(overlay[[collapse_by]], function(x) sum(overlay[[collapse_by]] == x))) > 1
  
  # combine overlapping overlays into a single lines 
  new = lapply(unique(overlay[drows, ][[collapse_by]]), function(g) {
    z = overlay[overlay[[collapse_by]] %in% g, ]
    z[[collapse]][is.na(z[[collapse]])] <- ''
    z[[collapse]] = paste(apply(z[,c(dataset, collapse)] , 1 , paste , collapse = dataset_collapse_sep), collapse = item_sep)
    # how do we deal with merging colors?
    return(z[1,])
    })
  
  # deal with single labels 
  old = overlay[!drows, ]
  old[[collapse]] = apply(old[,c(dataset, collapse)] , 1 , paste , collapse = dataset_collapse_sep)
  
  # conbine the filtered new overlays with alt labels with old
  combined = as.data.frame(rbind(do.call(rbind, new), old))
  
  return(combined)
}