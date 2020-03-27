#' @title get protein families




get_protein_families <- function(gene){
  
  # find families and calculate frequency
  family = lapply(prot_fam, function(x) df$gene[df$gene %in% x])
  freq = lapply(families, length)
  family = family[freq > 0]
  family
  
  
}