#' @title map id to gene
#' @description maps an uniprot id to hgnc symbol using a hashmap
#' @param uniprot a vector of uniprot IDs without isoform.
#' @return a vector of corresponding hgnc symbols

map_gene_id = function(uniprot){
  
  # check if data contains isoforms
  if (any(grepl('(\\-)|(\\.)', uniprot))) warning('map_gene_id.R can not handle isoforms, these are included as NAs')
  
  # map data
  require(hashmap)
  hm <- load_hashmap('inst/extdata/uniprotid_to_hgnc')
  return(hm[[uniprot]])
}