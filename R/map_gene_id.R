
map_gene_id = function(uniprot){
  require(hashmap)
  hm <- load_hashmap('inst/extdata/uniprotid_to_hgnc')
  return(hm[[uniprot]])
  
}