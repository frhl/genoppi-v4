#' @title map id to gene
#' @description maps an uniprot id to hgnc symbol using a hashmap
#' @param df data.frame containing accession_number column
#' @return list of (df with accession_number column replaced by gene column, df with accession_number and gene columns)
#' @export

map_gene_id <- function(df){
  
  # check if data contains isoforms
  if (any(grepl('(\\-)|(\\.)', df$accession_number))) warning('map_gene_id.R can not handle isoforms, these are included as NAs')
  
  # map data
  require(hashmap)
  dataPath <- system.file("extdata", "uniprotid_to_hgnc", package="genoppi")
  hm <- load_hashmap(dataPath)  
  mappedGenes <- as.factor(hm[[df$accession_number]])

  # mapping result
  mapDf <- data.frame(accession_number=df$accession_number,gene=mappedGenes)

  # replace accession_number with gene in input df
  df$accession_number <- mappedGenes
  colnames(df)[colnames(df)=="accession_number"] <- "gene"
 
  return(list(df,mapDf)) 
}
