#' @title Map Uniprot accession ID to gene name (HGNC symbol)
#' @description Map and replace accession_number column in a data.frame with gene column. Isoforms (indicated by suffixes separated by . or - in acession_number) are mapped to the same gene names.
#' @param df data.frame containing accession_number column
#' @return list of (df with accession_number column replaced by gene column, df with accession_number and gene columns)
#' @export

map_gene_id <- function(df){
  
  stopifnot('accession_number' %in% colnames(df))
  
  # strip any isoform suffixes (separated by . or -) before mapping
  accession_noIsoform <- sapply(strsplit(as.character(df$accession_number),'(\\-)|(\\.)'),'[',1)
  
  # map data
  require(hashmap)
  dataPath <- system.file("extdata", "uniprotid_to_hgnc", package="genoppi")
  hm <- load_hashmap(dataPath)  
  mappedGenes <- as.factor(hm[[accession_noIsoform]])

  # mapping result
  mapDf <- data.frame(accession_number=df$accession_number,gene=mappedGenes)

  # replace accession_number with gene in input df
  df$accession_number <- mappedGenes
  colnames(df)[colnames(df)=="accession_number"] <- "gene"
 
  return(list(df,mapDf)) 
}
