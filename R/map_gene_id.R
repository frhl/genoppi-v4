#' @title Map Uniprot accession ID to gene name (HGNC symbol)
#' @description Map and replace accession_number column in a data.frame with gene column. Isoforms (indicated by suffixes separated by . or - in acession_number) are mapped to the same gene names.
#' @param df data.frame containing accession_number column
#' @return list of (df with accession_number column replaced by gene column, df with accession_number and gene columns)
#' @export

map_gene_id <- function(df){
  
  stopifnot('accession_number' %in% colnames(df))
  
  # strip any isoform suffixes (separated by . or -) before mapping
  accession_noIsoform <- sapply(strsplit(as.character(df$accession_number),'(\\-)|(\\.)'),'[',1)
 
  # map accession_number in df to gene (unmapped entries shown as NA)
  matchInds <- match(accession_noIsoform,accession_gene_table$accession_number)
  mapDf <- data.frame(accession_number=df$accession_number,gene=accession_gene_table$gene[matchInds]) 

  # replace accession_number with gene in input df
  df$accession_number <- mapDf$gene
  colnames(df)[colnames(df)=="accession_number"] <- "gene"
 
  return(list(df,mapDf)) 
}
