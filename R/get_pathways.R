#' @title Retreive pathway annotations for a list of genes
#' @description Look up pathway annotations for genes using data from HGNC, GO (MF, CC, BP), or MSigDB database.
#' @param database string ("hgnc","mf","cc","bp","msigdb") 
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing gene, pathway, (and GO.ID if appropriate) columns 
#' @export

get_pathways <- function(database, genes){

  pathDf <- NULL

  # HGNC
  if (database=="hgnc") {
    pathDf <- hgnc_group_table[hgnc_group_table$Gene.symbol %in% genes,]
    names(pathDf) <- c("gene","pathway")
 
  # GO MF
  } else if (database=="mf") {
    pathDf <- goa_mf_table[goa_mf_table$Gene.symbol %in% genes,]
    names(pathDf) <- c("gene","GO.ID","pathway")

  # GO CC
  } else if (database=="cc") {
    pathDf <- goa_cc_table[goa_cc_table$Gene.symbol %in% genes,]
    names(pathDf) <- c("gene","GO.ID","pathway")
 
  # GO BP
  } else if (database=="bp") {
    pathDf <- goa_bp_table[goa_bp_table$Gene.symbol %in% genes,]
    names(pathDf) <- c("gene","GO.ID","pathway")

  # MSigDB
  } else if (database=="msigdb") {
    pathDf <- msigdb_table[msigdb_table$Gene.symbol %in% genes,]
    names(pathDf) <- c("gene","pathway")
  }

  return(pathDf)
}
