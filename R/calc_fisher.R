#' @title Perform Fisher's exact test
#' @description Use one-tailed Fisher's exact test to calcualte overlap enrichment between proteomic data and another dataset (e.g. InWeb, gene list, etc.)
#' @param df data.frame containing proteomic data, with gene and significant columns
#' @param listName name of data to overlap 
#' @param listDf data.frame containing data to overlap, with gene and significant columns
#' @param intersectN boolean variable indicating if total population should be intersect of the two datasets
#' @param bait name of bait protein
#' @return list of data.frame and list. Data.frame contains list_name, overlap_count, dfOnly_count, listOnly_count, neither_count, pvalue. List contains genes names corresponding to each group (overlap_genes, dfOnly_genes, listOnly_genes, neither_genes).
#' @export

calc_fisher <- function(df, listName, listDf, intersectN, bait=NULL){

  # total population = intersect of all genes in proteomic data  + list data
  if (intersectN==T) { population <- unique(df$gene[df$gene %in% listDf$gene]) }
  # total population = all genes in proteomic data
  else { population <- unique(df$gene) }

  # remove bait if bait is provided
  if (!is.null(bait)) { population <- population[population != bait] }

  # enriched proteins/genes in proteomic data
  sigDf <- unique(df$gene[df$significant & df$gene %in% population])
  # genes in list
  sigList <- unique(listDf$gene[listDf$significant & listDf$gene %in% population])

  overlap <- intersect(sigDf,sigList)
  dfOnly <- setdiff(sigDf,sigList)
  listOnly <- setdiff(sigList,sigDf)
  neither <- setdiff(setdiff(population,sigDf),sigList)

  # Fisher's exact test (one-tailed)
  fisherP <- fisher.test(matrix(c(length(overlap),length(dfOnly),
    length(listOnly),length(neither)),nrow=2),alternative="greater")$p

  outDf <- data.frame(list_name=listName, overlap_count=length(overlap), dfOnly_count=length(dfOnly),
	listOnly_count=length(listOnly), neither_count=length(neither), pvalue=fisherP)
  outList <- list(overlap_genes=overlap, dfOnly_genes=dfOnly, listOnly_genes=listOnly, neither_genes=neither)
 
  return(list(outDf,outList))

}
