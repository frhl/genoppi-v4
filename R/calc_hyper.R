#' @title Perform hypergeometric test
#' @description Use one-tailed hypergeometric test to calcualte overlap enrichment between proteomic data and another dataset (e.g. InWeb, gene list, etc.)
#' @param df data.frame containing proteomic data, with gene and significant columns
#' @param listName name of data to overlap 
#' @param listDf data.frame containing data to overlap, with gene and significant columns
#' @param intersectN boolean variable indicating if total population should be intersect of the two datasets
#' @param bait name of bait protein
#' @return list of data.frame and list. Data.frame contains list_name, successInSample_count (x), sample_count (n), notSample_count (N-n), success_count (k), pvalue. List contains genes names corresponding to each group (successInSample_genes, sample_genes, notSample_genes, success_genes).
#' @export

calc_hyper <- function(df, listName, listDf, intersectN, bait=NULL){

  # total population (N) = intersect of all genes in proteomic data  + list data
  if (intersectN==T) { population <- unique(df$gene[df$gene %in% listDf$gene]) }
  # total population (N) = all genes in proteomic data
  else { population <- unique(df$gene) }

  # remove bait if bait is provided
  if (!is.null(bait)) { population <- population[population != bait] }

  # success in population (k) = enriched genes in proteomic data
  success <- unique(df$gene[df$significant & df$gene %in% population])
  # sample (n) = significant genes in list
  sample <- unique(listDf$gene[listDf$significant & listDf$gene %in% population])
  
  # successInSample (x)
  successInSample <- intersect(success,sample)
  # population - sample (N - n)
  notSample <- setdiff(population,sample) 

  # Hypergeometric test (one-tailed)
  hyperP <- phyper(length(successInSample)-1, length(sample),
	length(notSample), length(success), lower.tail=F)

  outDf <- data.frame(list_name=listName, successInSample_count=length(successInSample), 
	sample_count=length(sample), notSample_count=length(notSample), success_count=length(success), pvalue=hyperP)
  outList <- list(successInSample_genes=successInSample, sample_genes=sample, notSample_genes=notSample, success_genes=success)
 
  return(list(outDf,outList))

}
