#' @title Identify enriched proteins
#' @description Identify enriched proteins based in input thresholds.
#' @param df data.frame contaiing logFC, pvalue, and FDR columns
#' @param logfc_dir string indicating direction of logFC cutoff ("positive" or "negative")
#' @param logfc_cutoff numeric value indicating logFC cutoff
#' @param p_cutoff numeric value indicating pvalue cutoff
#' @param fdr_cutoff numeric value indicating FDR cutoff
#' @return boolean vector indicating which df rows (proteins) are enriched

id_enriched_proteins <- function(df, logfc_dir='positive', fdr_cutoff=0.1, logfc_cutoff=NULL, p_cutoff=NULL){

  sig <- rep(T,nrow(df))

  if (!is.null(logfc_dir)) {
    if (logfc_dir=="positive") sig <- sig & df$logFC > 0
    else if (logfc_dir=="negative") sig <- sig & df$logFC < 0
  }

  if (!is.null(logfc_cutoff)) {
    sig <- sig & abs(df$logFC) > abs(logfc_cutoff)
  }  

  if (!is.null(p_cutoff)) {
    sig <- sig & df$pvalue < p_cutoff
  }

  if (!is.null(fdr_cutoff)) {
    sig <- sig & df$FDR <= fdr_cutoff
  }

  return(sig)
}
