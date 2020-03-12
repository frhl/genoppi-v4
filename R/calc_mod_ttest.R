#' @title Perform moderated t-test
#' @description Use moderated t-test implemented in limma package to calculate logFC, pvalue, and FDR.
#' @param df data.frame containing gene and rep (replicate logFC)  columns
#' @return data.frame containing containing df + logFC, pvalue, and FDR; sorted by decreasing logFC
#' @export

calc_mod_ttest <- function(df){
 
  # moderated t-test
  myfit <- lmFit(subset(df, select=-c(gene)), method="robust")
  myfit <- eBayes(myfit)
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  colnames(modtest)[4:5] <- c("pvalue","FDR")

  # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue, FDR 
  result <- data.frame(cbind(df, modtest[,-c(2,3,6)]))
  result <- result[with(result, order(-logFC, FDR)),]

  return(result)
} 
