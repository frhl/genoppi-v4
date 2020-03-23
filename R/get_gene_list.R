#' @title Read in gene list data
#' @description Create gene list data.frame from input file
#' @param filename file path of gene list file (containing gene and (optional) significant columns)
#' @param header boolean indicating whether gene list file contains header
#' @param sep file delimiter
#' @return list of (data.frame containing gene and significant columns, boolean value indicating if non-significant genes are included (for calling overlap enrichement functions)) 
#' @export

get_gene_list <- function(filename, header=T, sep="\t"){
 
  geneDf <- read.table(filename, header, sep)
 
  if (!"significant" %in% colnames(geneDf)) { geneDf$significant <- TRUE }
  intersectN <- sum(geneDf$significant) < nrow(geneDf) # if geneDf contains significant=F entries

  return(list(data=geneDf,intersect=intersectN))
}
