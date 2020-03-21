#' @title Read in SNP list data
#' @description Create gene list data.frame from input SNP list file
#' @param infile a.data.frame or a file path of SNP list file (one column of SNP IDs with no header) 
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return data.frame containing gene and SNP columns (restricting to genes in input genes vector; genes and SNPs can appear multiple times)
#' @export

read_snp_list <- function(infile, genes){
  require(hash)

  # handle data.frames or paths
  if (is.null(dim(infile))) {snpDf <- fread(infile)} else {snpDf <- infile}
  
  mapDf <- NULL
  for (g in genes) {
    if (sum(snpDf$V1 %in% genes_snps[[g]]) > 0) {
      mapDf <- rbind(mapDf,
	data.frame(gene=g, SNP=snpDf$V1[snpDf$V1 %in% genes_snps[[g]]]))
    }
  }
  
  return(mapDf)
}
