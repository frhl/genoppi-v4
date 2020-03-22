#' @title Retreive GWAS catalog data for a given list of traits
#' @description Create gene list data.frame from input traits
#' @param traits vector of GWAS catalog traits. See \code{gwas_table$DISEASE.TRAIT}
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return list of two data.frames. First data.frame contains gene and SNP columns (restricting to genes in input genes vector; genes and SNPs can appear multiple times). Second data.frame contains GWAS catalog info for the included SNPs.
#' @export

get_gwas_list <- function(traits, genes){
  require(hash)

  # extract all GWAS catalog entries matching the input traits
  gwasDf <- gwas_table[gwas_table$DISEASE.TRAIT %in% traits,]

  # extract list of SNPs (each entry might have > 1 SNP separated by ", " in SNPS column)
  snpList <- strsplit(as.character(gwasDf$SNPS),", ")
  snps <- unlist(snpList)

  # SNP-to-gene mapping, restricted to gene names in genes vector
  mapDf <- NULL
  for (g in genes) {
    if (sum(snps %in% genes_snps[[g]]) > 0) {
      mapDf <- rbind(mapDf,
	data.frame(gene=g, SNP=snps[snps %in% genes_snps[[g]]]))
    }
  }

  # subset GWAS info table to include only SNPs in mapDf
  gwasRows <- unlist(lapply(snpList, function(x) any(x %in% mapDf$SNP))) 
  if (any(gwasRows)) { outGwasDf <- gwasDf[gwasRows,] }
  else { outGwasDf <- NULL}

  return(list(mapDf,outGwasDf))
}
