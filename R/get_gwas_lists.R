#' @title Retreive GWAS catalog data for a given list of traits
#' @description Create gene list data.frame from input traits
#' @param traits vector of GWAS catalog traits. See \code{gwas_table$DISEASE.TRAIT}
#' @param genes vector of gene names (genes detected in proteomic data)
#' @return list of two data.frames. First data.frame contains trait, gene and SNP columns (restricting to genes in input genes vector). Second data.frame contains GWAS catalog info for the included SNPs.
#' @importFrom tidyr separate_rows
#' @import hash
#' @export

get_gwas_lists <- function(traits, genes){

  # extract all GWAS catalog entries matching the input traits
  gwasDf <- gwas_table[gwas_table$DISEASE.TRAIT %in% traits,]
  gwasDf$SNP <- gwasDf$SNPS

  # expand rows with multiple SNPs (separated by "; " or ", ")
  gwasDf <- tidyr::separate_rows(gwasDf,SNP,sep="; |, ")

  # SNP-to-gene mapping, restricted to gene names in genes vector
  mapDf <- NULL
  for (g in genes) {
    snpInGene <- gwasDf$SNP %in% genes_snps[[g]]

    if (sum(snpInGene) > 0) {
      mapDf <- rbind(mapDf, data.frame(trait=gwasDf$DISEASE.TRAIT[snpInGene],
        gene=g, SNP=gwasDf$SNP[snpInGene]))
    }
  }

  # only keep unique trait-gene-SNP entries
  mapDf <- unique(mapDf)  

  # subset GWAS info table to include only SNPs in mapDf
  gwasRows <- gwasDf$SNP %in% mapDf$SNP
  if (any(gwasRows)) {
    # want collapsed df with multiple SNPs per row
    outGwasDf <- gwasDf[gwasRows,!names(gwasDf)=="SNP"]
    outGwasDf <- unique(outGwasDf) 	 
  } else { outGwasDf <- NULL }

  return(list(mapDf,outGwasDf))

}
