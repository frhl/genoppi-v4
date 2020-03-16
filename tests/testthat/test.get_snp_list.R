context('get_snp_list')
library(testthat)

# SNP list test file
snpFile <- "data/test.snps.txt" # single column of SNP IDs, no header
genes <- as.factor(c("ATXN2","PFN1","FUS","MATR3","SOD1","PRPH"))
genes_null <- as.factor(c("TEST1","TEST2","TEST3"))

test_that('get_snp_list can return correct data.frame',{

  # SNP list with mapped genes overlapping proteomic data
  result <- get_snp_list(snpFile, genes)
  expect_identical(sort(result$gene),as.factor(c("ATXN2","FUS","MATR3","SOD1","SOD1")))

  # SNP list with no overlap in proteomic data
  result2 <- get_snp_list(snpFile, genes_null)
  expect_true(is.null(result2))

})
