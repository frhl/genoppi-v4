context('get_gwas_list')
library(testthat)

# test GWAS traits
traits <- c("Amyotrophic lateral sclerosis","Amyotrophic lateral sclerosis (sporadic)")
genes <- as.factor(c("SUSD1","LIPC","TEST1","FAM167A","DACH1"))
genes_null <- as.factor(c("TEST1","TEST2","TEST3"))

test_that('get_gwas_list can return correct data.frame',{

  # GWAS SNPs overlap with genes
  result <- get_gwas_list(traits, genes)
  expect_identical(sort(as.character(result[[1]]$gene)),c("DACH1","LIPC","SUSD1"))
  expect_equal(sort(result[[2]]$PUBMEDID),c(17362836,18084291,24529757))

  # GWAS SNPs do not overlap with gense
  result2 <- get_gwas_list(traits, genes_null)
  expect_true(is.null(result2[[1]]))
  expect_true(is.null(result2[[2]]))

})
