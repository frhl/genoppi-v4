context('get_gene_list')
library(testthat)

# ALS genes test files
geneFile <- "data/test.ALSgenes.txt" # file with a single gene column
sigFile <- "data/test.ALSgenes.sigCol.txt" # file with significant column
nonSigFile <- "data/test.ALSgenes.nonSig.txt" # file containing significant=F

test_that('get_gene_list can return correct data.frame',{

  # input file with gene column
  result <- get_gene_list(geneFile,header=T,sep="\t")
  expect_equal(length(result[[1]]$gene),54)
  expect_equal(sum(result[[1]]$significant),54)
  expect_false(result[[2]])

  # input file with gene and significant column
  result2 <- get_gene_list(sigFile)
  expect_identical(result,result2)

  # input file with non-significant genes
  result3 <- get_gene_list(nonSigFile,T,"\t")
  expect_equal(length(result3[[1]]$gene),56)
  expect_equal(sum(result3[[1]]$significant),54)
  expect_true(result3[[2]])

})
