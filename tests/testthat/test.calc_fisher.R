context('calc_fisher')
library(testthat)

# read in test data
df <- read.table("data/test.BCL2vsIgG.GPiN.txt",sep="\t",header=T)
statsDf <- calc_mod_ttest(df)
sigDf <- id_enriched_proteins(statsDf)

# InWeb df
inwebDf <- get_inweb_list("BCL2") 

# gene list df
geneInput <- get_gene_list("data/test.ALSgenes.txt")
geneDf <- geneInput[[1]]
intersectN <- geneInput[[2]]

test_that('calc_fisher can return correct overlap results',{

  # InWeb
  result <- calc_fisher(sigDf,"InWeb",inwebDf,T,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.908")
  expect_equal(result[[2]]$overlap_genes,"LARP1")

  # gene list
  result <- calc_fisher(sigDf,"ALS",geneDf,intersectN,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.00513")
  expect_identical(sort(result[[2]]$overlap_genes),c("ATXN2","FUS","PFN1","TAF15"))

})
