context('calc_hyper')

# read in test data
df <- read.table("data/test.BCL2vsIgG.GPiN.txt",sep="\t",header=T)
statsDf <- calc_mod_ttest(df)
sigDf <- id_enriched_proteins(statsDf)

# InWeb df
inwebDf <- data.frame(listName="InWeb", get_inweb_list("BCL2"))
inwebInterDf <- data.frame(listName="InWeb",intersectN=T)

# gene list df
geneInput <- get_gene_lists("data/test.ALSgenes.txt")
geneDf <- geneInput[[1]]
intersectDf <- geneInput[[2]]

test_that('calc_hyper can return correct overlap results',{

  # InWeb
  result <- calc_hyper(sigDf,inwebDf,inwebInterDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.908")
  expect_equal(result[[2]][["InWeb"]]$successInSample_genes,"LARP1")

  # gene list
  result <- calc_hyper(sigDf,geneDf,intersectDf,"BCL2")
  expect_equal(format(result[[1]]$pvalue,digits=3),"0.00513")
  expect_identical(sort(result[[2]][["ALS"]]$successInSample_genes),c("ATXN2","FUS","PFN1","TAF15"))

})
