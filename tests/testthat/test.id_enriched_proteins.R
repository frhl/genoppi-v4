context('id_enriched_proteins')
library(testthat)

# read in test data
df <- read.table("data/test.BCL2vsIgG.GPiN.txt",sep="\t",header=T)
statsDf <- calc_mod_ttest(df)


test_that('id_enriched_proteins return correct enriched proteins',{

  # a few different test cases
  result <- id_enriched_proteins(statsDf,NA,NA,NA,0.1) # bidirectional, FDR<=0.1
  expect_equal(sum(result),71)  

  result <- id_enriched_proteins(statsDf,"positive",NA,0.05,NA) # logFC>0, p<0.05
  expect_equal(sum(result),56)

  result <- id_enriched_proteins(statsDf,"negative",-5,NA,0.1) # logFC<-5, FDR<=0.1
  expect_equal(sum(result),16)  
 
})
