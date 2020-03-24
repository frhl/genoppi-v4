context('read_input')
library(testthat)
library(data.table)

# make artificial data
df1 = data.table(gene=letters, rep1=rnorm(26), rep2=rnorm(26), rep3=rnorm(26))
write.table(df1,"data/test.df1.txt",quote=F,sep="\t",row.names=F)

test_that('read_input can return input data.frame and data format',{
 
  # check returned data.frame 
  result = read_input("data/test.df1.txt",sep="\t",header=T)
  expect_equal(result[[1]],df1)
 
  # check returned data format list
  expected_check = list(gene_rep=T, accession_rep=F, gene_signif=F, accession_signif=F)
  expect_identical(result[[2]],expected_check)
 
})

