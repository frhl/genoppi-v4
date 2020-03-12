library(testthat)

# make artificial data
df1 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26))
df2 = matrix()


test_that('input format can describe different datastes',{
  
  # this contains gene and rep
  result = input_format(df1)
  expect_true(result$gene_rep)
  expect_false(result$accession_rep)
  
  # next test ..
  
})

test_that('yields errors',{

  # e.g do not accept matrix
  expect_error(df2)
  
})