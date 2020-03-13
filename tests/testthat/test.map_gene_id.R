context('map_gene_id')
library(testthat)

# read in test data
df = data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'), rep1 = runif(3), rep2=rnorm(3))


test_that('map_gene_id.R correctly maps acession_id (uniprot) to HGNC' ,{
  
  # can map a vector
  df$gene = map_gene_id(df$accession_number)
  expect_equal(df$gene, c("ARHGEF26","TARDBP", "FLT1"))
  
  # can map a string
  expect_equal(map_gene_id('Q13618'), 'CUL3')
  
})

test_that('map_gene_id.R can handle errors, i.e. non-uniprot names' ,{
  
  # Not a valid uniprot ID
  expect_true(is.na(map_gene_id('BLABLA')))
  
  # Valid Uniprot ID but with isoform
  expect_warning(map_gene_id(c('Q96DR7','Q96DR7-3')))
  
})




