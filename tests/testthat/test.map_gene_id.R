context('map_gene_id')
library(testthat)
library(hashmap)

# read in test data
df <- data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'), rep1=1:3, rep2=4:6)
outDf <-  data.frame(gene=c("ARHGEF26","TARDBP", "FLT1"), rep1=1:3, rep2=4:6)
mapDf <- data.frame(accession_number=c('Q96DR7', 'Q13148', 'P17948'),
		gene=c("ARHGEF26","TARDBP", "FLT1"))

df2 <- data.frame(accession_number=c('BADNAME'))
df3 <- data.frame(accession_number=c('Q96DR7','Q96DR7-3'))

test_that('map_gene_id.R correctly maps acession_number (uniprot) to HGNC' ,{
  
  result <- map_gene_id(df)
  expect_equal(result[[1]], outDf)
  expect_equal(result[[2]], mapDf) 
  
})

test_that('map_gene_id.R can handle errors, i.e. non-uniprot names' ,{
  
  # invalid uniprot ID
  result <- map_gene_id(df2)
  expect_true(is.na(result[[1]]$gene))
  
  # valid Uniprot ID but with isoform
  expect_warning(map_gene_id(df3))
  
})

