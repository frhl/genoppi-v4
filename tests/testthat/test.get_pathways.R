context('get_pathways')
library(testthat)

# test data
genes <- c("FAM167A","SUSD1","PRSS21")
hgncPaths <- c("Antisense RNAs","Serine proteases","Sushi domain containing")
mfPaths <- c("serine-type endopeptidase activity","calcium ion binding",
  "protein binding","serine-type peptidase activity")
ccPaths <- c("extracellular region","extracellular space","cytoplasm","plasma membrane",
  "membrane","integral component of membrane","anchored component of membrane")
bpPaths <- c("proteolysis","spermatogenesis")
#msigdbPaths <- () # too many, just check length


test_that('get_pathways can return correct data.frame',{

  # HGNC
  result <- get_pathways("hgnc",genes)
  expect_true(all(result$pathway %in% hgncPaths))

  # MF
  result2 <- get_pathways("mf",genes)
  expect_true(all(result2$pathway %in% mfPaths))

  # CC
  result3 <- get_pathways("cc",genes)
  expect_true(all(result3$pathway %in% ccPaths))

  # BP
  result4 <- get_pathways("bp",genes)
  expect_true(all(result4$pathway %in% bpPaths))

  # MSigDB
  result5 <- get_pathways("msigdb",genes)
  expect_equal(length(result5$pathway),217)

})
