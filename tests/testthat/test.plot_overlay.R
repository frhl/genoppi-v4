context('plot_overlay')
library(testthat)

# read in test data
df <- read.table("data/test.BCL2vsIgG.GPiN.txt",sep="\t",header=T)
df <- calc_mod_ttest(df)

test_that('simple overlay of a bait',{
  
  # a few different test cases
  df$significant <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  plot_overlay(p, as.bait('BCL2')) ## testcase here..
  
})


test_that('iterative overlay of ggobjects',{
  
  # a few different test cases
  df$significant <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APPL1', 'RAB7A'),col_significant='cyan',col_other='grey')
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey')
  reference = list(ref1, ref2)
  names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  plot_overlay(p1, reference) # testcase here..
})

test_that('iterative overlay of ggobjects with multiple shapes',{
  
  # a few different test cases
  df$significant <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APPL1', 'RAB7A'),col_significant='cyan',col_other='grey', shape = 4)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', shape = 7)
  reference = list(ref1, ref2)
  names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  plot_overlay(p1, reference)  # testcase here..
  
  
})

test_that('genes that are in multiple lists',{
  
  # a few different test cases
  df$significant <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APC2', 'RAB7A'),col_significant='cyan',col_other='grey', shape = 4)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', shape = 7)
  reference = list(ref1, ref2)
  names(reference) = c('SCZ genelist', 'ASD genelist')
  
  # overlay second list
  plot_overlay(p1, reference)  # testcase here..
  
  
})
