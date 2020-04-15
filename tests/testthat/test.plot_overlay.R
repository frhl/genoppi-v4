context('plot_overlay')

# read in test data
df <- read_input("data/test.BCL2vsIgG.GPiN.txt",header=T,sep="\t")$data
df <- calc_mod_ttest(df)
#func = 'plot_overlay'

test_that('simple overlay of a bait',{
  
  # a few different test cases
  id = 'A1'
  df <- id_enriched_proteins(df)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p = plot_overlay(p, as.bait('BCL2')) ## testcase here..
  
})


test_that('iterative overlay of ggobjects',{
  
  # a few different test cases
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset
  ref1= data.frame(gene=c('APPL1', 'RAB7A'),col_significant='cyan',col_other='grey', shape = 4)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey')
  reference = list(ref1, ref2)
  names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  p1 = plot_overlay(p1, reference) # testcase here..

})

test_that('iterative overlay of ggobjects with multiple shapes',{
  
  # a few different test cases
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # Generate random dataset

  #ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', shape = 7)
  #reference = list(ref1, ref2)
  #names(reference) = c('genes (I)', 'genes (II)')
  
  # overlay second list
  #plot_overlay(p1, reference)  # testcase here..
  
  
})

test_that('genes that are in multiple lists',{
  
  # a few different test cases
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
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

test_that('invalid columns in overlay gives warning',{
  
  # invalid columns in overlay gives warning
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  ref1= data.frame(gene=c('APC2', 'RAB7A'),col_significant='cyan',col_other='grey', col_invalid = T)
  ref2= data.frame(gene=c('APC2','MAP7','KHSRP'),col_significant='blue',col_other='grey', col_invalid = T)
  reference = list(ref1, ref2)
  names(reference) = c('SCZ genelist', 'ASD genelist')
  expect_warning(plot_overlay(p, reference))
  
})
