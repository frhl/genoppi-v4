context('collapse_labels')

# read in test data
df <- read_input("data/test.data.txt",header=T,sep="\t")$data
df <- calc_mod_ttest(df)

test_that('get colors by artifical dataset',{

  # setup basic plot
  df <- id_enriched_proteins(df, fdr_cutoff=0.1)
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p1 = plot_overlay(p, as.bait('BCL2'))
  
  # overlay with inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p2 = suppressWarnings(plot_overlay(p1, inweb, label = F))
  expect_true(compare_with_reference(p2, func, id))
  
  
  #collapse_labels(combined, collapse_by = 'gene', collapse_into = 'alt_text', )


})








