context('make_interactive')

test_that('basic functionality and overlay',{
  
  df <- read_input("data/test.data.txt",header=T,sep="\t")$data
  df <- suppressWarnings(calc_mod_ttest(df))
  df <- id_enriched_proteins(df)
  
  # overlay with bait
  p = plot_volcano_basic(df) + ggtitle('BCL2 vs IgG in GPiNs') 
  p = plot_overlay(p, as.bait('BCL2'))
  
  # overlay with inweb
  inweb = get_inweb_list('BCL2')
  inweb = list(inweb=inweb[inweb$significant, ])
  p = suppressWarnings(plot_overlay(p, inweb, label = F))
  plt = make_interactive(p)
  
  
  # get paths
  func = 'make_interactive'
  id = 'A1'
  type = 'RDS'
  #paths = make_test_path(func, id, type)
  
  #saveRDS(plt$x$visdat, paths$ref)
  #expect_equal_to_reference(plt$x$visdat, paths$ref)
  
})