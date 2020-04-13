#library(genoppi)



if (F){
  
  # load experimental data (triplicates)
  data("BCL2vsIgG.GPiN")
  
  # basic plotting of bait
  plt = BCL2vsIgG.GPiN %>% 
    calc_mod_ttest() %>% 
    id_enriched_proteins() %>%
    plot_scatter_basic('rep1','rep3') %>% 
    plot_overlay(as.bait('BCL2'))
  
  plt
  
  # plotting of bait and inweb data 
  # but only label LARP1.
  inweb=get_inweb_list('BCL2')
  inweb$label = inweb$gene %in% 'LARP1'
  inweb = list(InWeb=inweb[inweb$significant, ])
  plt = plt %>% plot_overlay(inweb)
  
  plt
  
}

