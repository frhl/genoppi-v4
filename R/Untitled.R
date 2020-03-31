


if (F){
# load immunoprecipitation experiment
library(genoppi)
data("BCL2vsIgG.GPiN")

# look at replicate correlation
BCL2vsIgG.GPiN %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  plot_scatter_basic('rep1','rep3') %>% 
  plot_overlay(as.bait('BCL2')) %>% 
  make_interactive()

# look at basic voclcano plot
BCL2vsIgG.GPiN %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  plot_volcano_basic() %>%
  plot_overlay(as.bait('BCL2')) %>% 
  make_interactive() %>%
  add_layout_html_axes_volcano(width = NULL, height = NULL)

# note 2: bait should always be the top layer. Currently, it doesnt show when overlaying with inweb.
}
  


#%>% make_interactive()
              




