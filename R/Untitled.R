


if (F){
# load immunoprecipitation experiment
library(genoppi)
data("BCL2vsIgG.GPiN")

# look at replicate correlation
BCL2vsIgG.GPiN %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  plot_overlay(as.bait('BCL2')) %>% 
  plot_scatter_basic('rep1','rep2')
  
# look at basic voclcano plot
BCL2vsIgG.GPiN %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  plot_volcano_basic() %>%
  plot_overlay(as.bait('BCL2')) %>% 
  make_interactive(volcano = T)

# plot inweb interactors
ggplot_interactors = ggplot_bait %>%
  plot_overlay(list(inweb=data.frame(get_inweb_list('BCL2')[get_inweb_list('BCL2')$significant,], label = FALSE)), volcano = T) 

  #%>% # this should be simplified
  #make_interactive(volcano = T) %>% 
  #add_layout_html_axes_volcano(height = NULL,width =NULL) # simplified

# plot 
ggplot_interactors = ggplot_bait


# note 1: if volcano has been set to true once, then all downstream volcano should be true
# note 2: bait should always be the top layer. Currently, it doesnt show when overlaying with inweb.
}
  


#%>% make_interactive()
              




