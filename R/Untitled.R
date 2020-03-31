


if (F){
# load immunoprecipitation experiment
data("BCL2vsIgG.GPiN")
df = BCL2vsIgG.GPiN


#

#df %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  enumerate_plot_scatter_basic(repB='rep3')

# setup basic pipeline for plotting
df %>% 
  calc_mod_ttest() %>% 
  id_enriched_proteins() %>%
  plot_volcano_basic() %>%
  plot_overlay(as.bait('BCL2'))

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
              




