# genoppi-v4 (developer version)
Genoppi version 4. See 'issues' for current development todo's. Run the current shiny app development by loading the package and running:

```R
# load immunoprecipitation experiment
# for Antibody to BCL2 and IgG control 
# within neurons
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


# tally overlap
```

## R-package development

```R
library(devtools)
1. devtools::document()
2. devtools::check()
```

1. devtools::load_all()
2. launch_genoppi()
