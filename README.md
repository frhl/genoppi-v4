
<!-- badges: start -->
[![Build Status](https://travis-ci.com/frhl/genoppi-v4.svg?branch=master)](https://travis-ci.com/frhl/genoppi-v4)
[![Codecov test coverage](https://codecov.io/gh/frhl/genoppi-v4/branch/master/graph/badge.svg)](https://codecov.io/gh/frhl/genoppi-v4?branch=master)
<!-- badges: end -->

# Overview

Genoppi is an open-source software for performing quality control and analyzing quantitative proteomic data. In particular, it streamlines the integration of proteomic data with external datasets such as known protein-protein interactions in published literature, data from genetic studies, gene set annotations, or other user-defined inputs.

This README provides instructions for locally installing the Genoppi software in R (>= 3.6), which consists of two main components: an R package and an interactive shiny application. The application is also available remotely at <http://www.lagelab.org/genoppi/>.

In addition, we provide a [welcome guide](inst/shiny-examples/myapp/www/welcome_guide_200415.pdf) to describe the user interface of the application. The guide also describes the accepted format of various input files; example inputs can be found in the sub-directory *tests/testthat/data*.


## Installation

```R

# download and install Genoppi using the devtools package:
install.packages("devtools")
library(devtools)
devtools::install_github('frhl/genoppi-v4') # UPDATE WHEN UPLOAD TO LAGELAB

```

## Launching shiny application

```R

library(genoppi)
launch_genoppi()

```

## R package usage example

```R

library(genoppi)

# load example proteomic data
data("example_data")


### ------------------------------------------------------------------
### (1) Basic analyses

# perform moderated t-test
stats_df <- calc_mod_ttest(example_data)

# identify enriched proteins
sig_df <- id_enriched_proteins(stats_df)

# generate volcano plot with bait protein labeled
basic_volcano <- plot_volcano_basic(sig_df)
bait_volcano <- plot_overlay(basic_volcano,as.bait('BCL2'))

# generate correlation scatter plot for two replicates
basic_scatter <- plot_scatter_basic(sig_df,"rep1","rep2")
bait_scatter <- plot_overlay(basic_scatter,as.bait('BCL2'))

# output volcano and scatter plots as PDF
pdf("example_basic_plots.pdf",height=7,width=7)
print(bait_volcano)
print(bait_scatter)
dev.off()

# NOTE: the piping (%>%) command can be used to streamline steps, e.g.: 
example_data %>%
  calc_mod_ttest() %>%
  id_enriched_proteins() %>%
  plot_volcano_basic() %>%
  plot_overlay(as.bait('BCL2'))


### NOT WORKING (accession_number error)
# interactive volcano plot
#make_interactive(bait_volcano) %>%
#  add_layout_html_axes_volcano(width = NULL, height = NULL)


### ------------------------------------------------------------------
### (2) Integrated analyses (using InWeb data as example)

# query InWeb interactors for a bait protein (e.g. BCL2)
inweb_df <- data.frame(listName="InWeb",get_inweb_list('BCL2'))

# overlaid volcano plot labeling InWeb interactors
inweb_list <- list(InWeb=inweb_df[inweb_df$significant, ])
plot_overlay(bait_volcano,inweb_list)

# assess overlap b/w enriched proteins and InWeb interactors
overlap_results <- calc_hyper(sig_df, inweb_df,
  data.frame(listName="InWeb",intersectN=T), bait='BCL2')

### NOT WORKING: plot not showing?
# Venn diagram of overlap
#venn_list <- list(Enriched=overlap_results[[2]]$InWeb$success_genes,
#  InWeb=overlap_results[[2]]$InWeb$sample_genes)
#draw_genoppi_venn(venn_list)


### ------------------------------------------------------------------
### (3) Gene set annotations (using HGNC gene groups as example)

# get HGNC gene group annotations
annot_df <- get_pathways('hgnc', sig_df$gene)

### TO DO: which function? just plot_overlay?
# volcano plot with annotations


```

