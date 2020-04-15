
<!-- badges: start -->
[![Build Status](https://travis-ci.com/frhl/genoppi-v4.svg?branch=master)](https://travis-ci.com/frhl/genoppi-v4)
[![Codecov test coverage](https://codecov.io/gh/frhl/genoppi-v4/branch/master/graph/badge.svg)](https://codecov.io/gh/frhl/genoppi-v4?branch=master)
<!-- badges: end -->

# Overview

Genoppi is an open-source software for performing quality control and analyzing quantitative proteomic data. Genoppi streamlines the integration of proteomic data with external datasets such as known protein-protein interactions in published literature, data from genetic studies, gene set annotations, or other user-defined inputs. This README provides instructions for locally installing the Genoppi package and shiny application within R.

We also provide a [supplementary protocol](inst/shiny-examples/myapp/www/supplementary_protocol_20200414.pdf) to describe the user interface of the shiny application, which is also available at www.lagelab.org/genoppi.


# Installation

```R

# download and install Genoppi using the devtools package:
install.packages("devtools")
library(devtools)
devtools::install_github('frhl/genoppi-v4')

```

# Usage

```R

# launch shiny application
library(genoppi)
launch_genoppi()

```

# R package usage example

```R

# load immunoprecipitation experiment
# for Antibody to BCL2 and IgG control 
# within neurons
library(genoppi)
data("BCL2vsIgG.GPiN")

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



