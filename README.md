
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
devtools::install_github('frhl/genoppi-v4')

```

## Launching shiny application

```R

library(genoppi)
launch_genoppi()

```

## R package usage example

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



