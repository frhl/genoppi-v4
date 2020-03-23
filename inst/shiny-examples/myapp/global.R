# this will be loaded before ui/server loads.
# these should be present in NAMESPACE.
library(shiny)
library(shinyjs)
library(shinydashboard)
library(hash)
library(plotly)
library(data.table)
library(ggrepel)

# load genoppi if not attcahed
if (!'genoppi' %in% .packages()) devtools::load_all('~/Toolbox/packages/genoppi/')
# load aprils functions for now
source("~/Projects/04_genoppi/Genoppi-master/functions.R")


## modules // these are Aprils.. for now we load.

#load("data/InWeb_combined_Oct2018.RData")
#load("data/proteinfam_loc_May2019.RData")
#load("data/snp_to_gene.RData")
#human_genome <- read.table("data/ensembl_homo_sapiens_genes.txt", header = T)
#exac <- read.table("data/constrained_cleaned_exac_with_pHI_Aug26.txt", header = T, sep = "\t")
#inweb_combined <- read.table("data/inweb_pooled.txt")


# relative main directory
main = '../../..'
marker_cols <- read.table(file.path(main, 'inst/extdata/colors.txt'))
add_marker_cols <- read.table(file.path(main, 'inst/extdata/colors_markers.txt'))
#up_to_hgnc <- read.table("data/HGNC_gene_to_UniProt_accession_number_Genoppi_ready.csv", header = T, sep = "\t", stringsAsFactors = F)
prot_fam <- read.table(file.path(main, "inst/extdata/protFams_genes_cols.txt"),  sep = "\t", quote = "", na.strings=c("","NA"), header = T, check.names = F)
prot_fam_t <- data.frame(t(prot_fam))


myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("camera"), label)
}