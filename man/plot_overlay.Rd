% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_overlay.R
\name{plot_overlay}
\alias{plot_overlay}
\title{plot overlay}
\usage{
plot_overlay(
  p,
  reference,
  match = "gene",
  label = NULL,
  label.size = NULL,
  label.color = "black",
  label.box.padding = 0.3,
  label.point.padding = 0.5,
  label.arrowhead.size = 0.01
)
}
\arguments{
\item{p}{A ggplot object. Usually passed down from \code{plot_volcano_basic} or \code{plot_scatter_basic}.}

\item{reference}{a list of data.frames that are preferably named. The name of the list will passed down to
the data.frame as the column 'dataset'. Alternatively, the dataset can have a column name dataset.}

\item{match}{by what string should the ggplot and overlay be merged? Default is 'gene'.}

\item{label}{A boolean. This will overwrite the \code{label} column in the reference data.frame.}

\item{label.size}{numeric. Size of label. This will overwrite the \code{label.size} column in the reference data.frame.}

\item{label.color}{the color of the label. Default is black.}

\item{label.box.padding}{Amount of padding around bounding box. See \code{?ggrepel::geom_text_repel} 
for more details.}

\item{label.point.padding}{Amount of padding around label. See \code{?ggrepel::geom_text_repel}.}

\item{label.arrowhead.size}{The size of the arrowhead. 0 means no arrowhead.}
}
\value{
a ggplot
}
\description{
Takes a ggplot2 object and overlays points from a reference. 
The function uses the mapping and plot environment from a previous ggplot to add an overlay
with a matching 'reference' data.frame. This yields a new plot with items that intersect the 
the original ggplot data and the reference data. Can be applied iteratively. 
The user can input certain columns in the reference to gain more control over the plotting 
parameters of individual points. See notes for additional details.
}
\note{
The following data.frame headers for a reference are accepted:
\itemize{
 \item{"gene"}{ A string tht indicates the gene name.}
 \item{"label"} {A boolen that indicates whether the label should be plotted.}
 \item{"label_size"}{A numeric that indicates the size of the label items.}
}
See ?validate_reference or additional details.
}
