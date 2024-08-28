---
title: "optimTFE package introduction"
author: "Christina Leopold, Devin Leopold, Lucas Berio Fortini"
date: "2024-07-15"
output: html_document
---

#### Introduction

**OptimTFE** is a spatial decision-support tool designed to help conservation practitioners
identify priority recovery areas for species/taxonomic groups based on known location data,
habitat suitability values, and other relevant data inputs. The tool can be used for
any dataset that includes suitability values for each species and a target number
of priority recovery areas for each.
This tool was designed for spatial visualization, but the optimization algorithm
does not require spatial data.
This tool is based on the spatial conservation prioritization tool, Marxan, and
our terminology is largely adopted from it (Ball et al., 2009).
This package includes two data visualization tools using RShiny to allow for visualizing tradeoffs
across metrics when evaluating solutions generated using the optimTFE algorithm.


#### Suggested citations
Berio Fortini, L., Leopold, C.R., ConBio paper
Leopold, C.R., Berio Fortini, L., Amidon, F., Fretz, S., Jacobi, J.D., Mehrhoff, L., & Sutter, R. 2023. East Maui, Hawaiʻi optimization of climate resilient habitat for native plant species recovery, 2021: U.S. Geological Survey data release, <https://doi.org/10.5066/P9LKNAR4>.
Ball, I. R., Possingham, H. P., & Watts, M. E. (2009). Marxan and relatives: Software for spatial conservation prioritization. In A. Moilanen, K. A. Wilson, & H. P. Possingham (Eds.), Spatial conservation prioritization: Quantitative methods and computational tools (pp. 185–195). Oxford University Press.

#### Disclaimer

[link to disclaimer](https://code.usgs.gov/pierc/optimTFE/-/blob/main/DISCLAIMER.md?ref_type=heads) 

#### License

[link to license](https://code.usgs.gov/pierc/optimTFE/-/blob/main/LICENSE.md?ref_type=heads)

#### Getting started: optimizing for priority recovery areas

##### Software requirements

optimTFE is an algorithm with associated visualization outputs built using R version 2023.09.1. The application relies on the following R packages (as well as any dependencies). 

Package name      |  Version      
----------------- |--------------
dplyr             | 1.1.4
ggplot2           | 3.5.0
htmltools         | 0.5.8.1
htmlwidgets       | 1.6.2
rmarkdown         | 2.25
leaflet           | 2.2.2
leaflet.providers | 2.0.0
sf                | 1.0-16
tidyverse         | 2.0.0
tidyr             | 1.3.1


To run the application locally, users must first install Rstudio (with R version 2023.09.1) and the required packages. Next, clone or download this repository (without changing the directory structure). The application will then open in the default browser.

#### Input data required

This package requires a minimum of 2 files:
(1) .csv with Planning Units as row names and features (species, taxon group, etc.) as columns. The Planning Unit identifier must occur in the first column for all files. For each column there must be a value 0-1 indicating target suitability for each PU. These data will be used as continuous values; categorical data are not considered. NA and empty values are acceptable where a Feature does not have representation. 
(2) .csv with Feature Targets, where each row is the Feature name matching that from previous file, and the second column with Targets.

Note: The Planning Unit identifier must occur in the first column for all files. It is renamed to unit_id in the output files.

#### Associated spatial data
The optimTFE_leaflet_map and optimTFE_explorer tools allow for spatial data exploration using the algorithm output and associated spatial data. All spatial data must be in .shp or .gpkg format.

#### Vignettes
Vignettes can be viewed in the [Vignettes folder] (https://code.usgs.gov/pierc/optimTFE/-/tree/main/vignettes?ref_type=heads) and
provide documentation for using the optimTFE algorithm and tools for exploring optimTFE
outputs.
