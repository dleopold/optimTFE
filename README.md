---
title: "README"
author: "Christina Leopold"
date: "2024-04-18"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

**OptimTFE** is a spatial decision-support tool designed to help conservation practitioners identify priority recovery areas for species/taxonomic groups based on known location data, habitat suitability values, and other relevant data inputs. The tool can be used for any dataset that includes suitability values for each species and a target number of priority recovery areas for each.
This tool was designed for spatial visualization, but the optimization algorithm does not require spatial data.
This tool is based on the spatial conservation prioritization tool, Marxan, and our terminology is largely adopted from it (Ball et al., 2009).

(When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```
)
## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

## Suggested citations
Berio Fortini, L., Leopold, C.R., ConBio paper
Data release
Ball, I. R., Possingham, H. P., & Watts, M. E. (2009). Marxan and relatives: Software for spatial conservation prioritization. In A. Moilanen, K. A. Wilson, & H. P. Possingham (Eds.), Spatial conservation prioritization: Quantitative methods and computational tools (pp. 185–195). Oxford University Press.

## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.

## Getting started: optimizing for priority recovery areas

### Software requirements

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

## Input data required

This package requires a minimum of 2 files:
(1) .csv with Planning Units as row names and features (species, taxon group, etc.) as columns. The Planning Unit identifier must occur in the first column for all files. For each column there must be a value 0-1 indicating target suitability for each PU. NA and empty values are acceptable where a Feature does not have representation. 
(2) .csv with Feature Targets, where each row is the Feature name matching that from previous file, and the second column with Targets.

Note: The Planning Unit identifier must occur in the first column for all files. It is renamed to unit_id in the output files.
