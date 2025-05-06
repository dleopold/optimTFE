
<!-- README.md is generated from README.Rmd. Please edit that file -->

# optimTFE

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dleopold/optimTFE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dleopold/optimTFE/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

OptimTFE is a spatial decision-support tool designed to help
conservation practitioners identify priority recovery areas for
species/taxonomic groups based on known location data, habitat
suitability values, and other relevant data inputs. The optimTFE tool
was developed to improve transparency, flexibility, and expert
engagement throughout the spatial conservation prioritization
process.optimTFE can be used for any dataset that includes suitability
values for each species and a target number of priority recovery areas
for each. This tool was designed for spatial visualization, but the
optimization algorithm does not require spatial data.

This tool is based on the spatial conservation prioritization tool,
Marxan, and our terminology is largely adopted from it (Ball et al.,
2009). This package includes two data visualization tools using RShiny
to allow for visualizing tradeoffs across metrics when evaluating
solutions generated using the optimTFE algorithm.

The function generates conservation footprints based on species/feature
habitat suitability scores within planning units, and a target number of
populations per species. A greedy algorithm iteratively selects planning
units with the highest number of remaining species targets until all
targets are met. To mitigate known pitfalls of richness-based selection
at each iteration, stochasticity is introduced where one planning unit
is randomly selected from a pool of planning units within a set number
of targets of the maximum for that iteration. To maximize species
suitability scores in selected units the probability of selection is
weighted by the mean suitability scores of remaining targets.
Constraints, such as hybridization, can be introduced to specifically
prohibit the algorithm from selecting the same planning unit for two
taxa. This process is then repeated to generate many spatially efficient
solutions that meet all targets for each species.

## Installation

You can install optimTFE from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dleopold/optimTFE")
```

## Suggested citation

Leopold, C. R., Leopold, D. R., Berio Fortini, L. optimTFE, U.S.
Geological Survey software release, <https://doi.org/10.5066/P137H9PF>.

## References

Berio Fortini, L., Leopold, C. L., Amidon, F., Leopold, D. R., Fretz, J.
S., Jacobi, J. D. Mehrhoff, L., Price, J. P., Duvall, F., Keir, M.,
Oppenheimer, H., Weiseneberger, L., & Sutter, R. *Accepted*. Advancing
Landscape-Scale Conservation Planning with a Transparent, Flexible, and
Expert-Engaged Approach for At-Risk Species Recovery in an Era of Rapid
Ecological Change. Conservation Biology.

Leopold, C.R., Berio Fortini, L., Amidon, F., Fretz, S., Jacobi, J.D.,
Mehrhoff, L., & Sutter, R. 2023. East Maui, Hawaiʻi optimization of
climate resilient habitat for native plant species recovery, 2021: U.S.
Geological Survey data release, <https://doi.org/10.5066/P9LKNAR4>.

Ball, I. R., Possingham, H. P., & Watts, M. E. (2009). Marxan and
relatives: Software for spatial conservation prioritization. In A.
Moilanen, K. A. Wilson, & H. P. Possingham (Eds.), Spatial conservation
prioritization: Quantitative methods and computational tools
(pp. 185–195). Oxford University Press.
