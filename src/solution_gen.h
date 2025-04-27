#ifndef SOLUTION_GEN_H
#define SOLUTION_GEN_H

#include <Rcpp.h>
using namespace Rcpp;

// Declaration for select_unit function.
IntegerVector select_unit(NumericMatrix suitability,
                          int rand_tolerance);

// Declaration for solution_gen function.
IntegerMatrix solution_gen(NumericMatrix suitability,
                           IntegerVector targets,
                           IntegerVector regions,
                           IntegerMatrix regional_min,
                           IntegerMatrix regional_max,
                           IntegerMatrix populations,
                           IntegerMatrix population_counts,
                           bool single_pu_pop,
                           int rand_tolerance,
                           int max_spp_selected,
                           int solution_id);

#endif
