#ifndef SOLUTION_GEN_H
#define SOLUTION_GEN_H

#include <Rcpp.h>
using namespace Rcpp;
#include <random>

// Declaration for select_unit function.
std::vector<double> select_unit(NumericMatrix suitability,
                                int rand_tolerance,
                                std::mt19937_64 &gen);

// Declaration for solution_gen function.
std::vector<int> solution_gen(NumericMatrix suitability,
                           IntegerVector spp_targets,
                           IntegerVector unit_regions,
                           IntegerMatrix unit_counts,
                           IntegerMatrix regional_min,
                           IntegerMatrix regional_max,
                           IntegerMatrix populations,
                           IntegerMatrix population_counts,
                           bool single_pu_pop,
                           int rand_tolerance,
                           int max_spp_selected,
                           int solution_id,
                           IntegerMatrix incompat,
                           std::mt19937_64 &gen);

#endif
