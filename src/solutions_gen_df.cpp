#include <Rcpp.h>
#include <random>
#include "solution_gen.h"   // declares IntegerMatrix solution_gen(...)
using namespace Rcpp;

//' Generate Multiple Solutions as a Data Frame
//'
//' This function iterates over solution_ids, calls `solution_gen` for each ID,
//' and combines all results into a single data frame. The solution generation
//' process implements a greedy algorithm that iteratively selects planning units
//' with high species richness to satisfy species targets across subregions.
//'
//' @param solution_ids IntegerVector of solution IDs. One solution will be 
//'   generated for each ID provided.
//' @param suitability NumericMatrix of species suitability scores, with planning
//'   units as rows and species as columns.
//' @param spp_targets IntegerVector of target populations for each species.
//' @param unit_regions IntegerVector mapping planning units to their respective
//'   regions.
//' @param unit_counts IntegerMatrix of unit counts by species and region.
//' @param regional_min IntegerMatrix of minimum targets for each species by region.
//' @param regional_max IntegerMatrix of maximum targets for each species by region.
//' @param populations IntegerMatrix specifying known populations for each
//'   species in each planning unit.
//' @param population_counts IntegerMatrix of population counts by species and region.
//' @param single_pu_pop Logical flag indicating whether only one unit should be
//'   selected per known population (default = TRUE).
//' @param rand_tolerance Integer specifying the range of species richness from
//'   maximum to consider for selection at each iteration (default = 5).
//' @param max_spp_selected Integer maximum number of species to select in each
//'   location to reduce 'species packing' within units. Use -1 for no limit.
//' @param spp_names CharacterVector of species names.
//' @param seed Unsigned 64-bit integer seed for random number generation.
//' @param incompat Optional IntegerMatrix specifying species incompatibilities,
//'   where 1 indicates two species cannot share the same planning unit.
//' @return A data frame with columns: solution, order, unit_id, passing, and
//'   one column per species indicating which species targets are satisfied
//'   in each planning unit.
//' @export
// [[Rcpp::export]]
DataFrame solutions_gen_df(IntegerVector solution_ids,
                            NumericMatrix suitability,
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
                            CharacterVector spp_names,
                            uint64_t seed,
                            Nullable<IntegerMatrix> incompat = R_NilValue) {

  // Set random seed
  std::mt19937_64 gen(seed);

  // Consider spp incompatabilities?
  Rcpp::IntegerMatrix incompat_mx(0,0);
  if (incompat.isNotNull()) {
    incompat_mx = incompat.get();
  }

  // Generate solutions and save as ragged array
  std::vector<std::vector<int>> results;
  results.reserve(solution_ids.size());
  for (int id : solution_ids) {
    results.push_back(
      solution_gen(
        suitability,
        spp_targets,
        unit_regions,
        unit_counts,
        regional_min,
        regional_max,
        populations,
        population_counts,
        single_pu_pop,
        rand_tolerance,
        max_spp_selected,
        id,
        incompat_mx,
        gen
      )
    );
  }

  // Compute total rows of results
  size_t ncol = spp_names.size() + 4;
  size_t total_rows = 0;
  for (auto &m : results) {
      total_rows += m.size() / ncol;
  }

  // Convert combined to an R data.frame
  List df(ncol);
  CharacterVector col_names(ncol);
  col_names[0] = "solution";
  col_names[1] = "order";
  col_names[2] = "unit_id";
  col_names[3] = "passing";
  for (size_t j = 4; j < ncol; ++j) {
    col_names[j] = spp_names[j - 4];
  }
  // Unpack ragged array into data.frame
  for (size_t j = 0; j < ncol; ++j) {
    IntegerVector col(total_rows);
    size_t row = 0;
    for (auto &m : results) {
      for(size_t idx = j; idx < m.size(); idx += ncol) {
          col[row++] = m[idx];
      }
    }
    df[j] = col;
  }

  df.attr("names")     = col_names;
  df.attr("class")     = CharacterVector::create("data.frame");
  df.attr("row.names") = IntegerVector::create(NA_INTEGER, -total_rows);

  return(df);

}
