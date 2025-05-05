#include <Rcpp.h>
#include <random>
#include "solution_gen.h"   // declares IntegerMatrix solution_gen(...)
using namespace Rcpp;

//' Generate Solutions and Write to Parquet File
//'
//' This function iterates over solution_ids, calls `solution_gen` (which
//' prepends the solution ids as the first column), stacks all results into one data.frame,
//' and then writes that data.frame to a .parquet file via arrow::write_parquet().
//'
//' @param solution_ids   IntegerVector of solution IDs. One solution will be generated for each ID provided.
//' @param suitability    NumericMatrix input for solution_gen.
//' @param targets        IntegerVector input for solution_gen.
//' @param spp_names      Character vector of species names (one per species column).
//' @param rand_tolerance Integer tolerance for solution_gen.
//' @param output_file    Path to the .parquet file to write (will overwrite).
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
