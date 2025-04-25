// #include <Rcpp.h>
// #include "solution_gen.h"   // declares IntegerMatrix solution_gen(...)
// using namespace Rcpp;

// //' Generate Solutions and Write to Parquet File
// //'
// //' This function iterates over solution_ids, calls `solution_gen` (which
// //' prepends the ID as the first column), stacks all results into one data.frame,
// //' and then writes that data.frame to a .parquet file via arrow::write_parquet().
// //'
// //' @param solution_ids   IntegerVector of solution IDs. One solution will be generated for each ID provided.
// //' @param suitability    NumericMatrix input for solution_gen.
// //' @param targets        IntegerVector input for solution_gen.
// //' @param spp_names      Character vector of species names (one per species column).
// //' @param rand_tolerance Integer tolerance for solution_gen.
// //' @param output_file    Path to the .parquet file to write (will overwrite).
// //' @export
// // [[Rcpp::export]]
// void solutions_gen_parquet(IntegerVector solution_ids,
//                            NumericMatrix suitability,
//                            IntegerVector targets,
//                            IntegerVector regions,
//                            IntegerMatrix regional_min,
//                            IntegerMatrix regional_max,
//                            IntegerMatrix populations,
//                            IntegerMatrix population_counts,
//                            bool single_pu_pop,
//                            int rand_tolerance,
//                            int max_spp_selected,
//                            CharacterVector spp_names,
//                            std::string output_folder) {
//   std::vector<IntegerMatrix> results;
//   results.reserve(solution_ids.size());
//   for (int i = 0; i < solution_ids.size(); ++i) {
//     int sid = solution_ids[i];
//     results.push_back(
//       solution_gen(
//         suitability,
//         targets,
//         regions,
//         regional_min,
//         regional_max,
//         populations,
//         population_counts,
//         single_pu_pop,
//         rand_tolerance,
//         max_spp_selected,
//         sid)
//     );
//   }

//   // Compute total rows & cols
//   int total_rows = 0;
//   int ncol = 0;
//   if (!results.empty()) {
//     ncol = results[0].ncol();
//     for (auto &m : results) {
//       total_rows += m.nrow();
//     }
//   }

//   // Stack into one big IntegerMatrix
//   IntegerMatrix combined(total_rows, ncol);
//   int row = 0;
//   for (auto &m : results) {
//     int nr = m.nrow();
//     for (int i = 0; i < nr; ++i, ++row) {
//       for (int j = 0; j < ncol; ++j) {
//         combined(row, j) = m(i, j);
//       }
//     }
//   }

//   // Convert combined to an R data.frame
//   List df(ncol);
//   CharacterVector col_names(ncol);
//   col_names[0] = "solution";
//   col_names[1] = "order";
//   col_names[2] = "unit_id";
//   for (int j = 3; j < ncol; ++j) {
//     col_names[j] = spp_names[j - 3];
//   }

//   for (int j = 0; j < ncol; ++j) {
//     IntegerVector col(total_rows);
//     for (int i = 0; i < total_rows; ++i) {
//       col[i] = combined(i, j);
//     }
//     df[j] = col;
//   }
//   df.attr("names")     = col_names;
//   df.attr("class")     = CharacterVector::create("data.frame");
//   df.attr("row.names") = IntegerVector::create(NA_INTEGER, -total_rows);

//   // Write to a parquet file
//   Function tempfile("tempfile");
//   CharacterVector tmp = tempfile(
//     Named("tmpdir")   = output_folder,
//     Named("fileext")  = ".parquet"
//   );
//   std::string out_path = as<std::string>(tmp[0]);

//   Environment arrow_pkg = Environment::namespace_env("arrow");
//   Function write_pq     = arrow_pkg["write_parquet"];
//   write_pq(df, out_path);

// }
