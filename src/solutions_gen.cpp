// #include <Rcpp.h>
// #include "solution_gen.h"
// #include <fstream>
// #include <sstream>
// #include <string>
// #include <vector>
// using namespace Rcpp;

// //' Generate Solutions and Write to CSV File
// //'
// //' This function iterates over a vector of solution IDs. For each solution ID,
// //' it calls solution_gen (which now accepts the solution_id and appends it as the first column)
// //' to generate an output matrix. The outputs of all solution_gen calls are written (appended)
// //' to the specified CSV file. If the file does not exist, it will be created.
// //'
// //' @param solution_ids An integer vector of solution IDs.
// //' @param suitability A numeric matrix used as input for solution_gen.
// //' @param targets An integer vector used as input for solution_gen.
// //' @param rand_tolerance An integer tolerance parameter for solution_gen.
// //' @param output_file A string specifying the path to the output CSV file.
// //' @export
// // [[Rcpp::export]]
// void solutions_gen(IntegerVector solution_ids,
//                    NumericMatrix suitability,
//                    IntegerVector targets,
//                    int rand_tolerance,
//                    std::string output_file) {

//   std::vector<IntegerMatrix> results;

//   // Loop over each solution id.
//   for (int i = 0; i < solution_ids.size(); i++) {
//     int sol_id = solution_ids[i];

//     IntegerMatrix sol = solution_gen(suitability, targets, rand_tolerance, sol_id);

//     results.push_back(sol);

//   }

//   int total_rows = 0;
//   int outCols = 0;
//   if (!results.empty()) {
//     for (size_t idx = 0; idx < results.size(); idx++) {
//       total_rows += results[idx].nrow();
//     }
//     outCols = results[0].ncol();
//   }

//   IntegerMatrix combined(total_rows, outCols);
//   int current_row = 0;
//   for (size_t idx = 0; idx < results.size(); idx++) {
//     IntegerMatrix sol = results[idx];
//     int r = sol.nrow();
//     int c = sol.ncol();  // equals outCols
//     for (int i = 0; i < r; i++) {
//       for (int j = 0; j < c; j++) {
//         combined(current_row, j) = sol(i, j);
//       }
//       current_row++;
//     }
//   }

//   // Open the output file in append mode.
//   std::ofstream out(output_file.c_str(), std::ios::app);
//   if (!out.is_open()) {
//     Rcpp::stop("Could not open output file: " + output_file);
//   }

//   // Write the combined matrix to the CSV file.
//   for (int i = 0; i < combined.nrow(); i++) {
//     std::stringstream row_ss;
//     for (int j = 0; j < combined.ncol(); j++) {
//       row_ss << combined(i, j);
//       if (j < combined.ncol() - 1)
//         row_ss << ",";
//     }
//     out << row_ss.str() << "\n";
//   }

//   out.close();
// }
