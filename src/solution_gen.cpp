#include <Rcpp.h>
#include "solution_gen.h"
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix solution_gen(
                            NumericMatrix suitability,
                            IntegerVector targets,
                            IntegerVector regions,
                            IntegerMatrix regional_min,
                            IntegerMatrix regional_max,
                            IntegerMatrix populations,
                            IntegerMatrix population_counts,
                            bool single_pu_pop,
                            int rand_tolerance,
                            int max_spp_selected,
                            int solution_id = 1) {

  int nUnits = suitability.nrow();
  int nSpp = suitability.ncol();

  NumericMatrix suitability_l = clone(suitability);
  IntegerVector targets_l = clone(targets);

  std::mt19937_64 gen(std::random_device{}());
  std::uniform_real_distribution<double> dist(0.0, 1.0);

  // Pre-calculate the initial sum of total_targets.
  int current_total = 0;
  for (int j = 0; j < targets_l.size(); j++) {
    current_total += targets_l[j];
  }

  // Container to accumulate the outputs from each iteration.
  std::vector< IntegerVector > results;

  // Select units until all targets are met.
  int selected_region = 0;
  while (current_total > 0) {

    IntegerVector selection = select_unit(suitability_l, rand_tolerance);
    selected_region = regions[selection[0]];

    // Set the selected candidate row in suitability_mx to all zeros.
    for (int j = 0; j < nSpp; j++) {
      suitability_l(selection[0], j) = 0.0;
    }

    // Limit by max spp selected
    if (max_spp_selected > 0) {
        // ensure last options for meeting targets are retained
        for (int j = 1; j <= nSpp; j++) {
            if (selection[j] > 0) {
                if (regional_min(j-1, selected_region) == regional_max(j-1, selected_region)) {
                    selection[j] = -1;
                }
                if (populations(selection[0], j-1) > 0) {
                    if (population_counts(j-1, selected_region) == regional_max(j-1, selected_region)) {
                        selection[j] = -1;
                    }
                }
            }
        }
        int required = 0;
        for (int j = 1; j <= nSpp; j++) {
            if (selection[j] == -1) {
                required += 1;
            }
        }
        // easy out if number of required spp is >= max_spp_selected
        if (required >= max_spp_selected) {
          for (int j = 1; j <= nSpp; j++) {
            if (selection[j] != -1) {
              selection[j] = 0;
            }
          }
          goto exit_max_spp;
        }
        // Find spp with highest suitability in the selected unit
        std::vector<int> nz_idx;
        nz_idx.reserve(nSpp);
          for(int i = 1; i <= nSpp; ++i)
            if(selection[i] != 0.0) nz_idx.push_back(i);

        auto it = std::stable_partition(
          nz_idx.begin(), nz_idx.end(),
          [&](int i){ return selection[i] == -1; }
        );

        int m = nz_idx.end() - it;
        std::vector<double> rnd_key(m);
        for(int k = 0; k < m; ++k)
          rnd_key[k] = dist(gen);
        int keep = std::min(max_spp_selected, m);
        std::nth_element(
          it, it + keep, nz_idx.end(),
          [&](int a, int b){
            if(selection[a] != selection[b])
              return selection[a] > selection[b];
            int a_pos = std::find(it, nz_idx.end(), a) - it;
            int b_pos = std::find(it, nz_idx.end(), b) - it;
            return rnd_key[a_pos] > rnd_key[b_pos];
          }
        );

        for(int k = keep; k < m; ++k)
          selection[nz_idx[(it - nz_idx.begin()) + k]] = 0.0;
    }
    exit_max_spp:

    //    decrease the corresponding target by 1.
    for (int j = 1; j < nSpp+1; j++) {
      if (selection[j] > 0) {

        current_total -= 1;
        targets_l[j-1] -= 1;

        // If the new value is 0, set the entire column j in suitability_mx to zero.
        if (targets_l[j] == 0) {
          for (int i = 0; i < nUnits; i++) {
            suitability_l(i, j) = 0.0;
          }
        }

      }
    }

    // Append the output row to the results vector.
    results.push_back(selection);

  }

  int n = results.size();
  IntegerMatrix finalMat(n, nSpp + 3);

  for (int i = 0; i < n; i++) {
    finalMat(i, 0) = solution_id;
    finalMat(i, 1) = i;
    for (int j = 2; j < nSpp + 2; j++) {
      finalMat(i, j) = results[i][j-2];
    }
  }

  return finalMat;
}
