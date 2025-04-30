#include <Rcpp.h>
#include "solution_gen.h"
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix solution_gen(
                            NumericMatrix suitability,
                            IntegerVector spp_targets,
                            IntegerVector unit_regions,
                            IntegerMatrix unit_counts,
                            IntegerMatrix regional_min,
                            IntegerMatrix regional_max,
                            CharacterMatrix populations,
                            IntegerMatrix population_counts,
                            bool single_pu_pop,
                            int rand_tolerance,
                            int max_spp_selected,
                            int solution_id = 1) {

  int nUnits = suitability.nrow();
  int nSpp = suitability.ncol();
  int nRegions = unit_counts.ncol();

  // Create local copies of inputs that will be modified
  NumericMatrix suitability_l = clone(suitability);
  IntegerVector spp_targets_l = clone(spp_targets);
  IntegerMatrix unit_counts_l = clone(unit_counts);
  IntegerMatrix regional_min_l = clone(regional_min);
  IntegerMatrix regional_max_l = clone(regional_max);
  IntegerMatrix population_counts_l = clone(population_counts);

  std::mt19937_64 gen(std::random_device{}());
  std::uniform_real_distribution<double> dist(0.0, 1.0);

  // Pre-calculate the sum of total spp targets.
  int current_total = 0;
  for (int j = 0; j < spp_targets_l.size(); j++) {
    current_total += spp_targets_l[j];
  }

  // Total sub-region targets for each species
  IntegerVector total_sub_targets(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      total_sub_targets[j] += regional_min_l(j, r);
    }
  }

  // Sum of total available units for each species
  IntegerVector spp_units(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      spp_units[j] += unit_counts_l(j, r);
    }
  }

  // Sum of total populations for each species
  IntegerVector spp_populations(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      spp_populations[j] += population_counts_l(j, r);
    }
  }

  // Container to accumulate the outputs from each iteration.
  std::vector< IntegerVector > results;

  // Select units until all targets are met.
  while (current_total > 0) {

    IntegerVector selection = select_unit(suitability_l, rand_tolerance);
    if(selection.size() == 0){
      break;
    }
    int selected_region = unit_regions[selection[0]] - 1; // Region index of selected unit (convert to 0-indexed)

    // Find total species with any remaining potential targets in the selected unit
    // along with the species that must be included for meeting targets (ie, can not
    // be excluded by max_spp_selected, or incompatibilities)
    IntegerVector required_spp(nSpp, 0);
    int n_spp_selected = 0;
    int n_required = 0;
    for (int j = 0; j < nSpp; j++) {
        if (selection[j+1] > 0) {
            n_spp_selected += 1;
            if (regional_min_l(j, selected_region) >= unit_counts_l(j, selected_region)) {
                required_spp[j] = 1;
                n_required += 1;
            }
            if(spp_targets_l[j] >= spp_units[j]) {
                required_spp[j] = 1;
                n_required += 1;
            }
            if(populations(selection[0], j) != NA_STRING) {
                required_spp[j] = 1;
                n_required += 1;
            }
        }
    }

    // TODO filter incomatabilities

    // Limit by max spp selected
    if (max_spp_selected > 0 && n_spp_selected > max_spp_selected) {

        // easy out if number of required spp is >= max_spp_selected
        if (n_required >= max_spp_selected) {

          for (int j = 0; j < nSpp; j++) {
            if ( selection[j+1] > 0 && required_spp[j] == 0) {
              selection[j+1] = 0;
              unit_counts_l(j, selected_region) -= 1;
              spp_units[j] -= 1;
            }
          }
          goto exit_max_spp; // exit the max_spp_selected loop
        }

        // Find spp with highest suitability in the selected unit
        std::vector<int> nz_idx;
        nz_idx.reserve(nSpp);
        for(int j = 0; j < nSpp; ++j){
            if(selection[j+1] != 0.0) nz_idx.push_back(j);
        }

        // Partition into required and non-required spp
        auto it = std::stable_partition(
          nz_idx.begin(), nz_idx.end(),
          [&](int j){ return required_spp[j] == 1; }
        );

        // For optional taxa keep the ones with highest suitability (randomly break ties)
        std::vector<double> rnd_key(nSpp+1, 0.0);
        for (int idx : nz_idx) {
            // only optional ones get a random key
            if ( required_spp[idx] == 0 ) {
                rnd_key[idx] = dist(gen);
            }
        }

        int m = nz_idx.size() - n_required;
        int keep = max_spp_selected - n_required;
        if (keep > 0 && keep < m) {
            std::nth_element(
              it,
              it + keep,
              nz_idx.end(),
              [&](int a, int b){
                if (selection[a+1] != selection[b+1])
                  return selection[a+1] > selection[b+1];
                return rnd_key[a] > rnd_key[b];
              }
            );
        }

        // Set the rest of the spp to zero
        for(int k = keep; k < m; ++k){
            int sp = nz_idx[n_required + k];
            selection[sp+1] = 0.0;
            unit_counts_l(sp, selected_region) -= 1;
            spp_units[sp] -= 1;
        }

    }
    exit_max_spp:

    // ---- Update tracking inputs ---- //
    for (int j = 0; j < nSpp; j++) {
        // Set the selected candidate row in suitability matrix to all zeros.
        suitability_l(selection[0], j) = 0.0;

        // Skip species if not selected
        if(selection[j+1] == 0) continue;

        // Increment target trackers
        current_total -= 1;
        spp_targets_l[j] -= 1;
        unit_counts_l(j, selected_region) -= 1;
        spp_units[j] -= 1;
        regional_max_l(j, selected_region) -= 1;
        if(regional_min_l(j, selected_region) > 0){
            regional_min_l(j, selected_region) -= 1;
            total_sub_targets[j] -= 1;
        }

        // If all targets are met for the species, zero out suitability and skip
        if (spp_targets_l[j] == 0) {
            spp_units[j] = 0;
            spp_populations[j] = 0;
            for(int r = 0; r < nRegions; r++){
                population_counts_l(j, r) = 0;
                unit_counts_l(j, r) = 0;
            }
            for (int i = 0; i < nUnits; i++) {
              suitability_l(i, j) = 0.0;
            }
            continue;
        }

        // Zero out suitability any region where the max possible selections is now zero
        for(int r = 0; r < nRegions; r++){
            int max = spp_targets_l[j] - total_sub_targets[j] + regional_min_l(j, r);
            regional_max_l(j, r) = std::min(max, unit_counts_l(j, r));
            if(regional_max_l(j, r) == 0) {
                spp_units[j] -= unit_counts_l(j, r);
                unit_counts_l(j, r) = 0;
                spp_populations[j] -= population_counts_l(j, r);
                population_counts_l(j, r) = 0;
                for(int i = 0; i < nUnits; i++){
                    if(unit_regions[i] - 1 != r) continue;
                    if(suitability_l(i, j) == 0.0) continue;
                    suitability_l(i, j) = 0.0;
                }
            }
        }
        // If current region has been zeroed out, skip population handing
        if(regional_max_l(j, selected_region) == 0){
            continue;
        }

        // Handle known populations;
        if (populations(selection[0], j) != NA_STRING) {
            population_counts_l(j, selected_region) -= 1;
            spp_populations[j] -= 1;
            // Handle population spanning multiple units if single_pu_pop = TRUE
            if(single_pu_pop){
                std::vector<int> pop_matches;
                pop_matches.reserve(nUnits);
                for (int i = 0; i < nUnits; ++i) {
                    if (i == selection[0]) continue;
                    if (populations(i, j) == populations(selection[0], j)) {
                        pop_matches.push_back(i);
                    }
                }
                if(pop_matches.size() > 0){
                    for (std::vector<int>::size_type k = 0; k < pop_matches.size(); ++k) {
                        suitability_l(pop_matches[k], j) = 0.0;
                    }
                }
            }
        }

        // When there are as many populations as targets, zero out non-population units
        // in any region where there are enough populations to meet the regional target
        if(spp_populations[j] >= spp_targets_l[j]){
            for(int r = 0; r < nRegions; r++){
                if(population_counts_l(j, r) == unit_counts_l(j, r)) continue;
                if(population_counts_l(j, r) >= regional_min_l(j, r)){
                    unit_counts_l(j, r) = population_counts_l(j, r);
                    int pop_track = 0;
                    for(int i = 0; i < nUnits; i++){
                        if(unit_regions[i] - 1 != r) continue;
                        if(suitability_l(i, j) == 0.0) continue;
                        if(CharacterVector::is_na(populations(i, j))){
                            spp_units[j] -= 1;
                            suitability_l(i, j) = 0.0;
                            pop_track += 1;
                        }
                    }
                }
            }
            continue;
        }

        for(int r = 0; r < nRegions; r++){
            if(population_counts_l(j, r) >= regional_max_l(j, r)){
                unit_counts_l(j, r) = population_counts_l(j, r);
                for(int i = 0; i < nUnits; i++){
                    if(unit_regions[i] - 1 != r) continue;
                    if(suitability_l(i, j) == 0.0) continue;
                    if(CharacterVector::is_na(populations(i, j))){
                        spp_units[j] -= 1;
                        suitability_l(i, j) = 0.0;
                    }
                }
            }
        }

    }

    // Append the output row to the results vector.
    results.push_back(selection);

  }

  // Construct final output matrix with one row per solution
  // First 3 columns are, solution_id, selection order, unit_index
  int n = results.size();
  IntegerMatrix finalMat(n, nSpp + 3);

  for (int i = 0; i < n; i++) {
    finalMat(i, 0) = solution_id;
    finalMat(i, 1) = i;
    finalMat(i, 2) = results[i][0] + 1; // convert unit index to 1-base for R
    for (int j = 0; j < nSpp; j++) {
      if(results[i][j+1] == 0) continue;
      finalMat(i, j+3) = 1;
    }
  }

  return finalMat;
}
