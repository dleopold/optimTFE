#include <Rcpp.h>
#include "solution_gen.h"
#include <algorithm>
#include <random>
using namespace Rcpp;


//' Generate a Single Conservation Solution
//'
//' Core optimization algorithm that generates a single conservation solution
//' through an iterative greedy selection process. The algorithm selects
//' planning units with the highest number of remaining species targets,
//' using randomization to avoid local minima.
//'
//' @param suitability NumericMatrix of species suitability scores
//' @param spp_targets IntegerVector of target populations for each species
//' @param unit_regions IntegerVector mapping planning units to regions
//' @param unit_counts IntegerMatrix of unit counts by species and region
//' @param regional_min IntegerMatrix of minimum targets for each species by region
//' @param regional_max IntegerMatrix of maximum targets for each species by region
//' @param populations IntegerMatrix of known populations for species in planning units
//' @param population_counts IntegerMatrix of population counts by species and region
//' @param single_pu_pop Logical flag for selecting only one unit per known population
//' @param rand_tolerance Integer range for randomization in planning unit selection
//' @param max_spp_selected Maximum number of species to select in each location
//' @param solution_id Integer identifier for this specific solution
//' @param incompat IntegerMatrix of species incompatibilities (1 = incompatible)
//' @param gen Random number generator reference
//' @return std::vector<int> of solution data in a flattened format
std::vector<int> solution_gen(
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
                            int solution_id,
                            IntegerMatrix incompat,
                            std::mt19937_64 &gen) {

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

//   std::mt19937_64 gen(std::random_device{}());
//   std::uniform_real_distribution<double> dist(0.0, 1.0);

  // Pre-calculate the sum of total spp targets.
  int current_total = 0;
  for (int j = 0; j < spp_targets_l.size(); j++) {
    current_total += spp_targets_l[j];
  }

  // Make region-specific sets of unit indicies
  std::vector<std::vector<int>> region_units(nRegions);
  for (int i = 0; i < nUnits; ++i){
    region_units[unit_regions[i] - 1].push_back(i);
  }

  // Total sub-region targets for each species
  std::vector<int> total_sub_targets(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      total_sub_targets[j] += regional_min_l(j, r);
    }
  }

  // Sum of total available units for each species
  std::vector<int> spp_units(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      spp_units[j] += unit_counts_l(j, r);
    }
  }

  // Sum of total populations for each species
  std::vector<int> spp_populations(nSpp, 0);
  for(int j = 0; j < nSpp; j++) {
    for(int r = 0; r < nRegions; r++) {
      spp_populations[j] += population_counts_l(j, r);
    }
  }

  // Container to accumulate the outputs from each iteration.
  std::vector< std::vector<double> > results;

  // Select units until all targets are met.
  int passing_solution = 1; // Track if the solution meets input requirements
  while (current_total > 0) {

    // Select a unit using suitability weighting
    std::vector<double> selection = select_unit(suitability_l, rand_tolerance, gen);
    // Stop if no units remain
    if(selection.size() == 0){
      break;
    }
    int selected_unit = selection[0];
    int selected_region = unit_regions[selected_unit] - 1; // Region index of selected unit (convert to 0-indexed)
    passing_solution = 1;

    // Find total species with any remaining potential targets in the selected unit
    // along with the species that must be retained for meeting targets (ie, can not
    // be excluded by max_spp_selected, or incompatibilities)
    std::vector<int> required_spp(nSpp, 0);
    std::vector<int> spp_selected;
    spp_selected.reserve(nSpp);
    int n_required = 0;
    for (int j = 0; j < nSpp; j++) {
        if (selection[j+1] > 0) {
            spp_selected.push_back(j);
            suitability_l(selected_unit, j) = 0.0;
            // Always keep species when # remaining targets equals the # remaining units
            if(spp_targets_l[j] == spp_units[j]) {
                required_spp[j] = 1;
                n_required += 1;
                continue;
            }
            // Always keep species when # remaining regional targets equals the # remaining units
            if (regional_min_l(j, selected_region) == unit_counts_l(j, selected_region)) {
                required_spp[j] = 1;
                n_required += 1;
                continue;
            }
            // Always keep known population
            if(populations(selected_unit, j) > 0) {
                required_spp[j] = 1;
                n_required += 1;
                continue;
            }
        }
    }
    std::shuffle(spp_selected.begin(), spp_selected.end(), gen);

    // Filter incomatabilities
    if(incompat.size() > 0) {
        size_t j = 0;
        while(j + 1 < spp_selected.size()) {
            int spa = spp_selected[j];
            size_t i = j + 1;
            while(i < spp_selected.size()) {
                int spb = spp_selected[i];
                if (incompat(spa, spb) == 1) {
                    bool a_req = required_spp[spa] == 1;
                    bool b_req = required_spp[spb] == 1;
                    if (a_req && b_req) {
                        passing_solution = 0;
                        i += 1;
                        continue;
                    }
                    // If neither species is required, select randomly (weighed on suitability)
                    if( !a_req && !b_req ) {
                        std::uniform_real_distribution<double> dist(0.0, selection[spa+1] + selection[spb+1]);
                        double r = dist(gen);
                        a_req = r < selection[spa+1];
                        b_req = !a_req;
                    }
                    // Remove one of the incompatible species
                    if( a_req ) {
                        selection[spb+1] = 0;
                        spp_selected.erase(spp_selected.begin() + i);
                        unit_counts_l(spb, selected_region) -= 1;
                        spp_units[spb] -= 1;
                        continue;
                    }
                    if( b_req ) {
                        selection[spa+1] = 0;
                        spp_selected.erase(spp_selected.begin() + j);
                        unit_counts_l(spa, selected_region) -= 1;
                        spp_units[spa] -= 1;
                        break;
                    }
                }
                i += 1; // not incompat -> continue
            }
            j += 1; // continue
        }
    }

    // Limit by max spp selected
    if (max_spp_selected > 0 && spp_selected.size() > static_cast<size_t>(max_spp_selected)) {

        // easy out if number of required spp is >= max_spp_selected
        if (n_required >= max_spp_selected) {
          if(n_required > max_spp_selected){
            passing_solution = 0;
          }
          size_t i = 0;
          while(i < spp_selected.size()) {
            int sp = spp_selected[i];
            if (required_spp[sp] == 0) {
              spp_selected.erase(spp_selected.begin() + i);
              selection[sp+1] = 0.0;
              unit_counts_l(sp, selected_region) -= 1;
              spp_units[sp] -= 1;
              continue;
            }
            i += 1;
          }
          goto exit_max_spp; // exit the max_spp_selected loop
        }

        // Partition into required and non-required spp
        auto it = std::stable_partition(
          spp_selected.begin(), spp_selected.end(),
          [&](int j){ return required_spp[j] == 1; }
        );

        // Input order for optional spp (for breaking ties - still random due to initial shuffle)
        std::vector<int> order_in(nSpp, 0);
        for (size_t i = n_required; i < spp_selected.size(); ++i) {
            order_in[ spp_selected[i] ] = i;
        }

        // reorder optional spp by suitability
        size_t m = spp_selected.size() - n_required;
        int keep = max_spp_selected - n_required;
        if (keep > 0 && static_cast<size_t>(keep) < m) {
          std::nth_element(
            it,
            it + keep,
            spp_selected.end(),
            [&](int a, int b){
              double va = selection[a+1], vb = selection[b+1];
              if (va != vb) return va > vb;
              return order_in[a] < order_in[b];
            }
          );
        }

        // Set the rest of the spp to zero
        auto k = it + keep;
        for (auto itr = spp_selected.end(); itr != k;) {
          --itr;
          int sp = *itr;
          selection[sp + 1] = 0.0;
          unit_counts_l(sp, selected_region) -= 1;
          spp_units[sp] -= 1;
          spp_selected.erase(itr);
        }

    }
    exit_max_spp:

    // ---- Update tracking inputs ---- //
    for(int sp : spp_selected){

         // Increment target trackers
        current_total -= 1;
        spp_targets_l[sp] -= 1;
        unit_counts_l(sp, selected_region) -= 1;
        spp_units[sp] -= 1;
        regional_max_l(sp, selected_region) -= 1;
        if(regional_min_l(sp, selected_region) > 0){
            regional_min_l(sp, selected_region) -= 1;
            total_sub_targets[sp] -= 1;
        }

        // If all targets are met for the species, zero out suitability and skip
        if (spp_targets_l[sp] == 0) {
            // spp_units[sp] = 0;
            // spp_populations[sp] = 0;
            // for(int r = 0; r < nRegions; r++){
            //     population_counts_l(sp, r) = 0;
            //     unit_counts_l(sp, r) = 0;
            // }
            for (int i = 0; i < nUnits; i++) {
              suitability_l(i, sp) = 0.0;
            }
            continue;
        }

        // Zero out suitability for any region where the max possible selections is now zero
        for(int r = 0; r < nRegions; r++){
            int new_max = spp_targets_l[sp] - total_sub_targets[sp] + regional_min_l(sp, r);
            new_max = std::min(new_max, unit_counts_l(sp, r));
            regional_max_l(sp, r) = new_max;
            if(new_max == 0) {
                int uc = unit_counts_l(sp, r);
                spp_units[sp] -= uc;
                unit_counts_l(sp, r) = 0;
                int pc = population_counts_l(sp, r);
                spp_populations[sp] -= pc;
                population_counts_l(sp, r) = 0;
                for(int unit : region_units[r]){
                    suitability_l(unit, sp) = 0.0;
                }
            }
        }

        // If current region has been zeroed out, skip population handing
        int spp_r_max = regional_max_l(sp, selected_region);
        if(spp_r_max == 0){
            continue;
        }

        // If there are no known populations, skip population logic
        if(spp_populations[sp] == 0){
            continue;
        }

        // Handle known populations;
        int selected_pop = populations(selected_unit, sp);
        if (selected_pop > 0) {
            population_counts_l(sp, selected_region) -= 1;
            spp_populations[sp] -= 1;
            // Handle population spanning multiple units if single_pu_pop = TRUE
            if(single_pu_pop){
                for(int unit : region_units[selected_region]){
                    if (populations(unit, sp) == selected_pop) {
                        suitability_l(unit, sp) = 0.0;
                    }
                }
            }
        }

        // When there are as many populations as targets, zero out non-population units
        // in any region where there are enough populations to meet the regional min target
        if(spp_populations[sp] >= spp_targets_l[sp]){
            for(int r = 0; r < nRegions; r++){
                int spp_rpops = population_counts_l(sp, r);
                if(spp_rpops == unit_counts_l(sp, r)) continue;
                if(spp_rpops >= regional_min_l(sp, r)){
                    unit_counts_l(sp, r) = spp_rpops;
                    for(int unit : region_units[r]){
                        if(populations(unit, sp) == 0){
                            spp_units[sp] -= 1;
                            suitability_l(unit, sp) = 0.0;
                        }
                    }
                }
            }
            continue;
        }

        // When regional populations equal to or greater than regional max, zero out non-population units
        for(int r = 0; r < nRegions; r++){
            int spp_rpops = population_counts_l(sp, r);
            if(spp_rpops >= regional_max_l(sp, r)){
                unit_counts_l(sp, r) = spp_rpops;
                for(int unit : region_units[r]){
                    if(populations(unit, sp) == 0){
                        spp_units[sp] -= 1;
                        suitability_l(unit, sp) = 0.0;
                    }
                }
            }
        }

    }

    // Append the output row to the results vector.
    results.push_back(selection);

  }

  // Construct final output
  size_t nrows = results.size(), ncols = nSpp + 4;
  std::vector<int> finalMat(nrows * ncols, 0);
  auto idx = [&](size_t i, size_t j){
    return i * ncols + j;
  };

  for (size_t i = 0; i < nrows; i++) {
    finalMat[idx(i,0)] = solution_id;             // solution id
    finalMat[idx(i,1)] = i + 1;                   // selection order
    finalMat[idx(i,2)] = results[i][0] + 1;       // planning unit index (base-1)
    finalMat[idx(i,3)] = passing_solution;        // does solution met requirements?
    for (int j = 0; j < nSpp; j++) {
      if(results[i][j+1] == 0) continue;
      finalMat[idx(i,j+4)] = 1;
    }
  }

  return finalMat;
}
