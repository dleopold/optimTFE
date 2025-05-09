#include <Rcpp.h>
#include <random>
#include <chrono>
#include <thread>
#include <functional>
using namespace Rcpp;


//' Select a Planning Unit Based on Species Richness and Suitability
//'
//' This function selects a planning unit for conservation based on both species 
//' richness (number of species with non-zero suitability) and the mean suitability 
//' score. The selection uses a randomized approach that considers units within 
//' a tolerance range of the maximum richness, weighted by mean suitability.
//'
//' @param suitability NumericMatrix of species suitability scores, with planning
//'   units as rows and species as columns.
//' @param rand_tolerance Integer specifying the range of species richness from 
//'   maximum to consider for selection (allows randomization).
//' @param gen Random number generator reference for reproducible selection.
//' @return std::vector<double> containing the selected unit index (0-indexed) as 
//'   the first element, followed by the suitability scores for each species in 
//'   that unit.
std::vector<double> select_unit(NumericMatrix suitability, int rand_tolerance, std::mt19937_64 &gen) {
  int nUnits = suitability.nrow();
  int nSpp = suitability.ncol();

  // Vectors for counting non-zero elements and summing non-zero values in each row.
  std::vector<int> nonzero_counts(nUnits, 0);
  std::vector<double> sums(nUnits, 0.0);

  // Loop over each row to compute non-zero count and sum.
  for (int i = 0; i < nUnits; i++) {
    for (int j = 0; j < nSpp; j++) {
      double val = suitability(i, j);
      if (val != 0.0) {
        nonzero_counts[i] += 1;
        sums[i] += val;
      }
    }
  }

  // Find the maximum count of non-zero values across all rows.
  int max_nonzero = 0;
  for (int i = 0; i < nUnits; i++) {
    if (nonzero_counts[i] > max_nonzero)
      max_nonzero = nonzero_counts[i];
  }

  // Determine the threshold for candidate rows.
  int min_required = max_nonzero - rand_tolerance;

  // Identify candidate rows and calculate each candidate's weight (the mean of non-zero values).
  std::vector<int> candidate_indices;
  std::vector<double> candidate_weights;
  for (int i = 0; i < nUnits; i++) {
    if (nonzero_counts[i] > 0 && nonzero_counts[i] >= min_required) {
      double mean_val = sums[i] / nonzero_counts[i];
      candidate_indices.push_back(i);
      candidate_weights.push_back(mean_val);
    }
  }

  // Ensure that there is at least one candidate row.
  int nCandidates = candidate_indices.size();
  if (nCandidates == 0) {
    // stop("No candidate rows found.");
    // TODO! decide if we want to stop here or not
    Rprintf("STOPPING EARLY - SOMETHING IS WRONG\n");
    return {};
  }

  // Compute the total weight from all candidate rows.
  double total_weight = 0.0;
  for (int i = 0; i < nCandidates; i++) {
    total_weight += candidate_weights[i];
  }

  // Perform weighted random selection
//   unsigned seed = std::chrono::system_clock::now().time_since_epoch().count() +
//                  std::hash<std::thread::id>{}(std::this_thread::get_id());
//   std::mt19937 generator(seed);
  std::uniform_real_distribution<double> distribution(0.0, total_weight);

  double random_value = distribution(gen);
  double cumulative = 0.0;
  int selected_candidate = candidate_indices[0];
  for (int i = 0; i < nCandidates; i++) {
    cumulative += candidate_weights[i];
    if (random_value <= cumulative) {
      selected_candidate = candidate_indices[i];
      break;
    }
  }

  // Construct Output (index of selected unit + binary spp vector)
  std::vector<double> output(nSpp + 1);
  output[0] = selected_candidate; // zero-indexed
  for (int j = 0; j < nSpp; j++) {
    output[j+1] = suitability(selected_candidate, j);
  }

  return output;

}
