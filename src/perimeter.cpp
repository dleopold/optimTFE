#include <Rcpp.h>
using namespace Rcpp;

//' Compute perimeters for many subsets in C++
//' @param subsets list of integer vectors of polygon-IDs
//' @param keyPolygonIds list of integer vectors (per segment)
//' @param lengths numeric vector of segment lengths
//' @param Pmax integer maximum polygon ID
//' @return numeric vector of perimeters
//' @noRd
// [[Rcpp::export]]
NumericVector compute_perimeters(
    const List& subsets,
    const List& keyPolygonIds,
    const NumericVector& lengths,
    int Pmax
) {

  int M = subsets.size();
  int N = lengths.size();

  std::vector<int> mark(Pmax + 1, 0);
  NumericVector out(M);

  for(int s = 0; s < M; ++s) {
    IntegerVector sub = as<IntegerVector>(subsets[s]);
    int tag = s + 1;

    for(int j = 0; j < sub.size(); ++j) {
      int id = sub[j];
      if(id >= 0 && id <= Pmax) mark[id] = tag;
    }

    double peri = 0.0;
    for(int i = 0; i < N; ++i) {
      IntegerVector ids = keyPolygonIds[i];
      int cnt = 0;
      for(int j = 0; j < ids.size(); ++j) {
        int id = ids[j];
        if(id >= 0 && id <= Pmax && mark[id] == tag) ++cnt;
      }
      if(cnt == 1) peri += lengths[i];
    }

    out[s] = peri;

  }

  return out;
}
