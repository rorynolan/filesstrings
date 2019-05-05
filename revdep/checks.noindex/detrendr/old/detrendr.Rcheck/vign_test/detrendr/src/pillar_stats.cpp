#include "pillar_utils_rcpp.h"
#include "summary_stats.h"

#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
LogicalMatrix int_anyNA_pillars(IntegerVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1];
  LogicalMatrix out(nrow, ncol);
  std::size_t n_pillars = d[0] * d[1];
  for (std::size_t p = 0; p != n_pillars; ++p) {
    IntegerVector pillar_p = extract_pillar(arr3d, p);
    std::size_t row = p % nrow;
    std::size_t col = p / nrow;
    out(row, col) = int_anyNA(pillar_p);
  }
  return out;
}

// [[Rcpp::export]]
LogicalMatrix dbl_anyNA_pillars(NumericVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1];
  LogicalMatrix out(nrow, ncol);
  std::size_t n_pillars = d[0] * d[1];
  for (std::size_t p = 0; p != n_pillars; ++p) {
    NumericVector pillar_p = extract_pillar(arr3d, p);
    std::size_t row = p % nrow;
    std::size_t col = p / nrow;
    out(row, col) = dbl_anyNA(pillar_p);
  }
  return out;
}
