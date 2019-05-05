#ifndef DETRENDR_PILLAR_UTILS_RCPP_
#define DETRENDR_PILLAR_UTILS_RCPP_

#include <Rcpp.h>
using namespace Rcpp;

IntegerVector extract_pillar(IntegerVector arr3d, const std::size_t p) {
  Dimension arr3d_dim = arr3d.attr("dim");
  std::size_t nrow = arr3d_dim[0], ncol = arr3d_dim[1];
  std::size_t pillar_len = arr3d_dim[2];
  IntegerVector pillar(pillar_len);
  std::size_t row = p % nrow;
  std::size_t col = p / nrow;
  for (std::size_t slice = 0; slice != pillar_len; ++slice) {
    pillar[slice] = arr3d[slice * ncol * nrow +
                          col * nrow +
                          row];
  }
  return pillar;
}

NumericVector extract_pillar(NumericVector arr3d, const std::size_t p) {
  Dimension arr3d_dim = arr3d.attr("dim");
  std::size_t nrow = arr3d_dim[0], ncol = arr3d_dim[1];
  std::size_t pillar_len = arr3d_dim[2];
  NumericVector pillar(pillar_len);
  std::size_t row = p % nrow;
  std::size_t col = p / nrow;
  for (std::size_t slice = 0; slice != pillar_len; ++slice) {
    pillar[slice] = arr3d[slice * ncol * nrow +
      col * nrow +
      row];
  }
  return pillar;
}

#endif  // DETRENDR_PILLAR_UTILS_RCPP_
