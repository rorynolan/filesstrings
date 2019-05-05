// [[Rcpp::depends(RcppParallel)]]

#include <vector>
#include <algorithm>

#include <Rcpp.h>
#include <RcppParallel.h>

#include "pillar_utils.h"
#include "summary_stats.h"

using namespace Rcpp;
using namespace RcppParallel;


struct PillarsToCols : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  // initialize with source and destination
  PillarsToCols(NumericVector arr3d, IntegerVector arr3d_dim,
                NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t p = begin; p != end; ++p) {
      std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim,
                                                            p);
      std::copy(pillar_p.begin(), pillar_p.end(), output.column(p).begin());
    }
  }
};

// [[Rcpp::export]]
NumericMatrix pillars_to_cols_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  std::size_t n_pillars = arr3d_dim[0] * arr3d_dim[1];

  NumericMatrix output(arr3d_dim[2], n_pillars);

  // create the worker
  PillarsToCols pillarsToCols(arr3d, arr3d_dim, output);

  // call it with parallelFor
  parallelFor(0, n_pillars, pillarsToCols);

  return output;
}


struct ColsToPillars : public Worker {

  RMatrix<double> mat;

  RVector<int> output_dim;
  RVector<double> output;

  // initialize with source and destination
  ColsToPillars(NumericMatrix mat, IntegerVector output_dim,
                NumericVector output) :
    mat(mat), output_dim(output_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t p = begin; p != end; ++p) {
      std::vector<double> pillar_p(mat.column(p).begin(), mat.column(p).end());
      assign_pillar(output, output_dim, pillar_p, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector cols_to_pillars_(NumericMatrix mat,
                               IntegerVector output_dim) {

  NumericVector output(myprod(output_dim));

  // create the worker
  ColsToPillars colsToPillars(mat, output_dim, output);

  // call it with parallelFor
  parallelFor(0, mat.ncol(), colsToPillars);

  output.attr("dim") = output_dim;

  return output;
}
