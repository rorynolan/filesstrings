// [[Rcpp::depends(RcppParallel)]]

#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "smooth.h"
#include "pillar_utils.h"
#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct BoxcarSmoothRows : public Worker {
  // source matrix
  RMatrix<double> mat;

  const std::size_t l;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  BoxcarSmoothRows(NumericMatrix mat, std::size_t l, NumericMatrix output) :
    mat(mat), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> row_i(mat.row(i).begin(), mat.row(i).end());
      vector<double> smoothed = boxcar_smooth<vector<double> >(row_i, l);
      std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix boxcar_smooth_rows_(NumericMatrix mat, std::size_t l) {

  // allocate the matrix we will return
  NumericMatrix output(mat.nrow(), mat.ncol());

  // create the worker
  BoxcarSmoothRows boxcarSmoothRows(mat, l, output);

  // call it with parallelFor
  parallelFor(0, mat.nrow(), boxcarSmoothRows);

  return output;
}


struct BoxcarSmoothPillars : public Worker {

  const RVector<double> arr;
  const RVector<int> arr_dim;

  const std::size_t l;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  BoxcarSmoothPillars(NumericVector arr, IntegerVector arr_dim,
                      std::size_t l, NumericVector output) :
    arr(arr), arr_dim(arr_dim), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> arr_pillar(arr_dim[2]);
    vector<double> smoothed_pillar(arr_dim[2]);
    for (std::size_t p = begin; p != end; ++p) {
      arr_pillar = extract_pillar<double>(arr, arr_dim, p);
      smoothed_pillar = boxcar_smooth<vector<double> >(arr_pillar, l);
      assign_pillar(output, arr_dim, smoothed_pillar, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector boxcar_smooth_pillars_(NumericVector arr, std::size_t l) {
  IntegerVector arr_dim = arr.attr("dim");

  // allocate the matrix we will return
  NumericVector output(myprod(arr_dim));

  // create the worker
  BoxcarSmoothPillars boxcarSmoothPillars(arr, arr_dim, l, output);

  // call it with parallelFor
  parallelFor(0, arr_dim[0] * arr_dim[1], boxcarSmoothPillars);

  output.attr("dim") = arr_dim;

  return output;
}



struct ExpSmoothRows : public Worker {
  // source matrix
  RMatrix<double> mat;


  const double tau;

  const std::size_t l;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  ExpSmoothRows(NumericMatrix mat, double tau, std::size_t l,
                NumericMatrix output) :
    mat(mat), tau(tau), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> smoothed(mat.ncol());
    vector<double> weights(2 * l + 1);
    weights[l] = 1;
    for (std::size_t i = 1; i != l + 1; ++i) {
      weights[l - i] = weights[l + i] = std::exp(- (double) i / tau);
    }
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> row_i(mat.row(i).begin(), mat.row(i).end());
      smoothed = weighted_smooth<vector<double> >(row_i, weights);
      std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix exp_smooth_rows_(NumericMatrix mat, double tau, std::size_t l) {

  // allocate the matrix we will return
  NumericMatrix output(mat.nrow(), mat.ncol());

  // create the worker
  ExpSmoothRows expSmoothRows(mat, tau, l, output);

  // call it with parallelFor
  parallelFor(0, mat.nrow(), expSmoothRows);

  return output;
}


struct ExpSmoothPillars : public Worker {

  RVector<double> arr;
  const RVector<int> arr_dim;

  const double tau;

  const int l;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  ExpSmoothPillars(NumericVector arr, IntegerVector arr_dim,
                   double tau, int l, NumericVector output) :
    arr(arr), arr_dim(arr_dim),
    tau(tau), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> arr_pillar(arr_dim[2]);
    std::vector<int> output_dim = {arr_dim[0], arr_dim[1], arr_dim[2]};
    vector<double> smoothed_pillar(output_dim[2]);
    vector<double> weights(2 * l + 1);
    weights[l] = 1;
    for (int i = 1; i != l + 1; ++i) {
      weights[l - i] = weights[l + i] = std::exp(- i / tau);
    }
    for (std::size_t p = begin; p != end; ++p) {
      arr_pillar = extract_pillar<double>(arr, arr_dim, p);
      smoothed_pillar = weighted_smooth<vector<double> >(arr_pillar, weights);
      assign_pillar(output, output_dim, smoothed_pillar, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector exp_smooth_pillars_(NumericVector arr, double tau, int l) {
  IntegerVector arr_dim = arr.attr("dim");

  IntegerVector output_dim = IntegerVector::create(
    arr_dim[0], arr_dim[1], arr_dim[2]);

  // allocate the matrix we will return
  NumericVector output(myprod(output_dim));

  // create the worker
  ExpSmoothPillars expSmoothPillars(arr, arr_dim, tau, l, output);

  // call it with parallelFor
  parallelFor(0, arr_dim[0] * arr_dim[1], expSmoothPillars);

  output.attr("dim") = output_dim;

  return output;
}
