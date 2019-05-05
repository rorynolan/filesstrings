// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>

#include <numeric>

#include "summary_stats.h"
#include "pillar_utils.h"

using namespace Rcpp;
using namespace RcppParallel;


struct SumPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  SumPillars(NumericVector arr3d, IntegerVector arr3d_dim,
             NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % nrow;
      size_t col = p / nrow;
      std::vector<double> pillar_p =
        extract_pillar<double>(arr3d, arr3d_dim, p);
      output(row, col) = std::accumulate(pillar_p.begin(), pillar_p.end(), 0.0);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix sum_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  SumPillars sumPillars(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], sumPillars);

  return output;
}


struct MeanPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  MeanPillars(NumericVector arr3d, IntegerVector arr3d_dim,
              NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % nrow;
      size_t col = p / nrow;
      std::vector<double> pillar_p =
        extract_pillar<double>(arr3d, arr3d_dim, p);
      output(row, col) = mymean(pillar_p);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix mean_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  MeanPillars meanPillars(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], meanPillars);

  return output;
}


struct VarPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  VarPillars(NumericVector arr3d, IntegerVector arr3d_dim,
             NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % nrow;
      size_t col = p / nrow;
      std::vector<double> pillar_p =
        extract_pillar<double>(arr3d, arr3d_dim, p);
      output(row, col) = myvar(pillar_p);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix var_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  VarPillars varPillars(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], varPillars);

  return output;
}


struct MedianPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  MedianPillars(NumericVector arr3d, IntegerVector arr3d_dim,
                NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % nrow;
      size_t col = p / nrow;
      std::vector<double> pillar_p =
        extract_pillar<double>(arr3d, arr3d_dim, p);
      output(row, col) = mymedian(pillar_p);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix median_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  MedianPillars medianPillars(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], medianPillars);

  return output;
}


struct BrightnessPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  BrightnessPillars(NumericVector arr3d, IntegerVector arr3d_dim,
                    NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % nrow;
      size_t col = p / nrow;
      std::vector<double> pillar_p =
        extract_pillar<double>(arr3d, arr3d_dim, p);
      output(row, col) = brightness(pillar_p);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix brightness_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  BrightnessPillars brightnessPillars(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], brightnessPillars);

  return output;
}
