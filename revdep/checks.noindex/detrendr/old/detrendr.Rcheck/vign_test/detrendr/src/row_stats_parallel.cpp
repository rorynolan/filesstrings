// [[Rcpp::depends(RcppParallel)]]

#include <vector>
#include <numeric>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct BrightnessRows : public Worker {

  RMatrix<int> rows;

  // destination
  RVector<double> output;

  // initialize with source and destination
  BrightnessRows(IntegerMatrix rows, NumericVector output) :
    rows(rows), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
        output[i] = brightness(row_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector brightness_rows_(IntegerMatrix rows) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  BrightnessRows brightnessRows(rows, output);

  // call it with parallelFor
  parallelFor(0, nrow, brightnessRows);

  return output;
}

struct BrightnessRowsGivenMean : public Worker {

  RMatrix<int> rows;
  RVector<double> means;

  // destination
  RVector<double> output;

  // initialize with source and destination
  BrightnessRowsGivenMean(IntegerMatrix rows, NumericVector means,
                          NumericVector output) :
    rows(rows), means(means), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
      output[i] = brightness(row_i, means[i]);
    }
  }
};

// [[Rcpp::export]]
NumericVector brightness_rows_given_mean_(IntegerMatrix rows,
                                          NumericVector means) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  BrightnessRowsGivenMean brightnessRowsGivenMean(rows, means, output);

  // call it with parallelFor
  parallelFor(0, nrow, brightnessRowsGivenMean);

  return output;
}

struct MeanRows : public Worker {

  RMatrix<int> rows;

  // destination
  RVector<double> output;

  // initialize with source and destination
  MeanRows(IntegerMatrix rows, NumericVector output) :
    rows(rows), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
      output[i] = mymean(row_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector mean_rows_(IntegerMatrix rows) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  MeanRows meanRows(rows, output);

  // call it with parallelFor
  parallelFor(0, nrow, meanRows);

  return output;
}


struct VarRowsGivenMean : public Worker {

  RMatrix<int> rows;
  RVector<double> means;

  // destination
  RVector<double> output;

  // initialize with source and destination
  VarRowsGivenMean(IntegerMatrix rows, NumericVector means,
                   NumericVector output) :
    rows(rows), means(means), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
      output[i] = myvar(row_i, means[i]);
    }
  }
};

// [[Rcpp::export]]
NumericVector var_rows_given_mean_(IntegerMatrix rows, NumericVector means) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  VarRowsGivenMean varRowsGivenMean(rows, means, output);

  // call it with parallelFor
  parallelFor(0, nrow, varRowsGivenMean);

  return output;
}


struct SumRows : public Worker {

  RMatrix<int> rows;

  // destination
  RVector<double> output;

  // initialize with source and destination
  SumRows(IntegerMatrix rows, NumericVector output) :
    rows(rows), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      output[i] = std::accumulate(rows.row(i).begin(), rows.row(i).end(), 0.0);
    }
  }
};

// [[Rcpp::export]]
NumericVector sum_rows_(IntegerMatrix rows) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  SumRows sumRows(rows, output);

  // call it with parallelFor
  parallelFor(0, nrow, sumRows);

  return output;
}
