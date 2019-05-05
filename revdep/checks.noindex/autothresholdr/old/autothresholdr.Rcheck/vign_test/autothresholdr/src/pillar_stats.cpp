#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
NumericMatrix sum_pillars(NumericVector arr3d) {
  IntegerVector dim = arr3d.attr("dim");
  int n_pillars = dim[0] * dim[1];
  int pillar_len = dim[2];
  NumericMatrix sums(dim[0], dim[1]);
  double sum_i;
  NumericVector pillar_i(pillar_len);
  for (int i = 0; i < n_pillars; i++) {
    for (int j = 0; j < pillar_len; j++) {
      pillar_i[j] = arr3d[i + j * n_pillars];
    }
    sum_i = sum(pillar_i);
    sums(i % dim[0], i / dim[0]) = sum_i;
  }
  return sums;
}

// [[Rcpp::export]]
NumericMatrix mean_pillars(NumericVector arr3d) {
  IntegerVector dim = arr3d.attr("dim");
  int n_pillars = dim[0] * dim[1];
  int pillar_len = dim[2];
  NumericMatrix means(dim[0], dim[1]);
  double mean_i;
  NumericVector pillar_i(pillar_len);
  for (int i = 0; i < n_pillars; i++) {
    for (int j = 0; j < pillar_len; j++) {
      pillar_i[j] = arr3d[i + j * n_pillars];
    }
    mean_i = mean(pillar_i);
    means(i % dim[0], i / dim[0]) = mean_i;
  }
  return means;
}

// [[Rcpp::export]]
NumericMatrix var_pillars(NumericVector arr3d) {
  IntegerVector dim = arr3d.attr("dim");
  int n_pillars = dim[0] * dim[1];
  int pillar_len = dim[2];
  NumericMatrix vars(dim[0], dim[1]);
  double var_i;
  NumericVector pillar_i(pillar_len);
  for (int i = 0; i < n_pillars; i++) {
    for (int j = 0; j < pillar_len; j++) {
      pillar_i[j] = arr3d[i + j * n_pillars];
    }
    var_i = var(pillar_i);
    vars(i % dim[0], i / dim[0]) = var_i;
  }
  return vars;
}

// [[Rcpp::export]]
NumericMatrix median_pillars(NumericVector arr3d) {
  IntegerVector dim = arr3d.attr("dim");
  int n_pillars = dim[0] * dim[1];
  int pillar_len = dim[2];
  NumericMatrix meds(dim[0], dim[1]);
  double med_i;
  NumericVector pillar_i(pillar_len);
  for (int i = 0; i < n_pillars; i++) {
    for (int j = 0; j < pillar_len; j++) {
      pillar_i[j] = arr3d[i + j * n_pillars];
    }
    med_i = median(pillar_i);
    meds(i % dim[0], i / dim[0]) = med_i;
  }
  return meds;
}
