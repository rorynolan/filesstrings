#include <Rcpp.h>

#include "smooth.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector boxcar_smooth(NumericVector vec, std::size_t l) {
  return boxcar_smooth<NumericVector>(vec, l);
}

// [[Rcpp::export]]
NumericVector weighted_smooth(NumericVector vec, NumericVector weights) {
  return weighted_smooth<NumericVector>(vec, weights);
}

// [[Rcpp::export]]
NumericVector exp_smooth(NumericVector vec, double tau, std::size_t l) {
  return exp_smooth<NumericVector>(vec, tau, l);
}
