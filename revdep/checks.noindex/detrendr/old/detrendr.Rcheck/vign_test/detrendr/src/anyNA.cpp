#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool int_anyNA(IntegerVector x) {
  bool out = false;
  std::size_t n = x.size();
  for (std::size_t i = 0; i != n; ++i) {
    if (IntegerVector::is_na(x[i])) {
      out = true;
      break;
    }
  }
  return out;
}

// [[Rcpp::export]]
bool dbl_anyNA(NumericVector x) {
  bool out = false;
  std::size_t n = x.size();
  for (std::size_t i = 0; i != n; ++i) {
    if (NumericVector::is_na(x[i])) {
      out = true;
      break;
    }
  }
  return out;
}
