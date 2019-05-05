#include <Rcpp.h>
#include <cfloat>
using namespace Rcpp;

// [[Rcpp::export]]
double float_max() {
  return FLT_MAX;
}
