#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cross_var_Cpp(NumericVector x, NumericVector y) {
  double cross_var = mean((x - mean(x)) * (y - mean(y)));
  return cross_var;
}

// [[Rcpp::export]]
NumericMatrix cross_var_pillars_Cpp(NumericVector x3d, NumericVector y3d) {
  IntegerVector dim = x3d.attr("dim");
  int n_pillars = dim[0] * dim[1];
  int pillar_len = dim[2];
  NumericMatrix cross_vars(dim[0], dim[1]);
  NumericVector pillar_x(pillar_len);
  NumericVector pillar_y(pillar_len);
  double cv;
  for (int i = 0; i < n_pillars; i++) {
    for (int j = 0; j < pillar_len; j++) {
      pillar_x[j] = x3d[i + j * n_pillars];
      pillar_y[j] = y3d[i + j * n_pillars];
    }
    cv = cross_var_Cpp(pillar_x, pillar_y);
    cross_vars(i % dim[0], i / dim[0]) = cv;
  }
  return cross_vars;
}
