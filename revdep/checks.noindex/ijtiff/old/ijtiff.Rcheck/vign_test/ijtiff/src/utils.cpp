#include <Rcpp.h>
#include <cfloat>
using namespace Rcpp;

// [[Rcpp::export]]
double float_max() {
  return FLT_MAX;
}

// [[Rcpp::export]]
List dims_cpp(List lst) {
  const std::size_t sz = lst.size();
  List dims(sz);
  for (std::size_t i = 0; i != sz; ++i) {
    NumericVector mat_i = as<NumericVector>(lst[i]);
    dims[i] = mat_i.attr("dim");
  }
  return dims;
}

// [[Rcpp::export]]
List enlist_img_cpp(NumericVector arr4d) {
  NumericVector d4 = arr4d.attr("dim");
  NumericVector d3(3);
  for (int i = 0; i != 3; ++i)
    d3[i] = d4[i];
  List out(d4[3]);
  std::size_t sub_len = d3[0] * d3[1] * d3[2];
  for (std::size_t j = 0; j != d4[3]; ++j) {
    NumericVector::iterator start_iter = arr4d.begin() + j * sub_len;
    NumericVector out_j(start_iter, start_iter + sub_len);
    out_j.attr("dim") = d3;
    out[j] = out_j;
  }
  return out;
}
