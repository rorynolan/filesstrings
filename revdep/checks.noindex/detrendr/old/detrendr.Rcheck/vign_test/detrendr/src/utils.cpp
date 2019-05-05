#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix mat_add1s(IntegerMatrix mat, IntegerMatrix add_pos) {
  // beware that this function modifies its input `mat` in the parent environment
  std::size_t npos = add_pos.nrow();
  for (std::size_t i = 0; i != npos; ++i)
    mat(add_pos(i, 0) - 1, add_pos(i, 1) - 1) += 1;
  return mat;
}

// [[Rcpp::export]]
IntegerVector vec_add1s(IntegerVector vec, IntegerVector add_pos) {
  // beware that this function modifies its input `arr3d` in the parent env
  Dimension d = vec.attr("dim");
  std::size_t npos = add_pos.size();
  for (std::size_t i = 0; i != npos; ++i)
    vec[add_pos[i] - 1] += 1;
  vec.attr("dim") = d;
  return vec;
}

