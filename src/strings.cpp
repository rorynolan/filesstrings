#include <string>

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector intmat_list_nrows(List intmat_list) {
  const std::size_t intmat_list_size = intmat_list.size();
  IntegerVector nrows(intmat_list_size);
  for (std::size_t i = 0; i != intmat_list_size; ++i) {
    IntegerMatrix mat_i = as<IntegerMatrix>(intmat_list[i]);
    nrows[i] = mat_i.nrow();
  }
  return nrows;
}
