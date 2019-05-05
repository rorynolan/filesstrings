#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector which_interval_(NumericVector numbers, NumericMatrix ranges) {
  int nn = numbers.size();
  IntegerVector interval(nn);
  for (int i = 0; i < nn; i++) {
    for (int j = 0; j < ranges.nrow(); j++) {
      if (numbers[i] > ranges(j, 0) && numbers[i] <= ranges(j, 1)) {
        interval[i] = j + 1;
        break;
      }
    }
  }
  interval.attr("dim") = numbers.attr("dim");
  return(interval);
}

// [[Rcpp::export]]
IntegerVector spread_specific_helper(NumericVector interval_lengths,
                                     IntegerVector interval_pops,  //populations
                                     int m) {
  NumericVector next_betw_lengths;
  NumericVector interval_pops_num = as<NumericVector>(interval_pops);
  int min_pos;
  for (int i = 0; i < m; i++) {
    next_betw_lengths = interval_lengths / interval_pops_num;
    min_pos = which_max(next_betw_lengths);
    interval_pops_num[min_pos]++;
  }
  return(as<IntegerVector>(interval_pops_num));
}
