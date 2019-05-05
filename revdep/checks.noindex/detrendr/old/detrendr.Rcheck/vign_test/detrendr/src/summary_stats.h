// [[Rcpp::depends(RcppParallel)]]

#ifndef DETRENDR_SUMMARY_STATS_
#define DETRENDR_SUMMARY_STATS_

#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <functional>
#include <iterator>

#include <Rcpp.h>

template <class Vec>
int myprod(const Vec& vec) {
  return std::accumulate(vec.begin(), vec.end(), 1.0, std::multiplies<int>());
}

template <class Iter>
double mymean(Iter start, Iter end) {
  if (start == end) return NAN;
  return std::accumulate(start, end, 0.0) / std::distance(start, end);
}

template <class Vec>
double mymean(const Vec& vec) {
  if (vec.size() == 0) return NAN;
  return std::accumulate(vec.begin(), vec.end(), 0.0) / vec.size();
}

template <class Vec>
double myvar(const Vec& vec) {
  double mean = mymean(vec);
  double accum = 0.0;
  double diff;
  std::for_each(std::begin(vec), std::end(vec), [&](const double vec_elem) {
    diff = vec_elem - mean;
    accum += diff * diff;
  });
  return accum / (vec.size() - 1);
}

template <class Vec>
double myvar(const Vec& vec, const double mean) {
  double accum = 0.0;
  double diff;
  std::for_each(std::begin(vec), std::end(vec), [&](const double vec_elem) {
    diff = vec_elem - mean;
    accum += diff * diff;
  });
  return accum / (vec.size() - 1);
}

template <class Vec>
double brightness(Vec& vec) {
  return myvar(vec) / mymean(vec);
}

template <class Vec>
double brightness(Vec& vec, const double mean) {
  return myvar(vec, mean) / mean;
}

template <class T>
double mymedian(std::vector<T>& vec) {
  if (vec.size() == 0) return NAN;
  typedef typename std::vector<T>::size_type vec_sz;
  typedef typename std::vector<T>::iterator vec_it;
  vec_sz n = vec.size() / 2;
  std::nth_element(vec.begin(), vec.begin() + n, vec.end());
  double med = vec[n];
  if(!(vec.size() & 1)) { //If the set size is even
    vec_it max_it = std::max_element(vec.begin(), vec.begin() + n);
    med = (*max_it + med) / 2.0;
  }
  return med;
}

bool int_anyNA(Rcpp::IntegerVector x);
bool dbl_anyNA(Rcpp::NumericVector x);

#endif  // DETRENDR_SUMMARY_STATS_
