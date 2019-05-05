#ifndef DETRENDR_SMOOTH_
#define DETRENDR_SMOOTH_

#include <stdexcept>
#include <algorithm>
#include <numeric>

#include <Rcpp.h>

#include "summary_stats.h"

using namespace Rcpp;


template <class ReturnVec, class Vec>
ReturnVec weighted_smooth(const Vec& vec,
                          const Vec& weights) {
  const std::size_t vec_size = vec.size();
  const std::size_t weights_size = weights.size();
  if (weights_size % 2 == 0) {
    throw std::invalid_argument("The number of elements in weights "
                                "must be odd.");
  }
  const std::size_t l = weights_size / 2;
  if (l >= vec_size) {
    std::size_t l_ = vec_size - 1;
    Vec weights_(weights.begin() + l - l_, weights.begin() + l + l_ + 1);
    return weighted_smooth<ReturnVec>(vec, weights_);
  }
  ReturnVec smoothed(vec_size);
  std::size_t i = 0;
  while (i < vec_size) {
    std::size_t go_left = std::min(i, l);
    std::size_t go_right = std::min(vec_size - i, l + 1);
    double sum = std::inner_product(vec.begin() + i - go_left,
                                    vec.begin() + i + go_right,
                                    weights.begin() + l - go_left,
                                    0.0);
    double denominator = std::accumulate(weights.begin() + l - go_left,
                                         weights.begin() + l + go_right,
                                         0.0);
    smoothed[i] = sum / denominator;
    ++i;
  }
  return smoothed;
}


template <class ReturnVec, class Vec>
ReturnVec boxcar_smooth(const Vec& vec, const std::size_t l) {
  Vec weights(2 * l + 1);
  std::fill(weights.begin(), weights.end(), 1.0);
  return weighted_smooth<ReturnVec>(vec, weights);
}


template <class ReturnVec, class Vec>
ReturnVec exp_smooth(Vec vec, double tau, const std::size_t l) {
  Vec weights(2 * l + 1);
  weights[l] = 1;
  for (std::size_t i = 1; i != l + 1; ++i) {
    weights[l - i] = weights[l + i] = std::exp(- (double) i / tau);
  }
  return weighted_smooth<ReturnVec>(vec, weights);
}


#endif  // DETRENDR_SMOOTH_
