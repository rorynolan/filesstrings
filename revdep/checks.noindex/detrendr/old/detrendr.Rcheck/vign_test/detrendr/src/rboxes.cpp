#include <cstdint>
#include <numeric>
#include <stdexcept>
#include <vector>
#include <limits>
#include <algorithm>

#include "rboxes.h"
#include "summary_stats.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rfromboxes_(double n, IntegerVector balls, NumericVector weights,
                          int seed, LogicalVector quick) {
  // if `quick` is all `true`, `balls` and `weights` must not be the same object
  IntegerVector balls_maybeclone = balls;
  if (!quick[0])  // `balls` will be modified if `quick[0]` is `true`
    balls_maybeclone = clone(balls);
  NumericVector weights_maybeclone = weights;
  if (!quick[1])  // `weights` will be modified if `quick[1]` is `true`
    weights_maybeclone = clone(weights);
  return rfromboxes(n, balls_maybeclone, weights_maybeclone, seed);
}

// [[Rcpp::export]]
IntegerVector rtoboxes_(double n, double boxes, NumericVector weights,
                        IntegerVector capacities, int seed,
                        LogicalVector quick) {
  // if `quick` is all `true`, `weights` and `capacities`
  // must not be the same object
  NumericVector weights_maybeclone = weights;
  if (!quick[0])  // `weights` will be modified if `quick[0]` is `true`
    weights_maybeclone = clone(weights);
  IntegerVector capacities_maybeclone = capacities;
  if (!quick[1])  // `capacitites` will be modified if `quick[1]` is `true`
    capacities_maybeclone = clone(capacities);
  return rtoboxes(n, boxes, weights_maybeclone, capacities_maybeclone, seed);
}

// [[Rcpp::export]]
IntegerVector px_take_arr3d(IntegerVector arr3d, IntegerVector frames_losing,
                            int seed) {
  IntegerVector out(arr3d.size());
  Dimension d = arr3d.attr("dim");
  std::size_t nr = d[0], nc = d[1], n_frames = d[2];
  std::size_t frame_length = nr * nc, frames_losing_sz = frames_losing.size();
  if (n_frames != frames_losing_sz) {
    throw std::invalid_argument("The length of `frames_losing` must be the "
                                "same as the number of frames in `arr3d`.");
  }
  for (std::size_t i = 0; i != n_frames; ++i) {
    if (frames_losing[i]) {
      IntegerVector frame_i(arr3d.begin() + (i * frame_length),
                            arr3d.begin() + ((i + 1) * frame_length));
      for (std::size_t j = 0; j != frame_length; ++j) {
        if (IntegerVector::is_na(frame_i[j]))
          frame_i[j] = 0;
      }
      NumericVector weights_i(frame_i.begin(), frame_i.end());
      IntegerVector frame_i_pixels = rfromboxes(
        frames_losing[i], frame_i, weights_i, seed + i);
      std::copy(frame_i_pixels.begin(), frame_i_pixels.end(),
                out.begin() + (i * frame_length));
    }
  }
  out.attr("dim") = d;
  return out;
}

// [[Rcpp::export]]
NumericVector px_take_mat(NumericMatrix mat, NumericMatrix mat_orig,
                          NumericVector frames_losing,
                          int seed) {
  std::size_t n_frames = mat.ncol(), frame_length = mat.nrow();
  std::size_t frames_losing_sz = frames_losing.size();
  NumericMatrix out(frame_length, n_frames);
  if (n_frames != frames_losing_sz) {
    throw std::invalid_argument("The length of `frames_losing` must be the "
                                  "same as the number of frames in `arr3d`.");
  }
  for (std::size_t i = 0; i != n_frames; ++i) {
    if (frames_losing[i]) {
      NumericVector frame_i(mat.column(i).begin(), mat.column(i).end());
      std::vector<double> weights_i(mat_orig.column(i).begin(),
                                    mat_orig.column(i).end());
      out.column(i) = rfromboxes(frames_losing[i], frame_i, weights_i,
                                 seed + i);
    }
  }
  return out;
}
