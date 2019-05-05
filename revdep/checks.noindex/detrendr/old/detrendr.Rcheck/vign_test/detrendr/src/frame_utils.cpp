// [[Rcpp::depends(RcppParallel)]]

#include <numeric>

#include <Rcpp.h>
#include <RcppParallel.h>

#include "summary_stats.h"
#include "pillar_utils.h"

using namespace Rcpp;
using namespace RcppParallel;


struct MeanFrames : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RVector<double> output;

  MeanFrames(NumericVector arr3d, IntegerVector arr3d_dim,
             NumericVector output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];
    for (std::size_t frame = begin; frame != end; ++frame) {
      output[frame] = mymean(arr3d.begin() + (frame_length * frame),
                             arr3d.begin() + (frame_length * (frame + 1)));
    }
  }
};

// [[Rcpp::export]]
NumericVector mean_frames_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericVector output(arr3d_dim[2]);

  MeanFrames meanFrames(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[2], meanFrames);

  return output;
}

struct SumFrames : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RVector<double> output;

  SumFrames(NumericVector arr3d, IntegerVector arr3d_dim,
            NumericVector output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];
    for (std::size_t frame = begin; frame != end; ++frame) {
      output[frame] = std::accumulate(arr3d.begin() + (frame_length * frame),
                                      arr3d.begin() +
                                        (frame_length * (frame + 1)),
                                      0.0);
    }
  }
};

// [[Rcpp::export]]
NumericVector sum_frames_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericVector output(arr3d_dim[2]);

  SumFrames sumFrames(arr3d, arr3d_dim, output);

  parallelFor(0, arr3d_dim[2], sumFrames);

  return output;
}


double sum_na_omit(NumericVector x) {
  NumericVector x_noNA = na_omit(x);
  if (x_noNA.size() > 0) {
    return sum(x_noNA);
  } else {
    return NA_REAL;
  }
}
double mean_na_omit(NumericVector x) {
  NumericVector x_noNA = wrap(na_omit(x));
  if (x_noNA.size() > 0) {
    return mean(x_noNA);
  } else {
    return NA_REAL;
  }
}
double sum_na_omit(IntegerVector x) {
  IntegerVector x_noNA = wrap(na_omit(x));
  if (x_noNA.size() > 0) {
    return sum(x_noNA);
  } else {
    return NA_REAL;
  }
}
double mean_na_omit(IntegerVector x) {
  IntegerVector x_noNA = na_omit(x);
  if (x_noNA.size() > 0) {
    return mean(x_noNA);
  } else {
    return NA_REAL;
  }
}

// [[Rcpp::export]]
NumericVector int_sum_frames_na_omit(IntegerVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1], n_frames = d[2];
  NumericVector out(n_frames);
  std::size_t frame_size = nrow * ncol;
  for (std::size_t i = 0; i != n_frames; ++i) {
    IntegerVector frame_i(arr3d.begin() + (i * frame_size),
                          arr3d.begin() + ((i + 1) * frame_size));
    out[i] = sum_na_omit(frame_i);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector dbl_sum_frames_na_omit(NumericVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1], n_frames = d[2];
  NumericVector out(n_frames);
  std::size_t frame_size = nrow * ncol;
  for (std::size_t i = 0; i != n_frames; ++i) {
    NumericVector frame_i(arr3d.begin() + (i * frame_size),
                          arr3d.begin() + ((i + 1) * frame_size));
    out[i] = sum_na_omit(frame_i);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector int_mean_frames_na_omit(IntegerVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1], n_frames = d[2];
  NumericVector out(n_frames);
  std::size_t frame_size = nrow * ncol;
  for (std::size_t i = 0; i != n_frames; ++i) {
    IntegerVector frame_i(arr3d.begin() + (i * frame_size),
                          arr3d.begin() + ((i + 1) * frame_size));
    out[i] = mean_na_omit(frame_i);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector dbl_mean_frames_na_omit(NumericVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t nrow = d[0], ncol = d[1], n_frames = d[2];
  NumericVector out(n_frames);
  std::size_t frame_size = nrow * ncol;
  for (std::size_t i = 0; i != n_frames; ++i) {
    NumericVector frame_i(arr3d.begin() + (i * frame_size),
                          arr3d.begin() + ((i + 1) * frame_size));
    out[i] = mean_na_omit(frame_i);
  }
  return out;
}

