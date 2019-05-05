// [[Rcpp::depends(RcppParallel)]]

#include <random>

#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;


int mysign(double x) {
  if (x >= 0) {
    return 1;
  } else {
    return -1;
  }
}

struct MyRPois : public Worker {

  const RVector<double> means;
  int seed;

  // destination
  RVector<int> output;

  // initialize with source and destination
  MyRPois(NumericVector means, int seed, IntegerVector output) :
    means(means), seed(seed), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::minstd_rand generator_int(seed + begin);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    for (std::size_t i = begin; i != end; ++i) {
      int seed_i = distribution_int(generator_int);
      std::minstd_rand generator(seed_i);
      std::poisson_distribution<int> distribution(std::abs(means[i]));
      output[i] = distribution(generator) * mysign(means[i]);
    }
  }
};

// [[Rcpp::export]]
IntegerVector myrpois_(NumericVector means, int seed) {

  std::size_t n = means.size();

  // allocate the matrix we will return
  IntegerVector output(n);

  // create the worker
  MyRPois myRPois(means, seed, output);

  // call it with parallelFor
  parallelFor(0, n, myRPois);

  return output;
}

struct MyRPoisFrames : public Worker {

  const RVector<double> means;
  std::size_t frame_length;
  int seed;

  // destination
  RMatrix<int> output;

  // initialize with source and destination
  MyRPoisFrames(NumericVector means, std::size_t frame_length,
                int seed, IntegerMatrix output) :
    means(means), frame_length(frame_length), seed(seed), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      std::minstd_rand generator_int(seed + begin);
      std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
      int seed_i = distribution_int(generator_int);
      std::minstd_rand generator(seed_i);
      std::poisson_distribution<int> distribution(std::abs(means[i]));
      for (std::size_t j = 0; j != frame_length; ++j) {
        output(j, i) = distribution(generator);
      }
    }
  }
};

// [[Rcpp::export]]
IntegerMatrix myrpois_frames_(NumericVector means, std::size_t frame_length,
                              int seed) {

  std::size_t ncol = means.size();

  IntegerMatrix output(frame_length, ncol);

  // create the worker
  MyRPoisFrames myRPoisFrames(means, frame_length, seed, output);

  // call it with parallelFor
  parallelFor(0, ncol, myRPoisFrames);

  return output;
}


struct MyRPoisFramesT : public Worker {

  const RVector<double> means;
  std::size_t frame_length;
  int seed;

  // destination
  RMatrix<int> output;

  // initialize with source and destination
  MyRPoisFramesT(NumericVector means, std::size_t frame_length,
                 int seed, IntegerMatrix output) :
    means(means), frame_length(frame_length), seed(seed), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      std::minstd_rand generator_int(seed + begin);
      std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
      int seed_i = distribution_int(generator_int);
      std::minstd_rand generator(seed_i);
      std::poisson_distribution<int> distribution(std::abs(means[i]));
      for (std::size_t j = 0; j != frame_length; ++j) {
        output(i, j) = distribution(generator);
      }
    }
  }
};

// [[Rcpp::export]]
IntegerMatrix myrpois_frames_t_(NumericVector means, std::size_t frame_length,
                                int seed) {

  std::size_t nrow = means.size();

  IntegerMatrix output(nrow, frame_length);

  // create the worker
  MyRPoisFramesT myRPoisFramesT(means, frame_length, seed, output);

  // call it with parallelFor
  parallelFor(0, nrow, myRPoisFramesT);

  return output;
}
