// [[Rcpp::depends(RcppParallel)]]

#include <random>

#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

struct MyRBernoulli : public Worker {

  const RVector<double> p;
  int seed;

  // destination
  RVector<int> output;

  // initialize with source and destination
  MyRBernoulli(NumericVector p, int seed, IntegerVector output) :
    p(p), seed(seed), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::minstd_rand generator_int(seed + begin);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    for (std::size_t i = begin; i != end; ++i) {
      int seed_i = distribution_int(generator_int);
      std::minstd_rand generator(seed_i);
      std::bernoulli_distribution distribution(p[i]);
      output[i] = distribution(generator);
    }
  }
};

// [[Rcpp::export]]
IntegerVector myrbernoulli_(NumericVector p, int seed) {

  std::size_t n = p.size();

  // allocate the matrix we will return
  IntegerVector output(n);

  // create the worker
  MyRBernoulli myRBernoulli(p, seed, output);

  // call it with parallelFor
  parallelFor(0, n, myRBernoulli);

  return output;
}
