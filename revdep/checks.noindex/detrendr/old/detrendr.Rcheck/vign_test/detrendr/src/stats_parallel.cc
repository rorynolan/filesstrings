// [[Rcpp::depends(RcppParallel)]]

#include <algorithm>
#include <cmath>

#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;


double square_root_double(double x) {
  return std::sqrt(x);
}

struct SquareRoot : public Worker
{
  // source matrix
  const RVector<double> input;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  SquareRoot(const NumericVector input, NumericVector output)
    : input(input), output(output) {}

  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, input.begin() + end,
                   output.begin() + begin,
                   square_root_double);
  }
};

// [[Rcpp::export]]
NumericVector square_root_(NumericVector x) {

  // allocate the output
  NumericVector output(x.size());

  // SquareRoot functor (pass input and output)
  SquareRoot squareRoot(x, output);

  // call parallelFor to do the work
  parallelFor(0, x.size(), squareRoot);

  // return the output
  return output;
}
