#ifndef DETRENDR_PILLAR_UTILS_
#define DETRENDR_PILLAR_UTILS_

#include <vector>

#include <RcppParallel.h>

using namespace RcppParallel;

template <class T, class Vec, class VecInt>
std::vector<T> extract_pillar(const Vec& arr3d,
                              const VecInt& arr3d_dim,
                              const std::size_t p) {
  typedef typename std::vector<T> vT;
  typedef typename std::vector<T>::size_type vTst;
  std::size_t nrow = arr3d_dim[0], ncol = arr3d_dim[1];
  std::size_t pillar_len = arr3d_dim[2];
  vT pillar(pillar_len);
  std::size_t row = p % nrow;
  std::size_t col = p / nrow;
  for (vTst slice = 0; slice != pillar_len; ++slice) {
    pillar[slice] = arr3d[slice * ncol * nrow +
                          col * nrow +
                          row];
  }
  return pillar;
}


template <class T, class Vec, class VecInt>
void assign_pillar(Vec& arr3d,
                   const VecInt& arr3d_dim,
                   const std::vector<T>& pillar,
                   const std::size_t p) {
  typedef typename std::vector<T>::size_type vTst;
  std::size_t nrow = arr3d_dim[0], ncol = arr3d_dim[1];
  std::size_t pillar_len = arr3d_dim[2];
  std::size_t row = p % nrow;
  std::size_t col = p / nrow;
  for (vTst slice = 0; slice != pillar_len; ++slice) {
    arr3d[slice * ncol * nrow +
          col * nrow +
          row] =
      pillar[slice];
  }
}

#endif  // DETRENDR_PILLAR_UTILS_
