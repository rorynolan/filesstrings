#include <Rcpp.h>
using namespace Rcpp;


double sign(double x) {
  if (x < 0) {
    return -1;
  } else {
    return 1;
  }
}

//' Smooth and median filters with options for handling NAs.
//'
//' These are alternatives to
//' `EBImage::filter2()` and `EBImage::medianFilter()` for
//' smooth and median filtering respectively. These functions have many options
//' for dealing with \code{NA} values which \code{EBImage}'s functions lack.
//'
//' The behavior at image boundaries is such as the source image has been padded
//' with pixels whose values equal the nearest border pixel value.
//'
//' @param mat A matrix (representing an image).
//' @param size An integer; the median filter radius.
//' @param na_rm Should \code{NA}s be ignored?
//' @param na_count If this is TRUE, in each median calculation, if the majority
//' of arguments are \code{NA}s, \code{NA} is returned but if the \code{NA}s are
//' in the minority, they are ignored as in \code{median(x, na.rm = TRUE)}.
//'
//' @return A matrix (the median filtered image).
//'
//' @examples
//' m <- matrix(1:9, nrow = 3)
//' m[2:3, 2:3] <- NA
//' print(m)
//' median_filter(m)
//' median_filter(m, na_rm = TRUE)
//' median_filter(m, na_count = TRUE)
//'
//' @export
// [[Rcpp::export]]
NumericMatrix median_filter(NumericMatrix mat, int size = 1,
                            bool na_rm = false, bool na_count = false) {
  int nr = mat.nrow();
  int nc = mat.ncol();
  NumericMatrix median_filtered(nr, nc);
  int square_side_len = 2 * size + 1;
  NumericMatrix square(square_side_len, square_side_len);
  int row;
  int col;
  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      for (int k = -size; k <= size; k++) {
        for (int l = -size; l <= size; l++) {
          row = i + k;
          col = j + l;
          /* The behavior at image boundaries is such as the source image
          has been padded with pixels whose values equal the nearest
          border pixel value. */
          while (row < 0 || row >= nr) {
            row -= sign(row);
          }
          while (col < 0 || col >= nc) {
            col -= sign(col);
          }
          square(k + size, l + size) = mat(row, col);
        }
      }
      double filtered_ij;
      if (na_count) {
        if (sum(is_na(square)) > square_side_len * square_side_len / 2.0)
          filtered_ij = NA_REAL;
        else
          filtered_ij = median(square, true);
      }
      else
        filtered_ij = median(square, na_rm);
      median_filtered(i, j) = filtered_ij;
    }
  }
  return median_filtered;
}

//' @rdname median_filter
//'
//' @examples
//' smooth_filter(m)
//' smooth_filter(m, na_rm = TRUE)
//' smooth_filter(m, na_count = TRUE)
//'
//' @export
// [[Rcpp::export]]
NumericMatrix smooth_filter(NumericMatrix mat, int size = 1,
                            bool na_rm = false, bool na_count = false) {
  int nr = mat.nrow();
  int nc = mat.ncol();
  NumericMatrix smoothed(nr, nc);
  int square_side_len = 2 * size + 1;
  int square_size = square_side_len * square_side_len;
  NumericMatrix square(square_side_len, square_side_len);
  int row;
  int col;
  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      for (int k = -size; k <= size; k++) {
        for (int l = -size; l <= size; l++) {
          row = i + k;
          col = j + l;
          /* The behavior at image boundaries is such as the source image
           has been padded with pixels whose values equal the nearest
           border pixel value. */
          while (row < 0 || row >= nr) {
            row -= sign(row);
          }
          while (col < 0 || col >= nc) {
            col -= sign(col);
          }
          square(k + size, l + size) = mat(row, col);
        }
      }
      double filtered_ij;
      if (na_count) {
        if (sum(is_na(square)) > square_size / 2.0)
          filtered_ij = NA_REAL;
        else {
          NumericVector no_nas(sum(!is_na(square)));
          int j = 0;
          for (int i = 0; i < square_size; i++) {
            if (!is_na(square)[i]) {
              no_nas[j] = square[i];
              j++;
            }
          }
          filtered_ij = mean(no_nas);
        }
      }
      else if (na_rm) {
        NumericVector no_nas(sum(!is_na(square)));
        int j = 0;
        for (int i = 0; i < square_size; i++) {
          if (!is_na(square)[i]) {
            no_nas[j] = square[i];
            j++;
          }
        }
        filtered_ij = mean(no_nas);
      }
      else
        filtered_ij = mean(square);
      smoothed(i, j) = filtered_ij;
    }
  }
  return smoothed;
}
