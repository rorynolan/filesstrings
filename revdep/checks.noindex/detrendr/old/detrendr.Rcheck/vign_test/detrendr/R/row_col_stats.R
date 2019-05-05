brightness_rows <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_rows_(mat)
}

brightness_cols <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_cols_(mat)
}

brightness_rows_given_mean <- function(mat, means, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_rows_given_mean_(mat, means)
}

brightness_cols_given_mean <- function(mat, means, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_cols_given_mean_(mat, means)
}

mean_rows <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  mean_rows_(mat)
}

mean_cols <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  mean_cols_(mat)
}

var_rows_given_mean <- function(mat, means, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  var_rows_given_mean_(mat, means)
}

var_cols_given_mean <- function(mat, means, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  var_cols_given_mean_(mat, means)
}

sum_rows <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  sum_rows_(mat)
}

sum_cols <- function(mat, parallel = FALSE) {
  checkmate::assert_numeric(mat)
  checkmate::assert_matrix(mat)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  sum_cols_(mat)
}
