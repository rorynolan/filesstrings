boxcar_smooth_rows <- function(mat, l, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  boxcar_smooth_rows_(mat, l)
}

boxcar_smooth_pillars <- function(arr3d, l, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  boxcar_smooth_pillars_(arr3d, l)
}

exp_smooth_rows <- function(mat, tau, l, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  exp_smooth_rows_(mat, tau, l)
}

exp_smooth_pillars <- function(arr3d, tau, l, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  exp_smooth_pillars_(arr3d, tau, l)
}
