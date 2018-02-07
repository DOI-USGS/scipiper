#' Evaluate an expression with retries, waiting between tries
#'
#' Waits progressively longer before each next try (at least by default; wait
#' times can be customized).
#'
#' @param expr expression to evaluate
#' @param max_wait_times vector of maximum wait times, in seconds, before trying
#'   the 1st, 2nd, etc. time. The length of this vector is the maximum number of
#'   tries.
#' @param min_wait_frac numeric between 0 and 1. If < 1, the actual wait time
#'   before try `i` will be a uniform random value selected from the range
#'   `(min_wait_frac*max_wait_times[i], max_wait_times[i])`. Modeled loosely on
#'   https://github.com/r-lib/httr/blob/master/R/retry.R.
retry_patiently <- function(expr, max_wait_times=c(0, 3^c(0:4)), min_wait_frac=0.5, verbose=FALSE) {
  expr <- substitute(expr)
  env <- parent.frame(1)
  
  final_wait_times <- runif(n=length(max_wait_times), min=min_wait_frac*max_wait_times, max=max_wait_times)
  
  i <- 1
  do_retry <- TRUE
  if(verbose) message(sprintf("Trying \"%s\":", deparse(expr)))
  while(do_retry) {
    wait_time <- final_wait_times[i]
    if(verbose) message(sprintf("  Waiting %0.1f seconds before %sevaluating", wait_time, if(i>1) 're-' else ''))
    Sys.sleep(wait_time)
    result <- tryCatch(eval(expr, envir=env), error = function(e) e)
    i <- i + 1
    do_retry <- inherits(result, "error") && i <= length(final_wait_times)
  }
  
  if (inherits(result, "error")) {
    if(verbose) message(sprintf("  All retries used, still erroring on \"%s\"", deparse(expr)))
    stop(result)
  } else {
    return(result)
  }
}
