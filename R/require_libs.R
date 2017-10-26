#' Ensure the listed libraries are available
#'
#' Calls requireNamespace for the packages listed in ..., throwing errors if any
#' package is unavailable.
#'
#' @param ... character-string names of packages to require. argument names are
#'   ignored.
require_libs <- function(...) {
  libs <- list(...)
  for(i in seq_along(libs)) {
    if(!requireNamespace(libs[[i]], quietly=TRUE)) {
      stop(paste0("library ", libs[[i]], " required but unavailable"))
    }
  }
}
