#' This function replaces ::: to avoid CRAN check errors
#'
#' Code and rationale from
#' https://stat.ethz.ch/pipermail/r-devel/2013-August/067210.html
#'
#' @param pkg character name of the package from which to import the function
#' @param fun character name of the function to import
#' @return an internal function imported from another package (in our case,
#'   probably/always remake)
#' @examples 
#' \dontrun{
#' ('remake' %:::% 'target_is_file')('mytarget.txt')
#' }
#' @keywords internal
use_internal_function <- `%:::%` <- function(pkg, fun) {
  get(fun, envir = asNamespace(pkg), inherits = FALSE)
}
