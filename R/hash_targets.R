#' Create a vector of hashes corresponding to a vector of targets
#'
#' @param target_names character vector of target names
#' @param target_is_file logical vector, same length as target_names, with TRUE
#'   for targets that are files and FALSE for targets that are objects
#' @param file_extensions if target_is_file is not specified, file_extensions
#'   can be used to supplement the default remake::file_extensions() to infer
#'   which targets are files from the target_names. If file_extensions=NULL,
#'   only the remake defaults will be used
#' @importFrom stats setNames
#' @importFrom digest digest
hash_targets <- function(target_names, target_is_file, file_extensions=NULL) {
  # allocate the named vector of hashes
  hashes <- character(length(target_names)) %>%
    setNames(target_names)
  
  # hash files and objects separately
  is_file <- if(!missing(target_is_file)) {
    target_is_file
  } else {
    ('remake' %:::% 'target_is_file')(target_names, file_extensions = c(remake::file_extensions(), file_extensions))
  }
  hashes[is_file] <- tools::md5sum(target_names[is_file])
  
  is_object <- !is_file
  hashes[is_object] <- digest::digest(target_names[is_object], algo='md5')
  
  # return the named vector of hashes
  return(hashes)
}

#' Hash the depends objects of a target
#'
#' Returns a named vector of hashes for all the dependencies of the target
#'
#' @param target_name the name of the single target whose dependencies should be
#'   hashed
#' @param remake_file the name of the remake file where `target_name`'s recipe
#'   is given
#' @export
hash_dependencies <- function(target_name, remake_file) {
  makefile <- ('remake' %:::% 'remake')(remake_file, verbose=FALSE, load_sources=FALSE, allow_missing_packages=TRUE)
  deps <- makefile$targets[[target_name]]
  hashes <- hash_targets(deps$depends_name, target_is_file=unname(deps$depends_type=='file'))
  return(hashes)
}
