cache_file <- function(datafile) {
  fnames <- expand_names(datafile)
  message(sprintf("cache %s", fnames$var))
  file.copy(fnames$data, fnames$cache, overwrite=TRUE)
  invisible(NULL)
}
