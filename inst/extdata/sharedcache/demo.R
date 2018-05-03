# timestamp helpers for cache/indicator/data functions to follow

#' formats the modified-time of a file into a character string
record_mtime <- function(filename) {
  POSIX2char(file.mtime(filename))
}
#' reads a timestamp from an .ind file into POSIXct
read_time <- function(indfile) {
  char2POSIX(readLines(indfile))
}

#' Create length-3 vector of cache, indicator, and data filenames
#' 
#' For shared cache: given any name ending in .cache, .ind, or something else,
#' categorizes the name and finds matching versions of .cache, .ind, and data
#' filenames
expand_names <- function(filename) {
  # the result will be a named vector
  allnames <- list()
  
  # determine the name of the datafile
  if(grepl('\\.cache$', filename)) allnames$data <- gsub('.cache', '', filename)
  else if(grepl('\\.ind$', filename)) allnames$data <- gsub('.ind', '', filename)
  else allnames$data <- filename
  
  allnames$cache <- paste0(allnames$data, '.cache')
  allnames$indicator <- paste0(allnames$data, '.ind')
  allnames$var <- tools::file_path_sans_ext(allnames$data)
  
  return(allnames)
}


# Mock functions for checking, pulling, and pushing to cache, and processing a
# file

#' get a hash of the cache file. for an S3 cache, this code could be replaced by
#' S3's ETag (an MD5 sum, with special treatment for multi-part uploads,
#' supplied in list_s3)
get_hash <- function(fname) {
  unname(tools::md5sum(fname))
}

# make_file must be defined in both helper-demo.R and extdata/sharedcache/demo.R
# because we need it both places, remake only sees one
make_file <- function(fname, ftext='', ftstamp=Sys.time()) { # "2017-09-05 07:00:00 MST"
  readr::write_lines(ftext, fname)
  system(sprintf('touch -d "%s" %s', POSIX2char(ftstamp), fname))
  invisible(NULL)
}

make_cache_indicator <- function(indfile) {
  fnames <- expand_names(indfile)
  if(!file.exists(fnames$cache)) stop(paste(fnames$data, "is missing from the cache"))
  message("note ", fnames$var)
  
  # write the indicator file
  sc_indicate(fnames$indicator, data_file=fnames$cache)
  invisible(NULL)
}

get_cached_file <- function(indfile) {
  # compute names of the cache and data files corresponding to the indfile indicator file
  fnames <- expand_names(indfile)
  
  # check the integrity of the cache relative to the indicator file
  if(!file.exists(fnames$cache)) stop(paste0("despite ", fnames$indicator, ", missing ", fnames$cache))
  hash_cache <- get_hash(fnames$cache)
  hash_ind <- yaml::yaml.load_file(fnames$indicator)$hash
  if(hash_cache != hash_ind) stop(paste0("despite ", fnames$indicator, ", badhash ", fnames$cache))
  
  # update iff the fnames$data needs to be updated. if the cache were remote, we
  # could look at a timestamp in indfile as an indicator of the cache status
  if(!file.exists(fnames$data) || get_hash(fnames$data) != hash_cache) {
    message("get ", fnames$var)
    file.copy(fnames$cache, fnames$data, overwrite=TRUE)
  } else {
    message("noget ", fnames$var)
  }
  invisible(NULL)
}

cache_file <- function(datafile) {
  fnames <- expand_names(datafile)
  message(sprintf("cache %s", fnames$var))
  file.copy(fnames$data, fnames$cache, overwrite=TRUE)
  invisible(NULL)
}

pretend_process <- function(in_ind, out_ind) {
  # we'll need a get_cached_file call in any function that [remake]depends on an
  # indicator file (nearly all functions)
  in_var <- readLines(sc_retrieve(in_ind))
  
  # identify the names for the output
  fnames <- expand_names(out_ind)
  
  # create the actual data file
  message(sprintf("make %s", fnames$var))
  indat <- paste0(in_var, "B", R)
  saveRDS(indat, fnames$data)
  
  # cache the file
  cache_file(fnames$data)
  
  # make the indicator file
  make_cache_indicator(fnames$indicator)
  
  # return something arbitrary
  invisible(NULL)
}

do_something <- function(obj) {
  # actually don't do anything for now
  invisible(NULL)
}
