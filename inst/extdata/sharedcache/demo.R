# timestamp helpers for cache/indicator/data functions to follow

#' formats the modified-time of a file into a character string
record_mtime <- function(filename) {
  POSIX2char(file.mtime(filename))
}
#' reads a timestamp from an .st file into POSIXct
read_time <- function(stfile) {
  char2POSIX(readLines(stfile))
}

#' Create length-3 vector of cache, status, and data filenames
#' 
#' For shared cache: given any name ending in .cache, .st, or something else,
#' categorizes the name and finds matching versions of .cache, .st, and data
#' filenames
expand_names <- function(filename) {
  # the result will be a named vector
  allnames <- list()
  
  # determine the name of the datafile
  if(grepl('\\.cache$', filename)) allnames$data <- gsub('.cache', '', filename)
  else if(grepl('\\.st$', filename)) allnames$data <- gsub('.st', '', filename)
  else allnames$data <- filename
  
  allnames$cache <- paste0(allnames$data, '.cache')
  allnames$indicator <- paste0(allnames$data, '.st')
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

make_cache_indicator <- function(stfile) {
  fnames <- expand_names(stfile)
  if(!file.exists(fnames$cache)) stop(paste(fnames$data, "is missing from the cache"))
  message("note ", fnames$var)
  
  # get a hash of the file, so we can check if this indicator file has gone bad
  hash <- get_hash(fnames$cache)
  
  # write the hash to the indicator file and get out of here
  make_file(fnames$indicator, ftext=hash, ftstamp=Sys.time())
  invisible(NULL)
}

get_cached_file <- function(stfile) {
  # compute names of the cache and data files corresponding to the stfile indicator file
  fnames <- expand_names(stfile)
  
  # check the integrity of the cache relative to the status file
  if(!file.exists(fnames$cache)) stop(paste("missing", fnames$cache))
  hash_cache <- get_hash(fnames$cache)
  hash_st <- readLines(fnames$indicator)
  if(hash_cache != hash_st) stop(paste("badhash", fnames$cache))
  
  # update iff the fnames$data needs to be updated. if the cache were remote, we
  # could look at a timestamp in stfile as an indicator of the cache status
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

pretend_process <- function(in_st, out_st, R_R) {
  # we'll need a get_cached_file call in any function that [remake]depends on an
  # indicator file (nearly all functions)
  get_cached_file(stfile=in_st)
  in_var <- readLines(expand_names(in_st)$data)
  
  # read the "script" (it just defines a variable called R)
  source(R_R)
  
  # identify the names for the output
  fnames <- expand_names(out_st)
  
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
