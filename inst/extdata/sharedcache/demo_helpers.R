# timestamp helpers for cache/indicator/data functions to follow

#' formats a POSIXct timestamp with UTC into a character string
POSIX2char <- function(ptime) {
  format(ptime, tz='UTC', format="%Y-%m-%d %H:%M:%S", usetz=TRUE)
}
char2POSIX <- function(stime) {
  as.POSIXct(strptime(stime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
}

#' formats the modified-time of a file into a character string
record_mtime <- function(filename) {
  POSIX2char(file.mtime(filename))
}
#' reads a timestamp from an .st file into POSIXct
read_time <- function(stfile) {
  char2POSIX(readLines(stfile))
}

# filename helper

expand_names <- function(filename) {
  # the result will be a named vector
  allnames <- list()
  
  # determine the name of the datafile
  if(grepl('\\.cache$', filename)) allnames$data <- gsub('.cache', '', filename)
  else if(grepl('\\.st$', filename)) allnames$data <- gsub('.st', '', filename)
  else allnames$data <- filename
  
  allnames$cache <- paste0(allnames$data, '.cache')
  allnames$indicator <- paste0(allnames$data, '.st')
  
  return(allnames)
}


# Mock functions for checking, pulling, and pushing to cache, and processing a
# file

make_cache_indicator <- function(stfile) {
  fnames <- expand_names(stfile)
  if(!file.exists(fnames$cache)) stop(paste(fnames$data, "is missing from the cache"))
  message("creating ", fnames$indicator, " because found ", fnames$data, " in cache")
  make_file(fnames$indicator, ftext=record_mtime(fnames$cache), ftstamp=Sys.time())
  return('ran make_cache_indicator')
}

get_cached_file <- function(stfile) {
  # compute names of the cache and data files corresponding to the stfile indicator file
  fnames <- expand_names(stfile)
  
  # update iff the fnames$data needs to be updated. if the cache were remote, we
  # could look at a timestamp in stfile as an indicator of the cache status
  if(!file.exists(fnames$data) || file.mtime(fnames$data) < read_time(stfile)) {
    message("pulling ", fnames$data, " from cache")
    if(!file.exists(fnames$cache)) stop(paste(fnames$data, "is missing from the cache"))
    file.copy(fnames$cache, fnames$data)
  } else {
    message("no need to pull ", fnames$data)
  }
  return('ran get_cached_file')
}

cache_file <- function(datafile) {
  fnames <- expand_names(datafile)
  message(sprintf("caching %s", fnames$data))
  file.copy(fnames$data, fnames$cache)
}

pretend_process <- function(in_st, out_st, obj) {
  # we'll need a get_cached_file call in any function that [remake]depends on an
  # indicator file (nearly all functions)
  get_cached_file(stfile=in_st)
  
  # identify the names for the output
  fnames <- expand_names(out_st)
  
  # create the actual data file
  message(sprintf("creating %s from %s and obj", fnames$data, in_st))
  indat <- c(readLines(in_st), "and did some processing")
  saveRDS(indat, fnames$data)
  
  # cache the file
  cache_file(fnames$data)
  
  # make the indicator file
  make_cache_indicator(fnames$indicator)
  
  # return something arbitrary
  return('ran pretend_process')
}


# Helpers for creating scenarios

make_file <- function(fname, ftext='', ftstamp=Sys.time()) {
  writeLines(ftext, con=fname)
  system(sprintf('touch -d "%s" %s', POSIX2char(ftstamp), fname))
  return(NULL)
}

scenario <- function(scenario_code) {
  remake::make('clean', verbose=FALSE) # 'tidy' removes objects; 'clean' removes objects & files
  suppressWarnings(file.remove(dir(pattern='.cache')))
  switch(
    scenario_code,
    a1 = {
      make_file("A.txt.cache",          "data for A.txt", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "2017-08-25 08:00:00 UTC", "2017-08-25 08:00:00 UTC")
      message("make B.rds should pull A.txt, make B.rds/.cache/.st, not re-pull B.rds")
    },
    a2 = {
      make_file("A.txt.cache",          "data for A.txt", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "2017-08-25 08:00:00 UTC", "2017-08-25 08:00:00 UTC")
      make_file("B.rds",                            "10", "2017-08-25 10:00:00 UTC")
      message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
    },
    a3 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
      message("make B.rds should pull B.rds")
    },
    a4 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt",      "11", "2017-08-25 11:00:00 UTC")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
      message("make B.rds should pull B.rds from cache")
    },
    a5 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds",                            "10", "2017-08-25 07:00:00 UTC")
      message("make B.rds should pull B.rds from cache")
    },
    a6 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds",      "10", "2017-08-25 10:00:00 UTC")
      message("make B.rds should do nothing")
    },
    a7 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
      make_file("A.txt",       "8", "2017-08-25 08:00:00 UTC")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
      make_file("B.rds",      "10", "2017-08-25 10:00:00 UTC")
      message("make B.rds should do nothing")
    },
    b1 = {
      
      message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
    },
    b2 = {
      
      message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
    },
    b3 = {
      
      message("make B.rds should make B.rds/.st/.cache, not re-pull B.rds")
    },
    b4 = {
      
      message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
    },
    c1 = {
      
      message("make B.rds should give error about missing A.txt.cache")
    },
    c2 = {
      
      message("make B.rds should give error about missing A.txt.cache")
    },
    c3 = {
      
      message("make B.rds should give error about missing A.txt.cache")
    },
    c4 = {
      
      message("make B.rds should give error about missing A.txt.cache")
    }
  )
}

