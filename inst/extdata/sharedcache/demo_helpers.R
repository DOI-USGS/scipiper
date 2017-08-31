# Mock functions for checking, pulling, and pushing to cache, and processing a
# file

make_cache_indicator <- function(indfile) {
  datafile <- gsub('.st', '', indfile)
  cachefile <- paste0(datafile, '.cache')
  if(!file.exists(cachefile)) stop(paste(datafile, "is missing from the cache"))
  message("creating ", indfile, " because found ", datafile, " in cache")
  make_file(indfile, ftext=paste(datafile, "is in the cache"), ftstamp=Sys.time())
  return('ran make_cache_indicator')
}

get_cached_file <- function(datafile) {
  cachefile <- paste0(datafile, '.cache')
  if(!file.exists(datafile) || file.mtime(datafile) < file.mtime(cachefile)) {
    message("pulling ", datafile, " from cache")
    if(!file.exists(cachefile)) stop(paste(datafile, "is missing from the cache"))
    file.copy(cachefile, datafile)
  } else {
    message("no need to pull ", datafile)
  }
  return('ran get_cached_file')
}

cache_file <- function(datafile) {
  message(sprintf("caching %s", datafile))
  cachefile <- paste0(datafile, '.cache')
  file.copy(datafile, cachefile)
}

pretend_process <- function(infile, indfile) {
  datafile <- gsub('.st', '', indfile)
  message(sprintf("creating %s from %s", datafile, infile))
  indat <- c(readLines(infile), "and did some processing")
  saveRDS(indat, datafile)
  cache_file(datafile)
  make_cache_indicator(indfile)
  return('ran pretend_process')
}


# Helpers for creating scenarios

make_file <- function(fname, ftext='', ftstamp=Sys.time()) {
  writeLines(ftext, con=fname)
  system(sprintf('touch -d "%s" %s', format(as.POSIXct(ftstamp), "%Y-%m-%d %H:%M:%S"), fname))
  return(NULL)
}

scenario <- function(scenario_code) {
  remake::make('clean', verbose=FALSE) # 'tidy' removes objects; 'clean' removes objects & files
  suppressWarnings(file.remove(dir(pattern='.cache')))
  switch(
    scenario_code,
    a1 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      message("make B.rds should pull A.txt, make B.rds/.cache/.st, not re-pull B.rds")
    },
    a2 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("B.rds",      "10", "2017-08-25 10:00:00 -0700")
      message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
    },
    a3 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 -0700")
      message("make B.rds should pull B.rds")
    },
    a4 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt",      "11", "2017-08-25 11:00:00 -0700")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 -0700")
      message("make B.rds should pull B.rds from cache")
    },
    a5 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds",       "7", "2017-08-25 07:00:00 -0700")
      message("make B.rds should pull B.rds from cache")
    },
    a6 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds",      "10", "2017-08-25 10:00:00 -0700")
      message("make B.rds should do nothing")
    },
    a7 = {
      make_file("A.txt.cache", "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt.st",    "8", "2017-08-25 08:00:00 -0700")
      make_file("A.txt",       "8", "2017-08-25 08:00:00 -0700")
      make_file("B.rds.cache", "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds.st",    "9", "2017-08-25 09:00:00 -0700")
      make_file("B.rds",      "10", "2017-08-25 10:00:00 -0700")
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

# Helpers for inspecting remake

get_status <- function(target='B.rds') {
  m <- remake:::remake()
  g <- remake:::remake_dependency_graph(m)
  remake:::remake_status(m, target, g)
  # remake_status doesn't actually get used by remake::make, and the code is
  # full of caveats that make me wonder if they believe it...but it's the
  # nearest thing to a current status report that I've found so far
}
