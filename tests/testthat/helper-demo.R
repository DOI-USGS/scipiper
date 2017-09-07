make_file <- function(fname, ftext='', ftstamp=Sys.time()) { # "2017-09-05 07:00:00 MST"
  writeLines(ftext, con=fname)
  system(sprintf('touch -d "%s" %s', POSIX2char(ftstamp), fname))
  invisible(NULL)
}

setup_demo <- function(Ac=1, Ai=1, Ad=NA, R=1, Bc=1, Bi=1, Bd=NA) {
  dirinfo <- list(
    wd = getwd(),
    newdir = file.path(tempdir(), format(Sys.time(), '%y%m%d_%H%M%S')))
  dir.create(dirinfo$newdir)
  setwd(dirinfo$newdir)
  
  # copy/create core project files
  file.copy('D:/APAData/GitHub/OWI/scipiper/inst/extdata/sharedcache/remake.yml', 'remake.yml')
  file.copy('D:/APAData/GitHub/OWI/scipiper/inst/extdata/sharedcache/demo.R', 'demo.R')
  # file.copy(system.file('extdata/sharedcache/remake.yml', package='scipiper'), 'remake.yml')
  # file.copy(system.file('extdata/sharedcache/demo.R', package='scipiper'), 'demo.R')
  make_file("R.R", "R <- 1")
  make_file("A.txt.cache", "A1")
  
  # pretend-create data, cache, and indicator files
  source('demo.R')
  make_cache_indicator("A.txt.st")
  get_cached_file("A.txt.st")
  pretend_process(in_st="A.txt.st", out_st="B.rds.st", R_R="R.R")
  
  # revise files as requested
  # make_file("A.txt.cache", sprintf("A%d", Ac))
  # make_file("R.R", sprintf("R <- %d", R))
  
  # revise files as requested
  if(is.na(Ac)) file.remove("A.txt.cache")
  if(is.na(Ai)) file.remove("A.txt.st")
  if(is.na(Ad)) file.remove("A.txt")
  if(is.na(R)) file.remove("R.R")
  if(is.na(Bc)) file.remove("B.rds.cache")
  if(is.na(Bi)) file.remove("B.rds.st")
  if(is.na(Bd)) file.remove("B.rds")
  
  return(dirinfo)
}

cleanup_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$newdir, recursive=TRUE)
}

describe_setup <- function() {
  
}

capture_make <- function(target) {
  rawmsg <- capture_messages(remake::make(target, verbose=FALSE))
  paste(gsub('\n', '', rawmsg), collapse='; ')
}
