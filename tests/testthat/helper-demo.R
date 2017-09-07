make_file <- function(fname, ftext='', ftstamp="2017-09-05 07:00:00 MST") { # Sys.time()
  writeLines(ftext, con=fname)
  system(sprintf('touch -d "%s" %s', POSIX2char(ftstamp), fname))
  return(NULL)
}

setup_demo <- function(R) {
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
  make_file("R.R", sprintf("R <- %d", R))
  
  # pretend-create data, cache, and indicator files
  source('demo.R')
  
  
  return(dirinfo)
}

cleanup_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$newdir, recursive=TRUE)
}

capture_make <- function(target) {
  rawmsg <- capture_messages(remake::make(target, verbose=FALSE))
  paste(gsub('\n', '', rawmsg), collapse='; ')
}
