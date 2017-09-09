make_file <- function(fname, ftext='', ftstamp=Sys.time()) { # "2017-09-05 07:00:00 MST"
  writeLines(ftext, con=fname)
  system(sprintf('touch -d "%s" %s', POSIX2char(ftstamp), fname))
  invisible(NULL)
}

setup_demo <- function(remote_target='B.rds') {
  dirinfo <- list(
    wd = getwd(),
    newdir = file.path(tempdir(), format(Sys.time(), '%y%m%d_%H%M%S')))
  dirinfo$local <- file.path(dirinfo$newdir, 'local')
  dirinfo$remote <- file.path(dirinfo$newdir, 'remote')
  dir.create(dirinfo$newdir)
  dir.create(dirinfo$local)
  dir.create(dirinfo$remote)
  
  # copy/create core project files
  setwd(dirinfo$remote)
  file.copy(system.file('extdata/sharedcache/remake.yml', package='scipiper'), 'remake.yml')
  file.copy(system.file('extdata/sharedcache/demo.R', package='scipiper'), 'demo.R')
  make_file("R.R", "R <- 1")
  make_file("A.txt.cache", "A1")
  
  # make the project in the remote repo
  if(!is.na(remote_target)) capture_make(remote_target)
  
  # git pull all the committable files (not data files or .remake) into the local repo
  git_pull(dirinfo)
  
  # put us in the local dir to help pretend that's our repo
  setwd(dirinfo$local)
  
  return(dirinfo)
}

develop_remote_A2 <- function(dirinfo, remote_target='B.rds') {
  setwd(dirinfo$remote)
  
  # in this scenario we are updating A.txt.cache to contain 'A2' instead of 'A1'
  make_file("A.txt.cache", "A2")
  
  # simply updating the cached file is not enough, because remake will assume
  # the cached file is unchanged until told otherwise.
  scdel("A.txt.st", verbose=FALSE)
  
  # make the project in the remote repo
  if(!is.na(remote_target)) capture_make(remote_target)
  
  # git pull all the committable files (not data files or .remake) into the local repo
  git_pull(dirinfo)
  setwd(dirinfo$local)
}

develop_local_R3 <- function(dirinfo, local_target='B.rds') {
  setwd(dirinfo$local) # make sure
  
  make_file("R.R", "R <- 3")
  
  # make the project in the local repo
  if(!is.na(local_target)) capture_make(local_target)
}

cleanup_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$newdir, recursive=TRUE)
}

# a fragile mock of git pull for testing scenarios
git_pull <- function(dirinfo) {
  # list all the files except those in .remake
  from <- dir(dirinfo$remote, full.names=TRUE, recursive=TRUE, include.dirs = TRUE)
  to <- gsub(dirinfo$remote, dirinfo$local, from, fixed=TRUE)
  
  to_dirs <- to[dir.exists(from)]
  are_files <- !dir.exists(from) & grepl('\\.(st|cache|R|yml)$', basename(from))
  from_files <- from[are_files]
  to_files <- to[are_files]
  
  # create the needed directories
  lapply(to_dirs, function(todir) if(!dir.exists(todir)) dir.create(todir))
  
  # copy the files
  mapply(function(from, to) {
    if(!file.exists(to) || tools::md5sum(from) != tools::md5sum(to)) {
      return(file.copy(from, to, recursive=dir.exists(from), overwrite=TRUE))
    } else {
      return(NA)
    }
  }, from_files, to_files, USE.NAMES=FALSE)
}

# captures output from a non-verbose scmake, removes '\n's from outputs, and
# collapses vector to character with '; ' for easy comparison to an expected
# build sequence
capture_make <- function(target) {
  options('remake.verbose'=FALSE, 'remake.verbose.noop'=FALSE, 'remake.verbose.command'=FALSE, 'remake.verbose.command.abbreviate'=FALSE)
  rawmsg <- capture_messages(scmake(target, verbose=FALSE))
  options('remake.verbose'=NULL, 'remake.verbose.noop'=NULL, 'remake.verbose.command'=NULL, 'remake.verbose.command.abbreviate'=NULL)
  msg <- paste(gsub('\n', '', rawmsg), collapse='; ')
  gsub('[  LOAD ] ; [  READ ] ; ', '', msg, fixed=TRUE)
}
