#### task-plan demo ####

setup_tasks_demo <- function() {
  dirinfo <- list(
    wd = getwd(),
    #srcdir = 'inst/extdata/tasks_demo', # for use while developing tests
    srcdir = system.file('extdata/tasks_demo', package='scipiper'),
    tmpdir = file.path(tempdir(), format(Sys.time(), '%y%m%d_%H%M%S')))
  dirinfo$newdir <- file.path(dirinfo$tmpdir, basename(dirinfo$srcdir))
  
  unlink(dirinfo$newdir, recursive=TRUE)
  dir.create(dirinfo$newdir, recursive=TRUE)
  file.copy(dirinfo$srcdir, dirinfo$tmpdir, recursive=TRUE)
  dir(dirinfo$newdir)
  
  setwd(dirinfo$newdir)
  return(dirinfo)
}

cleanup_tasks_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$tmpdir, recursive=TRUE)
}

#### shared-cache demo ####

# make_file must be defined in both helper-demo.R and extdata/sharedcache/demo.R
# because we need it both places, remake only sees one
make_file <- function(fname, ftext='', ftstamp=Sys.time()) { # "2017-09-05 07:00:00 MST"
  readr::write_lines(ftext, fname)
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
  
  return(dirinfo)
}

develop_remote_A2 <- function(dirinfo, remote_target='B.rds', rdelete_target='A.txt.ind', remove=NA) {
  setwd(dirinfo$remote)
  
  # in this scenario we are updating A.txt.cache to contain 'A2' instead of 'A1'
  make_file("A.txt.cache", "A2")
  
  # simply updating the cached file is not enough, because remake will assume
  # the cached file is unchanged until told otherwise.
  if(length(rdelete_target) > 1 || !is.na(rdelete_target)) scdel(rdelete_target, verbose=FALSE)
  
  # make the project in the remote repo
  if(!is.na(remote_target)) capture_make(remote_target)
  
  # delete more files if requested
  if(length(remove) > 1 || !is.na(remove)) file.remove(remove)
  
  # git pull all the committable files (not data files or .remake) into the local repo
  git_pull(dirinfo)
}

develop_local_R3 <- function(dirinfo) {
  setwd(dirinfo$local) # make sure
  make_file("R.R", "R <- 3")
}

# a fragile mock of git pull for testing scenarios
git_pull <- function(dirinfo) {
  # list all the files except those in .remake
  from <- dir(dirinfo$remote, full.names=TRUE, recursive=TRUE, include.dirs = TRUE)
  to <- gsub(dirinfo$remote, dirinfo$local, from, fixed=TRUE)
  
  to_dirs <- to[dir.exists(from)]
  are_files <- !dir.exists(from) & grepl('\\.(ind|cache|R|yml)$', basename(from))
  from_files <- from[are_files]
  to_files <- to[are_files]
  gone_files <- file.path(
    dirinfo$local, 
    setdiff(dir(dirinfo$local, recursive=TRUE, include.dirs = TRUE),
            dir(dirinfo$remote, recursive=TRUE, include.dirs = TRUE)))
  
  # create the needed directories
  lapply(to_dirs, function(todir) if(!dir.exists(todir)) dir.create(todir))
  
  # copy the changed files. uses hashes instead of timetstamps so that tests can
  # be quick (otherwise need to wait >= 1 sec between changing & pulling)
  mapply(function(from, to) {
    if(!file.exists(to) || tools::md5sum(from) != tools::md5sum(to)) {
      return(file.copy(from, to, recursive=dir.exists(from), overwrite=TRUE))
    } else {
      return(NA)
    }
  }, from_files, to_files, USE.NAMES=FALSE)
  
  # delete the gone files
  file.remove(gone_files)
  
  # put us in the local dir to help pretend that's our repo
  setwd(dirinfo$local)
}

# captures output from a non-verbose scmake, removes '\n's from outputs, and
# collapses vector to character with '; ' for easy comparison to an expected
# build sequence
capture_make <- function(target) {
  options('remake.verbose'=FALSE, 'remake.verbose.noop'=FALSE, 'remake.verbose.command'=FALSE, 'remake.verbose.command.abbreviate'=FALSE)
  rawmsg <- testthat::capture_messages(scmake(target, verbose=FALSE))
  options('remake.verbose'=NULL, 'remake.verbose.noop'=NULL, 'remake.verbose.command'=NULL, 'remake.verbose.command.abbreviate'=NULL)
  msg <- paste(gsub('\n', '', rawmsg), collapse='; ')
  gsub('[  LOAD ] ; [  READ ] ; ', '', msg, fixed=TRUE)
}

inspect_local <- function(dirinfo) {
  setwd(dirinfo$local)
  files <- dir(pattern=c('^(A|B|R)'))
  contents <- sapply(files, function(file) {
    switch(
      file,
      A.txt.cache = , A.txt = readLines(file),
      B.rds.cache = , B.rds = readRDS(file),
      R.R = gsub('R <- ', '', readLines(file)),
      A.txt.ind = , B.rds.ind = paste0('[', c('dc7cf3'='A1', '35775d'='A2', 'cb4bb7'='A1B1', 'e556ae'='A2B1', '6ec33b'='A1B3')[[
        substring(yaml::yaml.load_file(file)$hash, 1, 6)]], ']')
    )
  })
  targets <- c('A.txt.cache','A.txt.ind','A.txt','R.R','B.rds.cache','B.rds.ind','B.rds')
  all <- setNames(contents[targets], targets)
  all[is.na(all)] <- '---'
  return(all)
}

cleanup_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$newdir, recursive=TRUE)
}
