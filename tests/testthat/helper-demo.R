#### task-plan demo ####

setup_tasks_demo <- function(demo_dir='tasks_demo') {
  
  find_srcdir <- function(demo_dir) {
    if(basename(normalizePath('.')) == 'scipiper') {
      srcdir <- file.path('inst/extdata', demo_dir) # for local+fast demo development
    } else {
      srcdir <- system.file(file.path('inst/extdata', demo_dir), package='scipiper') # for devtools::test()
      if(srcdir == '') srcdir <- system.file(file.path('extdata', demo_dir), package='scipiper') # for local tests (and R CMD check?)
    }    
    return(srcdir)
  }
  
  dirinfo <- list(
    wd = getwd(),
    srcdir = find_srcdir(demo_dir),
    tmpdir = tempfile())
  dirinfo$newdir <- file.path(dirinfo$tmpdir, basename(dirinfo$srcdir))
  
  unlink(dirinfo$newdir, recursive=TRUE)
  dir.create(dirinfo$newdir, recursive=TRUE)
  file.copy(dirinfo$srcdir, dirinfo$tmpdir, recursive=TRUE)
  
  setwd(dirinfo$newdir)
  return(dirinfo)
}

cleanup_tasks_demo <- function(dirinfo) {
  setwd(dirinfo$wd)
  unlink(dirinfo$tmpdir, recursive=TRUE)
}
