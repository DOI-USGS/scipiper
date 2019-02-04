#### task-plan demo ####

setup_tasks_demo <- function() {
  
  find_srcdir <- function() {
    srcdir <- system.file('inst/extdata/tasks_demo', package='scipiper') # for devtools::test()
    if(srcdir == '') srcdir <- system.file('extdata/tasks_demo', package='scipiper') # for local tests (and R CMD check?)
    return(srcdir)
  }
  
  dirinfo <- list(
    wd = getwd(),
    srcdir = find_srcdir(),
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
