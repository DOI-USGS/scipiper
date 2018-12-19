#### task-plan demo ####

setup_tasks_demo <- function() {
  dirinfo <- list(
    wd = getwd(),
    #srcdir = 'inst/extdata/tasks_demo', # for use while developing tests
    srcdir = system.file('inst/extdata/tasks_demo', package='scipiper'),
    tmpdir = file.path(tempdir(), format(Sys.time(), '%y%m%d_%H%M%S')))
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
