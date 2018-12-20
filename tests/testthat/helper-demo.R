#### task-plan demo ####

setup_tasks_demo <- function() {
  random_string <- function() {
    random_letters <- sample(LETTERS, 7, TRUE)
    random_numbers <- sample(0:9, 5, TRUE)
    paste(sample(c(random_letters, random_numbers), 10, FALSE), collapse='')
  }
  
  find_srcdir <- function() {
    srcdir <- system.file('inst/extdata/tasks_demo', package='scipiper') # for devtools::test()
    if(srcdir == '') srcdir <- system.file('extdata/tasks_demo', package='scipiper') # for local tests (and R CMD check?)
    return(srcdir)
  }
  
  dirinfo <- list(
    wd = getwd(),
    #srcdir = 'inst/extdata/tasks_demo', # for use while developing tests
    srcdir = find_srcdir(),
    tmpdir = file.path(tempdir(), random_string()))
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
