#' Create standard project directories
#' 
#' @param ... named characters where name=directory_name and 
#'   value=character_codes_for_dirs, or unnamed characters where 
#'   value=directory_name (and `default` will be used for 
#'   character_codes_for_dirs)
#' @param default character code, one letter per subdirectory. Permitted letters
#'   are c=cfg, d=doc, j=job, o=out, and s=src
#' @param .list as an alternative to ..., use .list to specify directories in a
#'   single list argument
#' @import tidyr
#' @import dplyr
#' @export
#' @md
#' @examples
#' # prep
#' dir.create('example_project')
#' setwd('example_project')
#' 
#' setup_dirs()
#' setup_dirs('2_data','3_analyze','4_report', default='cdos')
#' setup_dirs('1a_siteinfo','1b_discharge','2_clean'='dos','3_model'='cjs')
#' 
#' # clean up
#' setwd('..')
#' unlink('example_project', recursive=TRUE)
setup_dirs <- function(..., default='cos', .list=list('1_data'=default, '9_report'=default, 'lib'='')) {
  
  # collect arguments or assign the default
  maindirs <- list(...)
  if(length(maindirs) > 0 && !missing(.list)) stop('specify ... or .list but not both')
  if(length(maindirs) == 0) maindirs <- .list
  
  # tidy args into a named vector of 1-letter subdirectory codes
  dircodes <- sapply(maindirs, function(dir) {
    if(length(dir)!=1) stop('arguments should each have length 1')
    if(!is.character(dir)) stop('arguments should be strings')
    if(is.null(names(dir))) {
      setNames(default, dir)
    } else {
      dir
    }
  })
  # convert names and 1-letter subdirectory codes to full dir and subdir names
  alldirs <- data_frame(dir=names(dircodes), code=unname(dircodes)) %>%
    mutate(codes=strsplit(code, '')) %>%
    unnest() %>%
    mutate(subdir=c(c='cfg', d='doc', j='job', o='out', s='src')[codes]) %>%
    mutate(fulldir = file.path(dir, subdir)) %>%
    pull(fulldir)
  
  # create the directories
  lapply(alldirs, dir.create, recursive=TRUE)
  
  # return a vector of the directory names
  return(alldirs)
}

setup_files <- function(..., .list=c('README.md', 'Makefile')) {
  mainfiles <- list(...)
  if(length(mainfiles) > 0 && !missing(.list)) stop('specify ... or .list but not both')
  if(length(mainfiles) == 0) mainfiles <- .list
  
  lapply(mainfiles, file.create)
  
  return(mainfiles)
}
