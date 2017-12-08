#' Create standard project directories
#' 
#' @param ... named characters where name=directory_name and 
#'   value=character_codes_for_dirs, or unnamed characters where 
#'   value=directory_name (and `default` will be used for 
#'   character_codes_for_dirs). Permitted letters for character_codes_for_dirs 
#'   are c=cfg, d=doc, j=job, o=out, and s=src
#' @param default string identifying the default set of
#'   character_codes_for_dirs. one letter per subdirectory. Permitted letters 
#'   are c=cfg, d=doc, j=job, l=log, o=out, and s=src
#' @param .list as an alternative to ..., use .list to specify directories in a 
#'   single list argument
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' # prep
#' dir.create('example_project')
#' setwd('example_project')
#' 
#' setup_dirs()
#' setup_dirs('2_data','3_analyze','4_report','explore'='',default='cdos')
#' setup_dirs('1a_siteinfo','1b_discharge','2_clean'='dos','3_model'='cjs','explore'='')
#' 
#' # clean up
#' setwd('..')
#' unlink('example_project', recursive=TRUE)
setup_dirs <- function(..., default='cos', .list=list('1_data'=default, '9_report'=default, 'lib'='')) {
  
  code <- codes <- subdir <- fulldir <- '.dplyr.var'
  
  # collect arguments or assign the default
  maindirs <- list(...)
  if(length(maindirs) > 0 && !missing(.list)) stop('specify ... or .list but not both')
  if(length(maindirs) == 0) maindirs <- .list
  
  # tidy args into a named vector of 1-letter subdirectory codes
  dircodes <- sapply(seq_along(maindirs), function(i) {
    maindir <- names(maindirs)[[i]]
    subdirs <- maindirs[[i]]
    if(length(subdirs)!=1) stop('arguments should each have length 1')
    if(!is.character(subdirs)) stop('arguments should be strings')
    if(is.null(maindir) || maindir=='') {
      stats::setNames(default, subdirs)
    } else {
      stats::setNames(subdirs, maindir)
    }
  })
  # convert names and 1-letter subdirectory codes to full dir and subdir names
  codekey <- c(c='cfg', d='doc', j='job', l='log', o='out', s='src', '0'='')
  alldirs <- data_frame(dir=names(dircodes), code=unname(dircodes)) %>%
    mutate(code=ifelse(code=='', '0', code),
           codes=strsplit(code, '')) %>%
    tidyr::unnest() %>%
    mutate(
      allgood=if(!all(codes %in% names(codekey))) {
        stop(paste("unexpected code; characters must be among", paste(names(codekey), collapse=',')))
      } else TRUE) %>%
    mutate(subdir=codekey[codes],
           fulldir = file.path(dir, subdir)) %>%
    pull(fulldir)
  
  # create the directories
  lapply(alldirs, dir.create, recursive=TRUE)
  
  # return a vector of the directory names
  return(alldirs)
}

#' Create standard project directories
#' 
#' @param ... named characters where name=directory_name and value=filename[s], 
#'   or unnamed characters where value=filepath[s]. Paths can be relative.
#' @param .list as an alternative to ..., use .list to specify files in a single
#'   list argument
#' @export
#' @examples
#' # prep
#' dir.create('example_project')
#' setwd('example_project')
#' 
#' dirs <- setup_dirs('2_data','3_analyze','4_report','explore',default='cdos')
#' setup_files() # use the default as given for .list
#' setup_files('2_data/src'=c('data1.R','data2.R','helper-data.R'))
#' 
#' # clean up
#' setwd('..')
#' unlink('example_project', recursive=TRUE)
setup_files <- function(..., .list=list(file.path(grep('/out$', list.dirs('.'), value=TRUE), 'README.md'), '.'=c('README.md','Makefile'))) {
  fileargs <- list(...)
  if(length(fileargs) > 0 && !missing(.list)) stop('specify ... or .list but not both')
  if(length(fileargs) == 0) fileargs <- .list
  
  # combine paths as needed to get a 1D vector of all files to create
  fullfiles <- unlist(lapply(seq_along(fileargs), function(i) {
    dirname <- names(fileargs)[[i]]
    filenames <- fileargs[[i]]
    if(!is.character(filenames)) stop('arguments should be strings')
    if(is.null(dirname) || dirname == '') {
      filenames
    } else {
      file.path(dirname, filenames)
    }
  }))
  
  # create any files that don't already exist (don't overwrite existing)
  lapply(fullfiles, function(file) {
    if(!file.exists(file)) file.create(file)
  })
  
  # return a vector of the file names
  return(fullfiles)
}
