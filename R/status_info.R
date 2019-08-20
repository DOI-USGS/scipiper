#' Get a list of all targets in a remake file
#'
#' @param remake_file filename of the remake YAML file from which targets should
#'   be collected
#' @param recursive logical. if TRUE, result will include all targets from any
#'   YAMLs listed in the include: section of the given remake_file, or any YAMLs
#'   listed in the include: sections of those included YAMLs, etc.
#' @return vector of all target names explicitly declared in this remake_file
#'   (and if recursive=TRUE, also the names of targets declared in remake files
#'   included by this remake_file)
#' @export
#' @examples
#' \dontrun{
#' # assuming you have a file named remake.yml:
#' list_all_targets() # get status for all explicitly named targets in remake.yml
#' 
#' # status for all targets in a different remake YAML:
#' list_all_targets('other_remake.yml')
#' 
#' # status for all targets in remake.yml and any remake YAMLs included by remake.yml
#' list_all_targets(recursive=TRUE)
#' }
list_all_targets <- function(remake_file=getOption('scipiper.remake_file'), recursive=FALSE) {
  # load the remake file as a yaml and as remake loads it
  remake_list <- yaml::yaml.load_file(remake_file)
  
  # get all explicitly defined targets
  targets <- names(remake_list$targets)
  
  # exclude remake keyword targets, which can be explicit even though they're
  # usually not
  targets <- setdiff(targets, c('tidy','clean','purge'))
  
  # if requested, include targets of the included ymls
  if(isTRUE(recursive)) {
    includes <- remake_list$include
    nested_targets <- unlist(lapply(includes, list_all_targets))
    targets <- c(targets, nested_targets)
  }
  
  # if we wanted to add more info about these targets, we could return the following instead:
  # remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
  # remake_object$targets[targets]
  
  # return a simple vector of target names
  targets
}

#' list targets in remake file that are fake/dummy/group targets
#' 
#' @param remake_file filename of the remake YAML file from which targets should
#'   be collected
#'   
#' @details this is an internal file. We may want to make it recursive, but I
#'   don't see a use-case right now for that. 
#' @keywords internal
list_group_targets <- function(remake_file=getOption('scipiper.remake_file')){
  # load the remake file as a yaml and as remake loads it
  remake_list <- yaml::yaml.load_file(remake_file)
  
  # get all explicitly defined targets
  targets <- names(remake_list$targets)
  targets[sapply(remake_list$targets, FUN = function(x) is.null(x$command), USE.NAMES = FALSE)]
}

#' Produce a table describing the remake build status relative to 1+ targets
#'
#' @param target_names character vector of targets for which to determine build
#'   status, including status for dependencies of the named targets. If NULL
#'   will return status for the default target and its dependencies.
#' @param remake_file filename of the remake YAML file from which status should
#'   be determined
#' @export
#' @examples
#' \dontrun{
#' # assuming you have a file named remake.yml:
#' get_remake_status() # get status for the default target and its dependencies
#' get_remake_status(list_all_targets()) # get status for all explicitly named targets in remake.yml
#' 
#' # or to get status for all targets in a different remake YAML:
#' get_remake_status(list_all_targets('other_remake.yml'), 'other_remake.yml')
#' }
get_remake_status <- function(target_names=NULL, remake_file=getOption('scipiper.remake_file')) {
  # collect information about the current remake database. do load sources to get the dependencies right
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  
  # make sure target_names is concrete
  if(is.null(target_names)) target_names <- remake_object$default_target
  
  unknown_targets <- setdiff(target_names, names(remake_object$targets))
  if(length(unknown_targets) > 0) stop('unknown targets: ', paste(unknown_targets, collapse=', '))
  
  # create a table of all targets relevant to target_names, with T/F fields for
  # dirty and dirty_by_descent. remake_status doesn't actually get used by
  # remake::make, and the code is full of caveats that make me wonder if they
  # believe it...but it's the nearest thing to a current status report that I've
  # found so far
  graph <- ('remake' %:::% 'remake_dependency_graph')(remake_object)
  status <- as.data.frame(('remake' %:::% 'remake_status')(remake_object, target_names, graph))
  status$target <- rownames(status)
  rownames(status) <- NULL
  
  if(nrow(status) == 0) {
    status <- data.frame(
      target='', is_current=FALSE, dirty=TRUE, dirty_by_descent=TRUE, time='', hash='', fixed='',
      stringsAsFactors=FALSE)[c(),]
  } else {
    status$is_current <- status$hash <- status$time <- status$fixed <- as.character(NA)
    for(i in seq_len(nrow(status))) {
      tryCatch({
        status[i,'is_current'] <- ('remake' %:::% 'remake_is_current')(remake_object, status$target[i])
        remeta <- remake_object$store$db$get(status$target[i])
        status[i,'hash'] <- as.character(remeta$hash)
        status[i,'time'] <- as.character(POSIX2char(remeta$time))
        status[i,'fixed'] <- if(!is.null(remeta$fixed)) remeta$fixed else as.character(NA)
      }, error=function(e) NULL)
    }
  }
  status[c('target','is_current','dirty','dirty_by_descent','time','hash','fixed')]
}
