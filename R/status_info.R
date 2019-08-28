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
#' Describes the hash, build time, and dependencies as of the most recent build
#' of each target. If the dependencies have changed since that last build, those
#' new dependencies are not described here. See why_dirty() to compare the old
#' and new dependencies.
#'
#' Runs faster when RDSify_first = FALSE, but the default is TRUE so that you
#' can usually pretend that there's no extra step between a git pull and
#' updating the remake database.
#'
#' @param target_names character vector of targets for which to determine build
#'   status, including status for dependencies of the named targets. If NULL
#'   will return status for the default target and its dependencies.
#' @param remake_file filename of the remake YAML file from which status should
#'   be determined
#' @param RDSify_first logical. Should the info in build/status/*.yml files be
#'   copied over to the remake RDS-based status database before querying for
#'   target cleanliness?
#' @export
#' @examples
#' \dontrun{
#' # assuming you have a file named remake.yml:
#' get_remake_status() # get status for the default target and its dependencies
#' get_remake_status(list_all_targets(), RDSify_first=FALSE) # status for all targets in remake.yml
#'
#' # or to get status for all targets in a different remake YAML:
#' get_remake_status(list_all_targets('other_remake.yml'), 'other_remake.yml')
#' }
get_remake_status <- function(target_names=NULL, remake_file=getOption('scipiper.remake_file'), RDSify_first=TRUE) {
  # sync the remake status database with the scipiper status database if requested
  if(isTRUE(RDSify_first)) {
    RDSify_build_status(new_only=FALSE, remake_file=remake_file)
  }
  
  # collect information about the current remake database. do load sources to get the dependencies right
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  
  # make sure target_names is concrete and sensible
  if(is.null(target_names)) target_names <- remake_object$default_target
  unknown_targets <- setdiff(target_names, names(remake_object$targets))
  if(length(unknown_targets) > 0) stop('unknown targets: ', paste(unknown_targets, collapse=', '))
  
  # create a table of all targets relevant to target_names, with T/F fields for
  # dirty and dirty_by_descent. remake_status doesn't actually get used by
  # remake::make, and the code is full of caveats that make me wonder if they
  # believe it...but it's the nearest thing to a current status report that I've
  # found so far
  graph <- ('remake' %:::% 'remake_dependency_graph')(remake_object)
  currentness <- ('remake' %:::% 'remake_status')(remake_object, target_names, graph) %>% # matrix
    as.data.frame() %>%
    tibble::rownames_to_column(var='target') %>% as_tibble() %>% # convert to tibble with rownames as 'target' column
    dplyr::mutate( # mutate maintains a 0-row tibble if it started that way
      is_current = ('remake' %:::% 'remake_is_current')(remake_object, target)) %>%
    dplyr::select(target, is_current, dirty, dirty_by_descent)
  
  # extract dependency info for each target
  dependencies <- lapply(currentness$target, function(target_name) {
    # I can't decide whether this should be as_of 'now' or 'last_build', or if
    # it even makes sense to report this stuff in a table that also reports
    # is_current (which compares 'now' to 'last_build'). I'm trying last_build
    # for now.
    status_old <- get_dependency_status(target_name, remake_object, as_of='last_build', format='wide')
  }) %>%
    bind_rows()
  
  # combine into a single tibble
  status <- full_join(currentness, dependencies, by='target')
  
  return(status)
}

#' Retrieve status of dependencies from the remake store
#'
#' Returns placeholders if the object doesn't yet exist.
#'
#' @param target_name length-1 name of a target
#' @param remake_object as produced by remake:::remake
#' @param as_of what status should be reflected - status as of the last build
#'   (from the remake database) or status if we considered it built right now?
#' @param format return in remake's list format ('raw'), a one-row tibble with
#'   nested tibbles for depends and code ('wide'), or a multi-row tibble with
#'   columns for name, dependency type, and dependency hash ('long')?
get_dependency_status <- function(target_name, remake_object, as_of=c('last_build', 'now'), format=c('raw', 'wide', 'long')) {
  # process arguments
  target <- remake_object$targets[[target_name]]
  store <- remake_object$store
  as_of <- match.arg(as_of)
  format <- match.arg(format)
  
  # helper
  null_to_na <- function(x) {
    if(is.null(x)) NA else x
  }
  
  # mimic remake's behavior, using existing remake functions where possible
  status_list_default <- list(
    version=store$version,
    name=target_name,
    type=null_to_na(target$type),
    hash=NA,
    time=NA,
    depends=list(),
    fixed=NULL,
    code=list())
  status_list <- if(is.null(target)) {
    status_list_default
  } else if(target$type == 'fake') {
    status_list_default
  } else if('target_file_implicit' %in% attr(target, 'class')) {
    # target_file_implicits don't have a target rule (code) to check, so
    # just check depends. and return time as the time of last file update,
    # because Sys.time() just doesn't make sense
    status_list_implicit <- remake:::dependency_status(target, store, missing_ok=TRUE, check='depends')
    status_list_implicit$time <- file.mtime(target$name)
    status_list_implicit
  } else {
    switch(
      as_of,
      'last_build' = tryCatch({
        store$db$get(target_name)
      }, error=function(e) {
        # warning(sprintf('store$db$get failed for "%s"', target_name))
        status_list_default
      }),
      'now' = tryCatch({
        remake:::dependency_status(target, store, missing_ok=TRUE, check='all')
      }, error=function(e) {
        warning(sprintf('remake:::dependency_status failed for "%s"', target_name))
        status_list_default
      })
    )
  }
  
  if(format == 'raw') return(status_list)
  
  # override depends to include dependencies of fake targets, and to include
  # fake targets as dependencies (remake does neither; only file and object
  # dependencies of file or object targets are reported)
  if(is.null(target)) {
    missing_depends <- tibble(type='', name='')[c(),]
  } else if(target$type == 'fake') {
    missing_depends <- tibble(type = target$depends_type, name = target$depends_name)
  } else {
    missing_depends <- tibble(type = target$depends_type, name = target$depends_name) %>%
      dplyr::filter(type == 'fake')
  }
  if(nrow(missing_depends) > 0) {
    # we don't actually know which targets were dependencies during the
    # last_build, so we've had to assume that they're the same as those that are
    # dependencies now. we'll also take this into account in why_dirty().
    if(target$type == 'fake') {
      warning(sprintf("guessing names and returning hash=NA for depends of fake target '%s' during last build", target$name))  
    } else {
      if(as_of == 'last_build') {
        warning(sprintf("guessing names and returning hash=NA for fake depends of target '%s' during last build", target$name))  
      } else {
        # fake dependencies have no hash available through remake, hence the warning about hash=NA even when as_of=='now'
        warning(sprintf("returning hash=NA for fake dependencies of target '%s'", target$name)) 
      }
    }
    status_list$depends <- c(
      status_list$depends, # append the new dependencies to the current ones
      setNames(
        purrr::map2_chr(target$depends_name, target$depends_type, function(name, format) {
          if(format %in% c('file', 'object')) {
            switch(
              as_of,
              'last_build' = NA,
              'now' = store$get_hash(name, format, missing_ok=TRUE))
          } else { # fake
            NA
          }
        }),
        target$depends_name))
  }
  
  # long format
  status_long <- bind_rows(
    tibble(type = 'target', name = status_list$name, hash = status_list$hash),
    if(length(status_list$depends) > 0) {
      tibble(type='depends', name=names(status_list$depends), hash=unlist(unname(status_list$depends)))
    },
    if(!is.null(status_list$fixed)) {
      tibble(type='fixed', name=NA, hash=status_list$fixed)
    },
    if(length(status_list$code$functions) > 0) {
      tibble(type='functions', name=names(status_list$code$functions), hash=unlist(unname(status_list$code$functions)))
    }
  )
  if(format == 'long') return(status_long)
  
  # wide format    
  status_wide <- tibble(
    target = status_list$name,
    type = status_list$type,
    hash = status_list$hash,
    time = status_list$time,
    depends = status_long %>% dplyr::filter(type=='depends') %>% select(name, hash) %>% list(),
    fixed = null_to_na(status_list$fixed),
    functions = status_long %>% dplyr::filter(type=='functions') %>% select(name, hash) %>% list())
  
  if(format == 'wide') return(status_wide)
  
  stop('a status report should have been returned by now')
}

#' List the targets for which is_current is FALSE (as a character vector)
#' 
#' Runs faster when RDSify_first = FALSE.
#'
#' @param target_names character vector of targets for which to determine build
#'   status, including status for dependencies of the named targets. If NULL
#'   will return status for the default target and its dependencies.
#' @param remake_file filename of the remake YAML file from which status should
#'   be determined
#' @param RDSify_first logical. Should the info in build/status/*.yml files be
#'   copied over to the remake RDS-based status database before querying for
#'   target cleanliness?
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
which_dirty <- function(target_names=NULL, remake_file=getOption('scipiper.remake_file'), RDSify_first=TRUE) {
  
  # get the full status information for all relevant targets
  status <- get_remake_status(target_names=target_names, remake_file=remake_file, RDSify_first=RDSify_first)
  
  # pick out the non-current targets. use is_current rather than dirty because I
  # think something can be dirty by descent without being dirty...maybe...
  status %>%
    dplyr::filter(!is_current) %>%
    pull(target)
}

#' Explain why a target is dirty
#'
#' Compares the dependency status of a target (1) as it currently appears in the
#' repo and (2) as documented in the remake database. Returns information about
#' the mismatches that are causing remake to consider this target dirty.
#'
#' @param target_names character vector of targets for which to determine build
#'   status, including status for dependencies of the named targets. If NULL
#'   will return status for the default target and its dependencies.
#' @param remake_file filename of the remake YAML file from which status should
#'   be determined
#' @param RDSify_first logical. Should the info in build/status/*.yml files be
#'   copied over to the remake RDS-based status database before querying for
#'   target cleanliness? Defaults to FALSE because if you're asking why, you've
#'   probably already queried to determine that the target is dirty
#' @export
why_dirty <- function(target_name, remake_file=getOption('scipiper.remake_file'), RDSify_first=FALSE) {
  
  # get the full status information for all relevant targets
  status <- get_remake_status(target_names=target_name, remake_file=remake_file, RDSify_first=RDSify_first)
  
  # check that it's actually dirty
  is_current <- status %>%
    dplyr::filter(target == target_name) %>%
    pull(is_current)
  if(is_current) {
    stop(sprintf("target '%s' is not dirty", target_name))
  }
  
  # collect information about the current remake database. do load sources to get the dependencies right
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  store <- remake_object$store
  
  # get any pre-existing dependency information
  old_status <- get_dependency_status(target_name, remake_object, as_of='last_build', format='long')
  new_status <- get_dependency_status(target_name, remake_object, as_of='now', format='long')
  
  # compare
  remeta_compare <- full_join(old_status, new_status, by=c('type', 'name'), suffix=c('.old', '.new')) %>%
    mutate(dirty = xor(is.na(hash.old), is.na(hash.new)) | (!is.na(hash.old) & !is.na(hash.new) & (hash.old != hash.new)))
  
  # interpret
  dirty_rows <- dplyr::filter(remeta_compare, dirty)
  # note that remake doesn't compare depends for group=fake targets, but we'll make a note of these anyway
  
  # return
  return(remeta_compare)
}
