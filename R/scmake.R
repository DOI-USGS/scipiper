#' Wrapper for remake::make that permits cache sharing
#'
#' The name: sc=scipiper, sc=shared cache. This wrapper could eventually involve
#' other custom hooks for things to do before or after building a target
#'
#' @param target_names as in remake::make
#' @param ... as in remake::make
#' @param verbose as in remake::make
#' @param allow_missing_packages as in remake::make
#' @param remake_file as in remake::make, except that for scmake this param
#'   comes before ... and second in line, so it can be easily specified without
#'   naming the argument
#' @export
scmake <- function(
  target_names = NULL, remake_file = options('scipiper.remake_file')[[1]], ..., 
  verbose = TRUE, allow_missing_packages = FALSE) {
  
  # update .remake with any new build/status info
  RDSify_build_status(remake_file=remake_file)
  
  # record status before running make
  status_pre <- get_remake_status(target_names, remake_file=remake_file)
  
  # run remake::make
  start_time <- Sys.time()
  if(verbose) message('Starting build at ', start_time)
  out <- remake::make(
    target_names=target_names, ..., verbose=verbose,
    allow_missing_packages=allow_missing_packages, remake_file=remake_file)
  end_time <- Sys.time()
  if(verbose) {
    message('Finished build at ', end_time)
    message(sprintf('Build completed in %0.2f minutes', as.numeric(end_time - start_time, units='mins')))
  }
  
  # record status after running make
  status_post <- get_remake_status(target_names, remake_file=remake_file)
  
  # for every target that (1) changed status and (2) is a status indicator file,
  # make a text (YAML) copy of the build status file from the remake db storr;
  # put it in build/status
  tdiffs <- dplyr::anti_join(status_post, status_pre, by=names(status_pre))
  tdiffs <- tdiffs[is_indicator(tdiffs$target),]
  YAMLify_build_status(tdiffs$target, remake_file=remake_file)
  
  invisible(out)
}

#' Wrapper for remake::delete that permits cache sharing
#'
#' [remake::delete()] claims that for files you can generally just delete the
#' file itself with no need to call remake::delete(). This may also be the case
#' for a shared cache; especially for non-status-indicator files (which have no
#' build status file) however, it seems cleaner to delete the build status files
#' at the same time that one deletes an indicator file.
#'
#' The option to set `dependencies=TRUE` for [remake::delete()] is omitted
#' because it sounds terrifying to me: as currently implemented in remake,
#' dependencies are the UPSTREAM targets on which the current target_names
#' depend - i.e., if B is built from A and you ask to delete B with
#' dependencies=TRUE, A will also be deleted. Scary, right? So let's not.
#'
#' @param target_names vector of targets to delete
#' @param verbose as in [remake::delete()]
#' @param remake_file as in [remake::delete()]
#' @md
#' @export
scdel <- function(
  target_names, verbose = TRUE,
  remake_file = options('scipiper.remake_file')[[1]]) {
  
  # run remake::delete, which takes care of the file itself and the RDS status
  # file, leaving us with just the YAML file to deal with below. Lock in
  # dependencies=FALSE
  remake::delete(target_names=target_names, dependencies = FALSE,
                 verbose = verbose, remake_file=remake_file)
  
  # get info about the remake project
  remake_object <- remake:::remake(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
  dbstore <- remake_object$store$db
  
  # for every deleted target that is a status indicator file,
  # delete or confirm the absence of the corresponding text (YAML) version of
  # the build status file in build/status
  status_targets <- target_names[is_indicator(target_names)]
  status_keys <- get_mangled_key(status_targets, dbstore)
  status_files <- file.path('build/status', paste0(status_keys, '.yml'))
  status_exists <- status_files[file.exists(status_files)]
  file.remove(status_files)
}

#' Create an indicator file
#'
#' If only the first argument (`indicator`) is given, the contents of the
#' indicator file change every time. To create an indicator file whose contents
#' are static, specify a fixed argument in `...`.
#'
#' @param indicator file name of the indicator file to write
#' @param ... optional. named character strings/vectors to be written to the
#'   indicator file. one good option is a pre-computed hash of the actual data
#'   file (possibly retrieved as a hash from the remote cache). If you have the
#'   data_file locally and don't yet have a hash, just specify the `data_file`
#'   argument instead.
#' @param data_file optional. file name of the data file whose presence is being
#'   indicated. if given, the hash of the data file will be included in the
#'   indicator file as the `hash` element.
#' @md
#' @export
sc_indicate <- function(indicator, ..., data_file) {
  
  info_list <- list(...)
  
  # if data_file is given, get a hash of the file so we have the option of
  # checking whether this indicator file has gone bad
  if(!missing(data_file)) {
    if(!file.exists(data_file)) {
      stop('data_file must exist if specified')
    }
    info_list$hash <- unname(tools::md5sum(data_file))
  }
  
  # if no writable information is given, use the current time. this is a
  # fallback when we don't have direct information about the contents of the
  # data file or when the thing being indicated isn't a file and probably
  # changes every time the indicator file gets written
  if(length(info_list) == 0) {
    info_list$indication_time <- POSIX2char(Sys.time())
  }
  
  # write the info to the indicator file
  writeLines(yaml::as.yaml(info_list), con=indicator)
  
  invisible(NULL)
}
#' Retrieve the data file declared by an indicator
#'
#' Identifies the data file's name by removing the indicator extension, then
#' calls `scmake` to retrieve that file using a recipe given in the remake.yml
#'
#' @md
#' @param indicator the file path of the indicator
#' @export
sc_retrieve <- function(indicator) {
  data_file <- as_data_file(indicator)
  scmake(data_file, verbose=FALSE)
  return(data_file)
}


#' Determine whether target_names are status indicator files
#'
#' Status indicator files are those files (or maybe someday objects?) included
#' in the remake yml whose final extension is the accepted indicator extension
#' ('ind' by default, but see `indicator_extension`). If the target does not
#' have the indicator extension, FALSE is returned; no warnings or errors are
#' given if the target is not in the remake yml.
#'
#' By default, the only accepted indicator extension is 'ind'. If you want other
#' extensions to be used, add a object target to your remake.yml that contains a
#' character vector of the accepted extensions. See below for an example.
#'
#' @param target_names character vector of remake target names
#' @param remake_file filename of the remake YAML file
#' @examples
#' \dontrun{
#' # example remake.yml target to define extensions
#' targets:
#'   indicator_extensions:
#'     command: c(I("ind"))
#' }
#' @md
#' @export
is_indicator <- function(target_names) {
  tools::file_ext(target_names) == indicator_extension()
}

#' Returns the indicator file extension for this project
#'
#' The default extension is 'ind', but you can override this by defining a
#' remake recipe for `indicator_extension` in your remake_file, where the result
#' of that recipe is a length-1 character extension (with no leading period)
#'
#' @param remake_file name of the remake YAML file
#' @md
#' @export
indicator_extension <- function(remake_file=options('scipiper.remake_file')[[1]]) {
  all_targets <- remake::list_targets(remake_file=remake_file)
  if('indicator_extension' %in% all_targets) {
    ext <- remake::make('indicator_extension', remake_file=remake_file, verbose=FALSE)
    if(length(ext) != 1) {
      stop("expecting exactly 1 extension in indicator_extension in remake_file")
    }
    if(grepl('^\\.', ext)) {
      stop("indicator_extension should not begin with '.'")
    }
  } else {
    ext <- 'ind' # the default is to recognize only the .ind extension
  }
  return(ext)
}

#' Returns the indicator name corresponding to the given data file name
#'
#' If `data_file` already has the indicator extension, an error will be
#' generated.
#'
#' @param data_file the data file name (with path as needed) whose corresponding
#'   indicator name should be returned
#' @md
#' @export
as_indicator <- function(data_file) {
  if(is_indicator(data_file)) {
    stop('data_file is an indicator file already')
  }
  paste0(data_file, '.', indicator_extension())
}

#' Return the data file name corresponding to the given indicator name
#'
#' If `ind_file` does not have the indicator extension, an error will be
#' generated.
#'
#' @param ind_file the indicator name (with path as needed) whose corresponding
#'   data file name should be returned
#' @export
as_data_file <- function(ind_file) {
  if(!is_indicator(ind_file)) {
    stop('ind_file is not an indicator file')
  }
  tools::file_path_sans_ext(ind_file)
}

#' Produce a table describing the remake build status relative to 1+ targets
#'
#' @param target_names character vector of targets for which to determine build
#'   status (complete status will include dependencies of these targets)
#' @param remake_file filename of the remake YAML file
#' @md
#' @export
get_remake_status <- function(target_names, remake_file=options('scipiper.remake_file')[[1]]) {
  # collect information about the current remake database. do load sources to get the dependencies right
  remake_object <- remake:::remake(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  
  unknown_targets <- setdiff(target_names, names(remake_object$targets))
  if(length(unknown_targets) > 0) stop('unknown targets: ', paste(unknown_targets, collapse=', '))
  
  # create a table of all targets relevant to target_names, with T/F fields for
  # dirty and dirty_by_descent. remake_status doesn't actually get used by
  # remake::make, and the code is full of caveats that make me wonder if they
  # believe it...but it's the nearest thing to a current status report that I've
  # found so far
  graph <- remake:::remake_dependency_graph(remake_object)
  status <- as.data.frame(remake:::remake_status(remake_object, target_names, graph))
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
        status[i,'is_current'] <- remake:::remake_is_current(remake_object, status$target[i])
        remeta <- remake_object$store$db$get(status$target[i])
        status[i,'hash'] <- as.character(remeta$hash)
        status[i,'time'] <- as.character(POSIX2char(remeta$time))
        status[i,'fixed'] <- if(!is.null(remeta$fixed)) remeta$fixed else as.character(NA)
      }, error=function(e) NULL)
    }
  }
  status[c('target','is_current','dirty','dirty_by_descent','time','hash','fixed')]
}

#' Copy info from .remake/objects to build/status
#' 
#' Copy status files from .remake folder (binary form) to build/status folder
#' (versionable text)
#'  
#' @keywords internal
YAMLify_build_status <- function(target_names, remake_file=options('scipiper.remake_file')[[1]]) {
  # ensure there's a directory to receive the export
  if(!dir.exists('build/status')) dir.create('build/status', recursive=TRUE)
  
  # get info about the remake project
  remake_object <- remake:::remake(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
  dbstore <- remake_object$store$db
  
  # figure out which of target_names to export: we stick to files that have keys
  # existing in the .remake database, remake_db namespace. we avoid objects
  # because if we included them we'd need to share the objects among developers,
  # too, which sounds a lot like writing and sharing files but would require a
  # second system on top of the one we're already supporting. and no sense in
  # trying to export targets for which we have no .remake status
  rtargs <- remake::list_targets(remake_file=remake_file, type='file') # file targets
  rstats <- dbstore$list() # exist in db
  to_export <- intersect(intersect(rtargs, rstats), target_names)
  
  # for each target whose status we want to export, pull the status data,
  # convert non-character fields to character, and save as a .yml file
  sfiles <- lapply(seq_along(to_export), function(i) {
    status <- dbstore$get(to_export[i])
    status$version <- as.character(status$version)
    status$time <- POSIX2char(status$time)
    
    status_yml <- yaml::as.yaml(status)
    status_key <- get_mangled_key(to_export[i], dbstore)
    status_file <- file.path('build/status', paste0(status_key, '.yml'))
    writeLines(status_yml, status_file)
    
    return(status_file)
  })
}

#' Copy info from build/status to .remake/objects
#' 
#' Copy build status files from versionable text to .remake binary (.rds file)
#' 
#' @param new_only logical. You could corrupt a shared-cache repo by calling
#'   remake::make after git pulling new build/status files and before calling
#'   scmake. Therefore, (1) you should avoid calling remake::make in a
#'   shared-cache repo; call scmake instead, and (2) this flag provides
#'   recourse; set new_only=FALSE to overwrite all .remake files for which we
#'   have build/status files
#' @keywords internal
RDSify_build_status <- function(new_only=TRUE, remake_file=options('scipiper.remake_file')[[1]]) {
  # get info about the remake project. calling remake:::remake gives us info and
  # simultaneously ensures there's a directory to receive the export (creates
  # the .remake dir as a storr)
  remake_object <- remake:::remake(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
  dbstore <- remake_object$store$db
  
  # figure out which build/status files to import. don't import info for targets
  # that don't exist in the current remake.yml. stargs = targets for which we
  # have YAML status files. rtargs = targets in the remake.yml for which we'd
  # like build info if it's available. If we only ever YAMLify file targets, we
  # may not actually need to filter by rtargs; stargs could be enough. Keeping
  # the filter for now because this system is new and may evolve
  sfiles <- dir('build/status', full.names=TRUE)
  skeys <- gsub('\\.yml$', '', basename(sfiles))
  stargs <- storr::decode64(skeys) # decode=demangle. i think we can leave mangle_key_pad, etc. to defaults...
  rtargs <- remake::list_targets(remake_file=remake_file, type='file') # only import file targets (we don't sync object build status)
  # intersect stargs & rtargs, combine mangled/unmangled keys
  to_import <- data.frame(target=stargs, mkey=skeys, yaml=sfiles, stringsAsFactors=FALSE)[stargs %in% rtargs,]
  
  # for efficiency, filter to just those targets whose key file is older than
  # the yaml file; if we've already updated the key file, no sense in
  # overwriting. this strategy could be disrupted if you call remake::make after
  # git pulling new build/status files and before calling scmake - you should
  # therefore avoid that practice (i.e., don't ever call remake::make in a
  # shared cache repo), but we can also provide a helper here in the form of a
  # new_only flag that could be changed
  if(new_only && nrow(to_import) > 0) {
    to_import$yaml_time <- file.mtime(to_import$yaml)
    to_import$key_time <- file.mtime(dbstore$driver$name_key(to_import$target, 'remake_db'))
    to_import <- to_import[is.na(to_import$key_time) || to_import$key_time < to_import$yaml_time, ]
  }
  
  # do the import for the files that need it. 
  rfiles <- sapply(seq_len(nrow(to_import)), function(i) {
    status <- yaml::yaml.load_file(to_import$yaml[i])
    status$version <- as.package_version(status$version)
    status$time <- char2POSIX(status$time)
    
    dbstore$set(key=to_import$target[i], value=status)
    return(to_import$mkey[i])
  })
  
  # check for obsolete build/status files to help maintain integrity of the repo
  unmangled_keys <- dbstore$driver$list_keys(dbstore$driver$list_namespaces())
  storekeys <- get_mangled_key(key=unmangled_keys, dbstore=dbstore)
  extra_rfiles <- setdiff(rfiles, storekeys)
  if(length(extra_rfiles) > 0) warning(paste("these build/status files may be obsolete:", paste(extra_rfiles, collapse=", ")))
}

#' Convert keys into mangled keys as used in the .remake storr
#'
#' Whereas file and object names might be invalid or confusing as key file
#' names, mangled keys are always good as file names - no punctuation or
#' misleading suffixes
#' @param key character vector of key[s] to convert
#' @param dbstore a storr containing the remake build status, as from
#'   remake:::remake(load_sources=FALSE)$store$db
#' @keywords internal
get_mangled_key <- function(key, dbstore) {
  basename(dbstore$driver$name_key(key, ''))
}
