#' Wrapper for remake::make that permits cache sharing
#'
#' The name: sc=scipiper, sc=shared cache. This wrapper could eventually involve
#' other custom hooks for things to do before or after building a target
#'
#' @param target_names as in remake::make
#' @param remake_file the file path+name of the remake file to use in building
#'   the targets. As in remake::make, except that for scmake this param comes
#'   before ... and second in line, so it can be easily specified without naming
#'   the argument
#' @param ... as in remake::make
#' @param force logical. if TRUE, the target_names will be deleted with `scdel`
#'   before being built.
#' @param verbose as in remake::make
#' @param allow_missing_packages as in remake::make
#' @param ind_ext the indicator file extension identifying those files for which
#'   build/status information will be shared via git-committable files in the
#'   build/status folder. You should git commit the resulting build/status
#'   files.
#' @export
scmake <- function(
  target_names = NULL, remake_file = getOption('scipiper.remake_file'), ..., 
  force = FALSE, verbose = TRUE, allow_missing_packages = FALSE, ind_ext = getOption("scipiper.ind_ext")) {
  
  # allow force rebuild by deleting the target[s] before attempting a build
  if(isTRUE(force)) {
    scdel(target_names=target_names, remake_file=remake_file, verbose=verbose, ind_ext=ind_ext)
  }
  
  # update .remake with any new build/status info
  RDSify_build_status(remake_file=remake_file)
  
  # record status before running make
  status_pre <- get_remake_status(target_names, remake_file=remake_file, format='flat', RDSify_first=FALSE)
  
  # run remake::make
  update_build_files <- function(target_names, remake_file, status_pre) {
    # record status after running make
    status_post <- get_remake_status(target_names, remake_file=remake_file, format='flat', RDSify_first=FALSE)
    
    # for every target that (1) changed status and (2) is a status indicator file,
    # make a text (YAML) copy of the build status file from the remake db storr;
    # put it in build/status
    tdiffs <- dplyr::anti_join(status_post, status_pre, by=names(status_pre))
    tdiffs <- tdiffs[is_ind_file(tdiffs$target, ind_ext=ind_ext),]
    YAMLify_build_status(tdiffs$target, remake_file=remake_file)
  }
  
  start_time <- Sys.time()
  if(verbose) message('Starting build at ', start_time)
  out <- withCallingHandlers({
    remake::make(
      target_names=target_names, ..., verbose=verbose,
      allow_missing_packages=allow_missing_packages, remake_file=remake_file)
  }, error = function(e) {
    update_build_files(target_names = target_names, remake_file = remake_file, status_pre = status_pre)
    stop(e)
  })
  
  end_time <- Sys.time()
  if(verbose) {
    message('Finished build at ', end_time)
    message(sprintf('Build completed in %0.2f minutes', as.numeric(end_time - start_time, units='mins')))
  }
  update_build_files(target_names = target_names, remake_file = remake_file, status_pre = status_pre)
  
  invisible(out)
}

#' Fetch objects from the scipiper data store without rebuilding them
#' 
#' Identical to `remake::fetch()` except that `remake_file` is the second argument
#' @importFrom remake fetch
#' @export
fetch <- function(
  target_name, remake_file=getOption('scipiper.remake_file'),
  require_current = FALSE, verbose = TRUE, allow_missing_packages = FALSE) {

  # a simple reordering of arguments so that remake_file can be second in the scipiper version
  remake::fetch(
    target_name = target_name,
    require_current = require_current,
    verbose = verbose,
    allow_missing_packages = allow_missing_packages,
    remake_file = remake_file)
}

#' Declare to remake and scipiper that a target is current
#'
#' `scbless` (aka `sc_declare_current`) tells remake and scipiper to update
#' their databases to reflect that the current target is up to date with respect
#' to the current dependencies
#'
#' @aliases sc_declare_current
#' @param target_names names of the targets to record as already current
#' @param remake_file the file path+name of the remake file to use in building
#'   the targets. As in remake::make, except that for scmake this param comes
#'   before ... and second in line, so it can be easily specified without naming
#'   the argument
#' @param ind_ext the indicator file extension identifying those files for which
#'   build/status information will be shared via git-committable files in the
#'   build/status folder. You should git commit the resulting build/status
#'   files.
#' @importFrom tibble is_tibble
#' @export
scbless <- function(
  target_names, remake_file = getOption('scipiper.remake_file'), ind_ext = getOption("scipiper.ind_ext")) {
  
  hash <- name <- type <- label <- dirty_by_descent <- '.dplyrvar'
  
  # read the remake graph to get information about targets. do load sources to get the dependencies right
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  store <- remake_object$store
  
  # check that the targets to bless are known (stop if not)
  if(missing(target_names)) stop('target_names must be specified')
  non_targets <- target_names[!target_names %in% names(remake_object$targets)]
  if(length(non_targets) > 0) {
    stop(sprintf("unrecognized target names: %s", paste(non_targets, collapse=', ')))
  }
  
  # pull target information, including whether the file/object exists already
  clearly_dirty <- intersect(target_names, which_dirty(target_names, remake_file=remake_file, RDSify_first=FALSE))
  target_info <- lapply(setNames(nm=target_names), function(target_name) {
    target <- remake_object$targets[[target_name]]
    
    # confirm the availability of the object or file
    target$exists <- switch(
      target$type,
      'object' = store$objects$exists(target$name),
      'file' = file.exists(target$name), # equivalent to store$files$exists but simpler
      'fake' = NA)
    
    target$dep_stat <- tryCatch({
      # remake returns a list if successful
      ('remake' %:::% 'dependency_status')(target, store, check="all")
    }, error=function(e) {
      # run our own dependency status function to diagnose why remake's didn't work
      get_dependency_status(target$name, remake_object, as_of='now', format='long')
    })
    target$dep_stat_success <- !tibble::is_tibble(target$dep_stat)
    
    # determine dirtiness. check the hash because the user might have manually
    # modified a file and then want to declare it current. we know it's not
    # current if we weren't able to get dependency status from remake, because
    # that means it wasn't built
    target$dirty <- target_name %in% clearly_dirty
    if(!target$dirty && target$exists && target$type == 'file' && target$dep_stat_success) {
      target$hash_new <- target$dep_stat$hash
      target$hash_old <- get_dependency_status(target$name, remake_object, as_of='last_build', format='wide')$hash
      if(target$hash_new != target$hash_old) {
        target$dirty <- TRUE
      }
    }
    
    return(target)
  })
  
  # check that the targets to bless are non-current targets (just warn and skip
  # if they're current)
  non_dirty <- names(which(!sapply(target_info, `[[`, 'dirty')))
  if(length(non_dirty) > 0) {
    warning(sprintf("these non-dirty targets will be skipped: %s", paste(non_dirty, collapse=', ')))
    target_names <- setdiff(target_names, non_dirty)
    target_info <- target_info[target_names]
  }
  
  # throw warning for fake targets, error for non-existent objects or files,
  # error for files whose dependency status couldn't be determined
  fake <- names(which(sapply(target_info, `[[`, 'type') == 'fake'))
  if(length(fake) > 0) {
    warning(sprintf("these fake targets will be skipped: %s", paste(fake, collapse=', ')))
    target_names <- setdiff(target_names, fake)
    target_info <- target_info[target_names]
  }
  non_existent <- names(which(!sapply(target_info, `[[`, 'exists')))
  if(length(non_existent) > 0) {
    stop(sprintf("these targets lack built objects or files, so cannot be blessed: %s", paste(non_existent, collapse=', ')))
  }
  no_dep_status <- names(which(!sapply(target_info, `[[`, 'dep_stat_success')))
  if(length(no_dep_status) > 0) {
    for(target in target_info[no_dep_status]) {
      no_hash <- dplyr::filter(target$dep_stat, hash=='none') %>% mutate(label=ifelse(is.na(name), type, name)) %>% pull(label)
      warning(sprintf("these dependencies of target '%s' could not be hashed: %s", target$name, no_hash))
    }
    stop(sprintf("couldn't determine new dependency status for these targets: %s", paste(no_dep_status, collapse=', ')))
  }
  
  # change remake's dependency records for the specified targets
  for(target in target_info) {
    store$db$set(target$name, target$dep_stat)
  }
  
  # change scipiper's dependency records for any specified indicator-file targets
  ind_files <- target_names[is_ind_file(target_names, ind_ext=ind_ext)]
  YAMLify_build_status(ind_files, remake_file=remake_file)
  
  # check and warn for targets that are now current and !dirty but are still dirty_by_descent
  if(length(target_names) > 0) {
    status <- get_remake_status(target_names, remake_file=remake_file)
    still_dirty <- dplyr::filter(status, dirty_by_descent) %>% pull(target)
    if(length(still_dirty) > 0) {
      warning(sprintf("these targets have been declared current but are still dirty by descent: %s", paste(still_dirty, collapse=', ')))
    }
  }
  
  # return the names of targets we actually blessed
  invisible(target_names)
}

#' @rdname scbless
#' @export
sc_declare_current <- scbless

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
#' @param target_names vector of targets to delete, or NULL to delete the
#'   default target. Use the output of `list_all_targets()` to delete all
#'   explicitly named targets in the remake file (excluding tidy, clean, and
#'   purge)
#' @param remake_file as in [remake::delete()]
#' @param verbose as in [remake::delete()]
#' @param ind_ext the indicator file extension identifying those files for which
#'   build/status information will be deleted if their targets are
#'   remake::deleted. You should git commit the deletion of any build/status
#'   files (unless you immediately rebuild them and commit any changes instead).
#' @export
#' @examples
#' \dontrun{
#' scdel('one_target', 'remake.yml')
#' scdel(NULL, 'remake.yml') # delete the default target
#' scdel(list_all_targets('remake.yml'), 'remake.yml')
#' }
scdel <- function(
  target_names = NULL,
  remake_file = getOption('scipiper.remake_file'),
  verbose = TRUE,
  ind_ext = getOption('scipiper.ind_ext')) {
  
  # read the remake graph to get information about targets. do load sources to get the dependencies right
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=TRUE)
  
  # make sure target_names is concrete
  if(is.null(target_names)) {
    target_names <- remake_object$default_target
  }
  
  # run remake::delete, which takes care of the file itself and the RDS status
  # file, leaving us with just the YAML file to deal with below. Lock in
  # dependencies=FALSE
  targets <- remake_object$targets[target_names]
  deletable <- sapply(targets, function(tg) tg$type %in% c('file', 'object'))
  remake::delete(target_names=target_names[deletable], dependencies = FALSE,
                 verbose = verbose, remake_file=remake_file)
  
  # get info about the remake project
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
  dbstore <- remake_object$store$db
  
  # for every deleted target that is a status indicator file,
  # delete or confirm the absence of the corresponding text (YAML) version of
  # the build status file in build/status
  status_targets <- target_names[is_ind_file(target_names, ind_ext=ind_ext)]
  status_keys <- get_mangled_key(status_targets, dbstore)
  status_files <- file.path('build/status', paste0(status_keys, '.yml'))
  status_exists <- status_files[file.exists(status_files)]
  if(length(status_exists) > 0) file.remove(status_exists)
  
  invisible()
}

#' Create an indicator file
#'
#' If only the first argument (`ind_file`) is given, the contents of the
#' indicator file change every time. To create an indicator file whose contents
#' are static, specify a fixed argument in `...`.
#'
#' @param ind_file file name of the indicator file to write
#' @param ... optional. named character strings/vectors to be written to the
#'   indicator file. one good option is a pre-computed hash of the actual data
#'   file (possibly retrieved as a hash from the remote cache). If you have the
#'   data_file locally and don't yet have a hash, just specify the `data_file`
#'   argument instead.
#' @param data_file optional. file name(s) of the data file(s) whose presence is being
#'   indicated. if length one, the hash of the data file will be included in the
#'   indicator file as the `hash` element. if given as a multi-file vector, 
#'   the file names and `hash` elements will be paired in the output. 
#' @param hash_depends logical. If TRUE, this call will look through
#'   `depends_makefile` for a recipe for `depends_target`, will generate a hash
#'   of each file or object listed in the depends section for that recipe, and
#'   will report those named hashes in `ind_file`. This pattern is useful for
#'   generating indicator files that sum up the output of a target that groups
#'   together many other targets in its depends section.
#' @param depends_target character name of a target in the remake .yml file
#'   specified in `depends_makefile`. This target must have at least one item in
#'   its depends section. Used only when `hash_depends=TRUE`.
#' @param depends_makefile character name of the remake file that contains a
#'   recipe for `depends_target`. Used only when `hash_depends=TRUE`.
#' @export
sc_indicate <- function(ind_file, ..., data_file, hash_depends=FALSE, depends_target, depends_makefile) {
  
  info_list <- list(...)
  
  # if data_file is given, get a hash of the file so we have the option of
  # checking whether this indicator file has gone bad
  if(!missing(data_file)) {
    if(!all(file.exists(data_file))) {
      stop('data_file must exist if specified')
    }
    if (length(data_file) > 1){
      for (file in data_file){
        info_list[[file]] <- unname(tools::md5sum(file))
      }
    } else {
      info_list$hash <- unname(tools::md5sum(data_file))
    }
  }
  
  # if hash_depends and depends_target and depends_makefile are given, create
  # hashes of all the dependencies of that depends_target and append them to
  # info_list
  if(isTRUE(hash_depends)) {
    if(missing(depends_target)) stop('depends_target is required when hash_depends=TRUE')
    if(missing(depends_makefile)) stop('depends_makefile is required when hash_depends=TRUE')
    hashes <- hash_dependencies(depends_target, depends_makefile)
    info_list <- c(info_list, as.list(hashes))
  }
  
  # if no writable information is given, use the current time. this is a
  # fallback when we don't have direct information about the contents of the
  # data file or when the thing being indicated isn't a file and probably
  # changes every time the indicator file gets written
  if(length(info_list) == 0) {
    info_list$indication_time <- POSIX2char(Sys.time())
  }
  
  if(ind_file == '') {
    # return the information as an R object
    return(info_list)
  } else {
    # write the info to the indicator file
    if(!dir.exists(dirname(ind_file))) dir.create(dirname(ind_file), recursive=TRUE)
    readr::write_lines(yaml::as.yaml(info_list), ind_file)
    return(invisible(NULL))
  }
}

#' Create an indicator file that contains hashes of other files
#'
#'
#' @param ind_file file name of the indicator file to write
#' @param ... files to combine into a single indicator file
#' 
#' @details light wrapper on `sc_indicate`
#' @export
combine_to_ind <- function(ind_file, ...){
  sc_indicate(ind_file = ind_file, data_file = c(...))
} 

#' Create an object that contains hashes of other files
#'
#' @param ... files to combine into a single indicator file
#' 
#' @details light wrapper on `sc_indicate`
#' @export
#' @examples 
#' tfiles <- tempfile(pattern=as.character(1:3))
#' for(i in 1:3) readr::write_lines(i, path=tfiles[i])
#' combine_to_tibble(tfiles)
combine_to_tibble <- function(...){
  if(length(c(...)) < 1) {
    tibble::tibble(name='', hash='')[c(),]
  } else {
    inds <- sc_indicate(ind_file = '', data_file = c(...))
    if(length(inds) == 1) {
      tibble(name=c(...), hash=inds$hash)
    } else {
      inds %>%
        unlist() %>%
        tibble::enframe(name='name', value='hash')
    }
  }
} 

#' Retrieve the data file declared by an indicator
#'
#' Identifies the data file's name by removing the indicator extension, then
#' calls `scmake` to retrieve that file using a recipe given in the remake.yml
#'
#' @param ind_file the file path of the indicator for which the corresponding
#'   data_file will be retrieved
#' @param remake_file the file path+name of the remake file to use in retrieving
#'   the data file. On 10/10/20 the default value was changed from
#'   `getOption('scipiper.remake_file')` to
#'   `getOption('scipiper.getters_file')`. It is recommended to place all
#'   getters in a file named getters.yml or similar, which should not be
#'   imported by the main remake files.
#' @param ind_ext the indicator file extension to expect at the end of ind_file,
#'   and for which any altered targets should have their build/status files
#'   updated
#' @return the name of the retrieved data file
#' @export
sc_retrieve <- function(ind_file, remake_file=getOption('scipiper.getters_file'), ind_ext=getOption('scipiper.ind_ext'), verbose=FALSE) {
  if(missing(remake_file) && !file.exists(remake_file)) {
    warning(sprintf("sc_retrieve now looks for recipes in '%s' (the 'scipiper.getters_file' option) by default, but that file does not exist", getOption('scipiper.getters_file')))
  }
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  scmake(data_file, remake_file=remake_file, ind_ext=ind_ext, verbose=verbose)
  return(data_file)
}


#' Determine whether target_names are indicator files
#'
#' Indicator files are those files (or maybe someday objects?) included in the
#' remake yml whose final extension is the accepted indicator extension ('ind'
#' by default, but see `?scipiper::options`). If the target does not have the
#' indicator extension, FALSE is returned; no warnings or errors are given if
#' the target is not in the remake yml.
#'
#' By default, the only accepted indicator extension is 'ind'. If you want other
#' extensions to be used, add a object target to your remake.yml that contains a
#' character vector of the accepted extensions. See below for an example.
#'
#' @param target_names character vector of remake target names
#' @param ind_ext the indicator file extension to recognize, i.e., the final
#'   file extension of files for which `is_ind_file()` should return `TRUE`
#' @examples
#' is_ind_file('mydata.rds') # FALSE
#' is_ind_file('mydata.rds.ind') # TRUE
#' is_ind_file('mydata.rds.st', ind_ext='st') # TRUE
#' is_ind_file('mydata.rds', ind_ext='rds') # TRUE but you shouldn't do this
#' @export
is_ind_file <- function(target_names, ind_ext=getOption("scipiper.ind_ext")) {
  tools::file_ext(target_names) == ind_ext
}

#' Returns the indicator name corresponding to the given data file name
#'
#' If `data_file` already has the indicator extension, an error will be
#' generated.
#'
#' @param data_file the data file name (with path as needed) whose corresponding
#'   indicator name should be returned
#' @param ind_ext the indicator file extension to apply
#' @export
#' @examples 
#' as_ind_file('mydata.rds') # 'mydata.rds.ind'
#' as_ind_file('mydata.rds', ind_ext='st') # 'mydata.rds.st'
#' \dontrun{
#' as_ind_file('mydata.rds.ind') # Error: "data_file contains indicator files: mydata.rds.ind"
#' }
as_ind_file <- function(data_file, ind_ext=getOption("scipiper.ind_ext")) {
  ind_files <- data_file[which(is_ind_file(data_file, ind_ext=ind_ext))]
  if(length(ind_files) > 0) {
    stop(sprintf('data_file contains indicator files: %s', paste(ind_files, collapse=', ')))
  }
  paste0(data_file, '.', ind_ext)
}

#' Return the data file name corresponding to the given indicator name
#'
#' If `ind_file` does not have the indicator extension, an error will be
#' generated.
#'
#' @param ind_file the indicator name (with path as needed) whose corresponding
#'   data file name should be returned
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @export
#' @examples
#' as_data_file('mydata.rds.ind') # 'mydata.rds'
#' as_data_file('mydata.rds.st', ind_ext='st') # 'mydata.rds'
#' \dontrun{
#' as_data_file('mydata.rds') # Error: "ind_file is not an indicator file"
#' }
as_data_file <- function(ind_file, ind_ext=getOption("scipiper.ind_ext")) {
  non_inds <- ind_file[which(!is_ind_file(ind_file, ind_ext=ind_ext))]
  if(length(non_inds) > 0) {
    stop(sprintf('ind_file contains non-indicator files: %s', paste(non_inds, collapse=', ')))
  }
  tools::file_path_sans_ext(ind_file)
}

#' Copy info from .remake/objects to build/status
#'
#' Copy status files from .remake folder (binary form) to build/status folder
#' (versionable text)
#'
#' @param target_names as in remake::make, vector of specific targets
#' @param remake_file filename of the remake YAML file for which the status of
#'   target_names should be YAMLified
#' @keywords internal
YAMLify_build_status <- function(target_names, remake_file=getOption('scipiper.remake_file')) {
  # ensure there's a directory to receive the export
  if(!dir.exists('build/status')) dir.create('build/status', recursive=TRUE)
  
  # get info about the remake project
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
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
    readr::write_lines(status_yml, status_file)
    
    return(status_file)
  })
}

#' Copy info from build/status to .remake/objects
#'
#' Copy build status files from versionable text to .remake binary (.rds file)
#'
#' @param new_only logical. It's possible to corrupt a shared-cache repo. One
#'   way this happens seems to be something about collaborating on .ind and
#'   build/status files via git. Another way to corrupt it is by calling
#'   remake::make after git pulling new build/status files and before calling
#'   scmake. Therefore, (1) this flag provides recourse; set new_only=FALSE to
#'   overwrite all .remake files for which we have build/status files, (2) the
#'   default is FALSE, and (3) still, you should avoid calling remake::make in a
#'   shared-cache repo; call scmake instead.
#' @param remake_file filename of the remake YAML file for which build/status
#'   files should be RDSified
#' @keywords internal
RDSify_build_status <- function(new_only=FALSE, remake_file=getOption('scipiper.remake_file')) {
  # get info about the remake project. calling remake:::remake gives us info and
  # simultaneously ensures there's a directory to receive the export (creates
  # the .remake dir as a storr)
  remake_object <- ('remake' %:::% 'remake')(remake_file=remake_file, verbose=FALSE, load_sources=FALSE)
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
#' @param key character vector of key(s) to convert
#' @param dbstore a storr containing the remake build status, as from
#'   `remake:::remake(load_sources=FALSE)$store$db`
#' @keywords internal
get_mangled_key <- function(key, dbstore) {
  basename(dbstore$driver$name_key(key, ''))
}
