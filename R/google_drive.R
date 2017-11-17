# These functions download or upload any file you want, using the
# project-specific configuration for a Google Drive folder

#' Create or overwrite a Google Drive configuration file
#'
#' @param folder character name of the Google Drive folder where all files for
#'   this project are to be stored
#' @param config_file character name of the yml file where this configuration
#'   information should be written
#' @export
gd_config <- function(folder, config_file=options("scipiper.gd_config_file")[[1]]) {
  # write the given information to the specified config_file
  cfg <- list(folder=folder)
  if(!dir.exists(dirname(config_file))) dir.create(dirname(config_file), recursive=TRUE)
  writeLines(yaml::as.yaml(cfg), config_file)
  
  # check for credentials
  cred_file <- '.httr-oauth'
  if(!file.exists(cred_file)) {
    warning(paste0("googledrive expects credentials at ", cred_file, " - see ??drive_auth to create this file"))
  }
}

#' Upload a file to Google Drive
#'
#' Upload (create or overwrite) a file to the project bucket and path.
#'
#' @param data_file character name of the data file to upload. The basename will
#'   be used as the Google Drive key and must be unique within the project
#'   (bucket & path)
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param on_exists what to do if the file already exists - update, replace, or
#'   throw an error? the default is to update (using google drive's versioning
#'   functionality)
#' @param verbose logical, used in gd_put and passed onto
#'   googledrive::drive_update, drive_upload, and/or drive_rm
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
gd_put <- function(data_file, ind_file, on_exists=c('update','replace','stop'), verbose=FALSE, config_file=options("scipiper.gd_config_file")[[1]]) {
  
  on_exists <- match.arg(on_exists)
  if(!file.exists(data_file)) stop('data_file does not exist')
  
  require_libs('googledrive')
  gd_config <- yaml::yaml.load_file(config_file)
  
  # determine whether and where the remote file exists
  remote_path <- gd_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  
  # create the parent folder[s] on google drive as needed
  if(is.na(remote_id)) {
    # determine what exists
    remote_dirs <- remote_path %>%
      slice(-1) %>%slice(-nrow(.)) %>% # chop off the top parent-dir row and the bottom all-NA row
      pull(name)
    final_dirs <- strsplit(dirname(get_relative_path(data_file)), split='/')[[1]]
    
    # double-check that any overlapping path elements agree
    stopifnot(all.equal(final_dirs[seq_along(remote_dirs)], remote_dirs))
    
    # determine the last known parent, which is either already or soon to be the
    # proximate parent of the item to create
    parent <- remote_path %>% slice(nrow(remote_path)-1) %>% pull(id)
    
    # create any needed directories on google drive, updating the parent until
    # it's the proximate parent
    if(length(final_dirs) > length(remote_dirs)) {
      needed_dirs <- if(length(remote_dirs) == 0) {
        final_dirs
      } else {
        final_dirs[-seq_along(remote_dirs)]
      }
      # add the needed directories in order
      for(i in seq_along(needed_dirs)) {
        parent <- googledrive::drive_mkdir(name=needed_dirs[i], parent=as_id(parent))$id
      }
    }
  } else {
    parent <- remote_path %>% slice(nrow(remote_path)-1) %>% pull(id)
  }
  
  # post the file (create or update) from the local data_file to Google Drive
  if(verbose) message("Uploading ", data_file, " to Google Drive")
  if(is.na(remote_id)) {
    remote_id <- googledrive::drive_upload(media=data_file)$id
  } else {
    switch(
      on_exists,
      update={
        remote_id <- googledrive::drive_update(as_id(remote_id), media=data_file, verbose=verbose)$id
      },
      replace={
        googledrive::drive_rm(as_id(remote_id), verbose=verbose)
        remote_id <- googledrive::drive_upload(media=data_file, verbose=verbose)$id
      },
      stop={
        stop('file already exists and on_exists==stop')
      }
    )
  }
  
  # write the indicator file (involves another check on Google Drive to get the timestamp)
  success <- gd_confirm_posted(data_file=data_file, ind_file=ind_file, config_file=config_file)
  if(!success) {
    stop(paste0("Data file could not be posted to Google Drive: ", data_file))
  }
  return(success)
}

#' Download a file from Google Drive
#'
#' Download a file from Google Drive to the local project based on the
#' information implied by the indicator file (including the location on google
#' drive and the local destination location)
#'
#' @param indicator character name of the indicator file for which data should
#'   be downloaded. downloads the Google Drive object whose key equals the
#'   data_file basename
#' @param type see `type` argument to `googledrive::drive_download()`
#' @param overwrite see `overwrite` argument to `googledrive::drive_download()`
#' @param verbose see `verbose` argument to `googledrive::drive_download()`;
#'   also used to determine whether to include messages specific to `gd_get()`
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @md
#' @examples
#' \dontrun{
#' gd_get('0_test/test_sheet.ind', type='xlsx', overwrite=TRUE)
#' }
#' @export
gd_get <- function(indicator, type=NULL, overwrite=TRUE, verbose=FALSE, config_file=options("scipiper.gd_config_file")[[1]]) {
  
  # infer the data file name from the indicator. gd_get always downloads to that
  # location if it downloads at all
  data_file <- as_data_file(indicator)
  
  # if this function is being called straight from gd_put and the data file
  # exists, then gd_put is trying to bypass a superfluous download from google
  # drive. don't re-pull the data right now, just return smoothly so remake
  # understands that what we have is already up to date.
  gd_put_is_parent <- any(sapply(sys.calls(), function(sc) { isTRUE(sc[[1]] == 'gd_put') }))
  if(file.exists(data_file) && gd_put_is_parent) {
    invisible()
  }
  
  require_libs('googledrive')
  
  # figure out whether and where the file exists on gdrive
  remote_path <- gd_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  
  # download the file from Google Drive to the local data_file
  if(!is.na(remote_id)) {
    if(verbose) message("Downloading ", data_file, " from Google Drive")
    if(!dir.exists(dirname(data_file))) dir.create(dirname(data_file), recursive = TRUE)
    googledrive::drive_download(
      file=as_id(remote_id), path=data_file,
      type=type, overwrite=overwrite, verbose=verbose)
  } else {
    stop(paste0("Could not locate ", data_file, " for download from Google Drive"))
  }
}

# Locate a file along a path relative to the gd_config project_folder, or return NA if not found
gd_locate_file <- function(file, config_file=options("scipiper.gd_config_file")[[1]]) {
  # load the project's googledrive configuration
  gd_config <- yaml::yaml.load_file(config_file)
  
  # normalize the relative path for this file so we can use in confidently as a
  # relative path from both the local working directory and the google drive
  # parent folder
  relative_path <- get_relative_path(file)
  
  # query google drive for all possibly relevant files and add their parents as
  # a simple column
  relevant_files <- bind_rows(
    drive_get(
      id=as_id(gd_config$project_folder)),
    drive_ls(
      path=as_id(gd_config$project_folder), 
      pattern=gsub('.', '\\.', fixed=TRUE, x=gsub('/', '|', relative_path)),
      recursive=TRUE)
  ) %>%
    dplyr::mutate(parents=lapply(drive_resource, function(dr) unlist(dr$parents))) %>%
    tidyr::unnest(parents) # make it a single row per item-parent combination
  
  # navigate from the outermost directory down to the file to identify the file
  # by both its name and its directory location
  path_elements <- strsplit(relative_path, split='/')[[1]]
  path_df <- filter(relevant_files, id==as_id(gd_config$project_folder))
  for(i in seq_along(path_elements)) {
    elem <- path_elements[i]
    parent <- path_df[[i,'id']]
    elem_row <- filter(relevant_files, name==elem, parents==parent)
    if(nrow(elem_row) == 1) {
      path_exists <- TRUE
      path_df <- bind_rows(path_df, elem_row)
    } else {
      path_exists <- FALSE
      path_df <- bind_rows(path_df, data_frame(id=NA))
      break
    }
  }
  
 return(path_df)
}

get_relative_path <- function(file) {
  file %>% 
    normalizePath(winslash='/', mustWork=FALSE) %>%
    gsub(normalizePath(getwd(), winslash='/'), '', .) %>% # remove the working directory if present
    gsub('^/', '', .) # remove the leading slash if present
}

#' List the Google Drive objects for this project
#' 
#' List the Google Drive objects in the project bucket/path as given in config_file
#' 
#' @param ... arguments passed to googledrive::drive_ls
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
gd_list <- function(..., config_file=options("scipiper.gd_config_file")[[1]]) {
  
  require_libs('googledrive')
  
  message("Listing project files on Google Drive")
  gd_config <- yaml::yaml.load_file(config_file)
  folder_df <- googledrive::drive_ls(path=as_id(gd_config$project_folder), ...)

  return(folder_df)  
}

#' Check whether a file is on Google Drive, and if so, write an indicator file
#' 
#' @param data_file character name of the data file to download (downloads the
#'   Google Drive object whose key equals the data_file basename)
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
gd_confirm_posted <- function(data_file, ind_file, config_file=options("scipiper.gd_config_file")[[1]]) {
  
  # look on Google Drive for the specified file
  gd_config <- yaml::yaml.load_file(config_file)
  key <- file.path(gd_config$path, basename(ind_file))
  Key <- '.dplyr.var'
  remote.info <- filter(gd_list(config_file=config_file, prefix=key), Key==key)
  if(nrow(remote.info) != 1) stop(paste0("failed to find exactly 1 Google Drive file with Key=", key))
  
  gd_make_indicator(ind_file, remote_time=remote.info$LastModified)
}

#' Write an Google Drive indicator file
#' 
#' Write an indicator file using a standard format for gd files
#' 
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @keywords internal
gd_make_indicator <- function(ind_file, remote_time) {
  
  # write the cache file
  if(is.character(remote_time)) remote_time <- gd_read_time(remote_time)
  writeLines(remote_time, con=ind_file)
  
}

#' Reads a datetime as returned from AWS Google Drive
#' 
#' Reads in an AWS Google Drive standard datetime into POSIXct
#' 
#' @param datetime character datetime as from an aws.gd call
#' @return POSIXct datetime
#' @keywords internal
gd_read_time <- function(datetime) {
  as.POSIXct(datetime, format='%Y-%m-%dT%H:%M:%S.000Z', tz='UTC')
}
