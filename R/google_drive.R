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
#'   be used as the Google Drive key and must be unique within the project (bucket & path)
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
gd_put <- function(data_file, ind_file, config_file=options("scipiper.gd_config_file")[[1]]) {
  
  scmake(as_data_file(ind_file))
  
  require_libs('googledrive')
  
  # post the file from the local data_file to Google Drive
  gd_config <- yaml::yaml.load_file(config_file)
  message("Uploading ", data_file, " to Google Drive")
  aws.signature::use_credentials(profile = gd_config$profile)
  key <- file.path(gd_config$path, basename(data_file))
  success <- aws.gd::put_object(
    file = data_file,
    object = key, 
    bucket = gd_config$bucket)
  
  # write the indicator file (involves another check on Google Drive to get the timestamp)
  if(success) {
    success <- gd_confirm_posted(data_file=data_file, ind_file=ind_file, config_file=config_file)
  }
  if(!success) {
    warning(paste0("Data file could not be posted to Google Drive: ", data_file))
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
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
gd_get <- function(indicator, config_file=options("scipiper.gd_config_file")[[1]]) {
  
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
  
  # download the file from Google Drive to the local data_file
  message("Downloading ", data_file, " from Google Drive")
  gd_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = gd_config$profile)
  key <- file.path(gd_config$path, basename(data_file))
  aws.gd::save_object(
    object = key, 
    bucket = gd_config$bucket,
    file = data_file)
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
  folder_df <- googledrive::drive_ls(path=gd_config$folder, ...)

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
