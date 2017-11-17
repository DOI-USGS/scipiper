# These functions download or upload any file you want, using the
# project-specific configuration for an S3 bucket and subfolder

#' Create or overwrite an S3 configuration file
#'
#' @param bucket character name of an S3 bucket
#' @param path character name of a path (prefix) within the bucket where all
#'   files for this project are to be stored
#' @param profile character name of an S3 profile to use
#' @param config_file character name of the yml file where this configuration
#'   information should be written
#' @export
s3_config <- function(path, bucket, profile='default', config_file="lib/cfg/s3_config.yml") {
  # write the given information to the specified config_file
  cfg <- list(bucket=bucket, path=path, profile=profile)
  if(!dir.exists(dirname(config_file))) dir.create(dirname(config_file), recursive=TRUE)
  writeLines(yaml::as.yaml(cfg), config_file)
  
  # check for credentials
  cred_file <- aws.signature::default_credentials_file()
  if(!file.exists(cred_file)) {
    warning(paste0("aws.signature expects credentials at ", cred_file, " - see ??read_credentials to create this file"))
  }
}

#' Upload a file to S3
#'
#' Upload (create or overwrite) a file to the project bucket and path.
#'
#' @param data_file character name of the data file to upload. The basename will
#'   be used as the S3 key and must be unique within the project (bucket & path)
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
s3_put <- function(data_file, ind_file, config_file="lib/cfg/s3_config.yml") {
  
  require_libs('aws.signature', 'aws.s3')
  
  # post the file from the local data_file to S3
  s3_config <- yaml::yaml.load_file(config_file)
  message("Uploading ", data_file, " to S3")
  aws.signature::use_credentials(profile = s3_config$profile)
  key <- file.path(s3_config$path, basename(data_file))
  success <- aws.s3::put_object(
    file = data_file,
    object = key, 
    bucket = s3_config$bucket)
  
  # write the indicator file (involves another check on S3 to get the timestamp)
  if(success) {
    success <- s3_confirm_posted(data_file=data_file, ind_file=ind_file, config_file=config_file)
  }
  if(!success) {
    stop(paste0("Data file could not be posted to S3: ", data_file))
  }
  return(success)
}

#' Download a file from S3
#'
#' Download a file from S3 to the local project
#'
#' @param data_file character name of the data file to download (downloads the
#'   S3 object whose key equals the data_file basename)
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
s3_get <- function(data_file, config_file="lib/cfg/s3_config.yml") {
  
  require_libs('aws.signature', 'aws.s3')
  
  # download the file from S3 to the local data_file
  message("Downloading ", data_file, " from S3")
  s3_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = s3_config$profile)
  key <- file.path(s3_config$path, basename(data_file))
  aws.s3::save_object(
    object = key, 
    bucket = s3_config$bucket,
    file = data_file)
}

#' List the S3 objects for this project
#' 
#' List the S3 objects in the project bucket/path as given in config_file
#' 
#' @param ... arguments passed to aws.s3::get_bucket_df
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
s3_list <- function(..., config_file="lib/cfg/s3_config.yml") {
  
  require_libs('aws.signature', 'aws.s3')
  
  message("Listing project files on S3")
  s3_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = s3_config$profile)
  bucket_df <- aws.s3::get_bucket_df(bucket=s3_config$bucket, prefix=s3_config$path)
  Key <- '.dplyr.var'
  dplyr::filter(bucket_df, grepl(sprintf("^%s/.+", s3_config$path), Key))
  
}

#' Check whether a file is on S3, and if so, write an indicator file
#' 
#' @param data_file character name of the data file to download (downloads the
#'   S3 object whose key equals the data_file basename)
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @export
s3_confirm_posted <- function(data_file, ind_file, config_file="lib/cfg/s3_config.yml") {
  
  # look on S3 for the specified file
  s3_config <- yaml::yaml.load_file(config_file)
  key <- file.path(s3_config$path, basename(ind_file))
  Key <- '.dplyr.var'
  remote.info <- filter(s3_list(config_file=config_file, prefix=key), Key==key)
  if(nrow(remote.info) != 1) stop(paste0("failed to find exactly 1 S3 file with Key=", key))
  
  s3_make_indicator(ind_file, remote_time=remote.info$LastModified)
}

#' Write an S3 indicator file
#' 
#' Write an indicator file using a standard format for s3 files
#' 
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @keywords internal
s3_make_indicator <- function(ind_file, remote_time) {
  
  # write the cache file
  if(is.character(remote_time)) remote_time <- s3_read_time(remote_time)
  writeLines(remote_time, con=ind_file)
  
}

#' Reads a datetime as returned from AWS S3
#' 
#' Reads in an AWS S3 standard datetime into POSIXct
#' 
#' @param datetime character datetime as from an aws.s3 call
#' @return POSIXct datetime
#' @keywords internal
s3_read_time <- function(datetime) {
  as.POSIXct(datetime, format='%Y-%m-%dT%H:%M:%S.000Z', tz='UTC')
}
