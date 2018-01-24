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
s3_config <- function(path, bucket, profile='default', config_file=getOption("scipiper.s3_config_file")) {
  # write the given information to the specified config_file
  cfg <- list(bucket=bucket, path=path, profile=profile)
  if(!dir.exists(dirname(config_file))) dir.create(dirname(config_file), recursive=TRUE)
  writeLines(yaml::as.yaml(cfg), config_file)
  
  # check for credentials
  cred_file <- aws.signature::default_credentials_file()
  if(!file.exists(cred_file)) {
    warning(paste0("aws.signature expects credentials at ", cred_file, " - see ??read_credentials to create this file"))
  }
  
  if(config_file != getOption("scipiper.s3_config_file")) {
    warning("config_file != default; consider setting options('scipiper.s3_config_file') in .Rprofile")
  }
}

#' Upload a file to S3
#'
#' Upload (create or overwrite) a file to the project bucket and path. Writes an
#' indicator file exactly corresponding to the data_file path and name (but with
#' indicator file extension).
#'
#' @param remote_ind character name of the indicator file to write locally, but
#'   which describes the status of the remote file once the file has been
#'   uploaded by this function. The remote data file will have a name
#'   corresponding to this ind_file (without the indicator extension, but with
#'   same path and basename).
#' @param local_source character name of EITHER a data file to upload OR the
#'   indicator file of a data file to upload. Using the same value for both
#'   remote_ind and local_source (or setting local_source to the data file name
#'   corresponding to the indicator in remote_ind) will only work (in remake) if
#'   you are calling `s3_put` from within the same function that created the
#'   data_file. If instead you have separate recipes for (a) creating the
#'   original data_file, (b) posting the data_file, and (c) retrieving the
#'   data_file from S3, then the 'a' and 'c' recipes must have
#'   different targets and this function's local_source argument should match
#'   the target of the 'a' recipe while this function's remote_ind argument
#'   should match the target of this recipe (=='b') and the data_file target of
#'   the 'c' recipe. See the examples.
#' @param mock_get character. if remote_ind and local_source imply different
#'   local file locations, should the current local file (implied by
#'   local_source) be left alone ('none'), or copied ('copy') or moved ('move')
#'   to the location implied by remote_ind? If 'copy' or 'move' are used, and if
#'   gd_get will be called in an upcoming command, this argument may help to
#'   avoid an unnecessary download from S3 back to this computer
#'   because `s3_get` skips the download if there's already a local file in the
#'   right place with the right contents (MD5 hash).
#' @param on_exists what to do if the file already exists - replace, or
#'   throw an error? the default is to replace
#' @param verbose logical Used in this function and passed through aws.s3::put_object to
#'   aws.s3::s3HTTP
#' @param config_file character name of the YAML file containing
#'   project-specific configuration information for S3
#' @param ind_ext the indicator file extension to expect at the end of
#'   remote_ind
#' @export
s3_put <- function(remote_ind, local_source,  mock_get=c('copy','move','none'),
                   on_exists=c('replace','stop'), verbose = FALSE,
                  config_file=getOption("scipiper.s3_config_file"),
                  ind_ext=getOption("scipiper.ind_ext")) {
  
  warning('s3_put is out of date relative to gd_put') # need to update s3_xx with lessons learned from gd_xx
  
  # check arguments
  mock_get <- match.arg(mock_get)
  ind_file <- as_ind_file(data_file, ind_ext=ind_ext)
  
  # decide whether local_source is an indicator or data file and find the data file 
  # if it is an indicator
  local_file <- check_local_source(local_source, ind_ext)
  
  # identify the remote data file to be indicated by remote_ind
  data_file <- as_data_file(remote_ind, ind_ext=ind_ext)
  
  # prepare to use S3
  require_libs('aws.signature', 'aws.s3')
  s3_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = s3_config$profile)
  
  # determine whether and where the remote file exists
  bucket_contents <- get_bucket_df(bucket = s3_config$bucket)
  exists_on_s3 <- local_file %in% bucket_contents$Key
  
  #upload to S3 - note that S3 is a flat file system, so folders don't need
  #to be created.  paths are just part of object keys
  if(!exists_on_s3) {
    if(verbose) message("Uploading ", local_file, " to S3")
    success <- aws.s3::put_object(file = local_file, object = local_file, 
                                  bucket = s3_config$bucket)
  }
  
  message("Uploading ", data_file, " to S3")
  
  
  
  
  # write the indicator file (involves another check on S3 to get the timestamp)
  if(success) {
    success <- s3_confirm_posted(data_file=data_file, ind_file=ind_file, config_file=config_file, ind_ext=ind_ext)
  }
  if(!success) {
    stop(paste0("Data file could not be posted to S3: ", data_file))
  }
  return(success)
}

#' decide whether local_source is an indicator or data file and find the data file 
#' if it is an indicator
#' @keywords internals
check_local_source <- function(local_source, ind_ext) {
  if(is_ind_file(local_source)) {
    local_file <- as_data_file(local_source, ind_ext=ind_ext)
  } else {
    local_file <- local_source
  }
  if(!file.exists(local_file)) {
    stop(paste('data file matching local_source does not exist:', local_file))
  }
  return(local_file)
}




#' Download a file from S3
#'
#' Download a file from S3 to the local project
#'
#' @param data_file character name of the data file to download (downloads the
#'   S3 object whose key equals the data_file basename)
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @export
s3_get <- function(
  data_file,
  config_file=getOption("scipiper.s3_config_file"),
  ind_ext=getOption("scipiper.ind_ext")) {
  
  warning('s3_get is out of date relative to gd_get') # need to update s3_xx with lessons learned from gd_xx
  
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
s3_list <- function(..., config_file=getOption("scipiper.s3_config_file")) {
  
  warning('s3_list is out of date relative to gd_list') # need to update s3_xx with lessons learned from gd_xx
  
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
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @export
s3_confirm_posted <- function(
  ind_file,
  config_file=getOption("scipiper.s3_config_file"),
  ind_ext=getOption("scipiper.ind_ext")) {
  
  warning('s3_confirm_posted is out of date relative to gd_confirm_posted') # need to update s3_xx with lessons learned from gd_xx
  
  # look on S3 for the specified file
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  s3_config <- yaml::yaml.load_file(config_file)
  key <- file.path(s3_config$path, basename(ind_file))
  Key <- '.dplyr.var'
  remote.info <- filter(s3_list(config_file=config_file, prefix=key), Key==key)
  if(nrow(remote.info) != 1) stop(paste0("failed to find exactly 1 S3 file with Key=", key))
  
  s3_indicate(ind_file, remote_time=remote.info$LastModified)
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
s3_indicate <- function(ind_file, remote_time) {
  
  warning('s3_indicate is out of date relative to gd_indicate') # need to update s3_xx with lessons learned from gd_xx
  
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
  as.POSIXct(datetime, format='%Y-%m-%dT%H:%M:%OSZ', tz='UTC')
}
