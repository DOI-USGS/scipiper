# These functions download or upload any file you want, using the
# project-specific configuration for an S3 bucket and subfolder

#' Create or overwrite an S3 configuration file
#' 
#' It is assumed that the bucket corresponds 1:1 with the local project directory
#' structure, so all object keys will be the same as the local file path from the
#' root of the project directory.
#' 
#' @param bucket character name of an S3 bucket
#' @param profile character name of an S3 profile to use
#' @param config_file character name of the yml file where this configuration
#'   information should be written
#' @export
s3_config <- function(bucket, profile='default', config_file=getOption("scipiper.s3_config_file")) {
  # write the given information to the specified config_file
  cfg <- list(bucket=bucket, profile=profile)
  if(!dir.exists(dirname(config_file))) dir.create(dirname(config_file), recursive=TRUE)
  readr::write_lines(yaml::as.yaml(cfg), config_file)
  
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
#' Upload (create or overwrite) a file to the project bucket. Writes an
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
  
  # check arguments
  mock_get <- match.arg(mock_get)
  
  # decide whether local_source is an indicator or data file and find the data file 
  # if it is an indicator
  local_file <- find_local_file(local_source, ind_ext)
  
  # identify the remote data file to be indicated by remote_ind
  data_file <- as_data_file(remote_ind, ind_ext=ind_ext)
  
  # prepare to use S3
  require_libs('aws.signature', 'aws.s3')
  s3_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = s3_config$profile)
  
  # determine whether and where the remote file exists
  bucket_contents <- aws.s3::get_bucket_df(bucket = s3_config$bucket)
  exists_on_s3 <- local_file %in% bucket_contents$Key
  
  #upload to S3 - note that S3 is a flat file system, so folders don't need
  #to be created.  'Directories' are just part of object keys
  match.arg(on_exists)
  if(exists_on_s3 && on_exists == "stop") {
    stop('File already exists and on_exists==stop')
  } else {
    if(verbose) message("Uploading ", local_file, " to S3, overwriting = ", exists_on_s3)
    status <- aws.s3::put_object(file = local_file, object = data_file, 
                                 bucket = s3_config$bucket)
  }
  
  # write the indicator file (involves another check on S3 to get the timestamp)
  success <- s3_confirm_posted(ind_file=remote_ind, config_file=config_file, 
                               ind_ext=ind_ext)
  mock_move_copy(mock_get, local_file, data_file)
  
  return(success)
}

#' decide whether local_source is an indicator or data file and find the data file 
#' if it is an indicator
#' @keywords internal
find_local_file <- function(local_source, ind_ext) {
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
#' @param ind_file character name of the indicator file for which data should be
#'   downloaded. downloads the S3 object whose key equals the data_file basename
#' @param verbose logical  used to determine whether to include messages specific to `s3_get()`
#' @param config_file character name of the yml file containing project-specific
#'   configuration information
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @export
s3_get <- function(ind_file, verbose = FALSE,
                   config_file=getOption("scipiper.s3_config_file"),
                   ind_ext=getOption("scipiper.ind_ext")) {
  require_libs('aws.signature', 'aws.s3')
  # infer the data file name from the ind_file. gd_get always downloads to that
  # location if it downloads at all
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  
  # bypass the download from s3 if the right local file already exists
  if(file.exists(data_file)) {
    remote_hash <- yaml::yaml.load_file(ind_file)$hash
    local_hash <- unname(tools::md5sum(data_file))
    if(remote_hash == local_hash) return(data_file)
  }
  
  # download the file from S3 to the local data_file
  # will fail if it doesn't exist
  if(verbose) {message("Downloading ", data_file, " from S3")}
  
  s3_config <- yaml::yaml.load_file(config_file)
  aws.signature::use_credentials(profile = s3_config$profile)
  aws.s3::save_object(object = data_file, bucket = s3_config$bucket,
                      file = data_file)
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
  
  # look on S3 for the specified file
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  s3_config <- yaml::yaml.load_file(config_file)
  bucket_contents <- aws.s3::get_bucket_df(bucket = s3_config$bucket)
  remote_info <- filter(bucket_contents, Key == data_file)
  if(nrow(remote_info) == 0) {
    stop(paste0("failed to find S3 file with Key=", data_file))
  } else {
    sc_indicate(ind_file, hash = gsub(pattern = "\"", x = remote_info$ETag, 
                                      replacement = ""))
    return(TRUE)
  }
}

