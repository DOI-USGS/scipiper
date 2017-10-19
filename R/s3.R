# These functions download or upload any file you want, using the
# project-specific configuration for an S3 bucket and subfolder

# several functions require functions from lib/src/status.R, but that file is
# not sourced here because it should be declared as a dependency in the makefile
# recipe (or sources, for remake) instead

library(aws.signature)
library(aws.s3)
library(yaml)
library(dplyr)

#' @export
read_s3_time <- function(datetime) {
  as.POSIXct(datetime, format='%Y-%m-%dT%H:%M:%S.000Z', tz='UTC')
}

#' @export
make_s3_indicator <- function(file.name, s3.config="lib/cfg/s3_config.yaml") {
  
  s3_config <- yaml::yaml.load_file(s3.config)
  key <- file.path(s3_config$s3Path, basename(file.name))
  Key <- '.dplyr.var'
  remote.info <- filter(list_s3(s3.config=s3.config, prefix=key), Key==key)
  if(nrow(remote.info) != 1) stop(paste0("failed to find exactly 1 S3 file with Key=", key))
  
  # write the cache file if possible
  writeLines(
    con=paste0(file.name, '.s3'), 
    c(remote_time=read_s3_time(remote.info$LastModified),
      by=remote.info$Owner_DisplayName))
  
}

#' @export
post_s3 <- function(file.name, s3.config="lib/cfg/s3_config.yaml", s3.post="lib/cfg/s3_post.yaml") {
  
  s3_post <- yaml::yaml.load_file(s3.post)
  if(s3_post$post) {
    # download the file from S3 to the local file.name
    s3_config <- yaml::yaml.load_file(s3.config)
    message("Uploading ", file.name, " to S3 because s3_post.yaml says 'post: TRUE'")
    aws.signature::use_credentials(profile = s3_config$s3Profile)
    key <- file.path(s3_config$s3Path, basename(file.name))
    success <- aws.s3::put_object(
      file = file.name,
      object = key, 
      bucket = s3_config$bucket)
    
    # write the indicator file (but only if(s3_post))
    if(success) {
      Key <- '.dplyr.var'
      remote_time <- read_s3_time(filter(list_s3(prefix=key), Key==key)$LastModified)
      writeLines(con=paste0(file.name, '.s3'), c(remote_time=remote_time))
    }

  } else {
    message("Skipping upload of ", file.name, " to S3 because s3_post.yaml says 'post: FALSE'")
  }
  
}

#' @export
get_s3 <- function(file.name, s3.config="lib/cfg/s3_config.yaml") {
  
  message("Downloading ", file.name, " from S3")
  s3_config <- yaml::yaml.load_file(s3.config)
  aws.signature::use_credentials(profile = s3_config$s3Profile)
  key <- file.path(s3_config$s3Path, basename(file.name))
  aws.s3::save_object(
    object = key, 
    bucket = s3_config$bucket,
    file = file.name)
  
  # write the indicator file
  Key <- '.dplyr.var'
  remote_time <- read_s3_time(dplyr::filter(list_s3(prefix=key), Key==key)$LastModified)
  write_indicator(indicator_file=paste0(file.name, '.s3'), time=remote_time)
  
}

#' @export
list_s3 <- function(s3.config="lib/cfg/s3_config.yaml", prefix="s3Path") {
  
  message("Listing project files on S3")
  s3_config <- yaml::yaml.load_file(s3.config)
  if(prefix=="s3Path") prefix <- s3_config$s3Path
  aws.signature::use_credentials(profile = s3_config$s3Profile)
  bucket_df <- aws.s3::get_bucket_df(bucket=s3_config$bucket, prefix=prefix)
  Key <- '.dplyr.var'
  dplyr::filter(bucket_df, grepl(sprintf("^%s/.+", s3_config$s3Path), Key))
  
}
