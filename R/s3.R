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
  
  s3_config <- yaml.load_file(s3.config)
  key <- file.path(s3_config$s3Path, basename(file.name))
  remote.info <- filter(list_s3(prefix=key), Key==key)
  if(nrow(remote.info) != 1) stop(paste0("failed to find exactly 1 S3 file with Key=", key))
  
  # write the cache file if possible
  stamp_cache(
    target=paste0(file.name, '.s3'), 
    remote_time=read_s3_time(remote.info$LastModified),
    by=remote.info$Owner_DisplayName)
  
}

#' @export
post_s3 <- function(file.name, s3.config="lib/cfg/s3_config.yaml", s3.post="lib/cfg/s3_post.yaml") {
  
  s3_post <- yaml.load_file(s3.post)
  if(s3_post$post) {
    # download the file from S3 to the local file.name
    s3_config <- yaml.load_file(s3.config)
    message("Uploading ", file.name, " to S3 because s3_post.yaml says 'post: TRUE'")
    aws.signature::use_credentials(profile = s3_config$s3Profile)
    key <- file.path(s3_config$s3Path, basename(file.name))
    success <- aws.s3::put_object(
      file = file.name,
      object = key, 
      bucket = s3_config$bucket)
    
    # write the indicator file (but only if(s3_post))
    if(success) {
      remote_time <- read_s3_time(filter(list_s3(prefix=key), Key==key)$LastModified)
      stamp_cache(target=paste0(file.name, '.s3'), remote_time=remote_time)
    }

  } else {
    message("Skipping upload of ", file.name, " to S3 because s3_post.yaml says 'post: FALSE'")
  }
  
}

#' @export
get_s3 <- function(file.name, s3.config="lib/cfg/s3_config.yaml") {
  
  message("Downloading ", file.name, " from S3")
  s3_config <- yaml.load_file(s3.config)
  aws.signature::use_credentials(profile = s3_config$s3Profile)
  key <- file.path(s3_config$s3Path, basename(file.name))
  aws.s3::save_object(
    object = key, 
    bucket = s3_config$bucket,
    file = file.name)
  
  # write the indicator file
  remote_time <- read_s3_time(filter(list_s3(prefix=key), Key==key)$LastModified)
  stamp_get(target=paste0(file.name, '.s3'), remote_time=remote_time)
  
}

#' @export
list_s3 <- function(s3.config="lib/cfg/s3_config.yaml", prefix="s3Path") {
  
  message("Listing project files on S3")
  s3_config <- yaml::yaml.load_file(s3.config)
  if(prefix=="s3Path") prefix <- s3_config$s3Path
  aws.signature::use_credentials(profile = s3_config$s3Profile)
  bucket_df <- aws.s3::get_bucket_df(bucket=s3_config$bucket, prefix=prefix)
  dplyr::filter(bucket_df, grepl(sprintf("^%s/.+", s3_config$s3Path),Key))
  
}

#' @export
inventory_s3 <- function(s3.inventory="lib/out/s3_inventory.tsv", s3.config="lib/cfg/s3_config.yaml") {
  
  cache_df <- list_s3()
  write.table(cache_df, file=s3.inventory, sep='\t', row.names=FALSE)
  
}
