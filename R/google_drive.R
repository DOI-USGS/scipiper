# These functions download or upload any file you want, using the
# project-specific configuration for a Google Drive folder

#' Create or overwrite a Google Drive configuration file
#'
#' @param folder character name of the Google Drive folder where all
#'   files for this project are to be stored, nested in a file structure
#'   parallel to the local project file structure
#' @param config_file character name of the YAML file where this configuration
#'   information should be written
#' @export
gd_config <- function(folder, config_file=getOption("scipiper.gd_config_file")) {
  # write the given information to the specified config_file
  cfg <- list(folder=folder)
  if(!dir.exists(dirname(config_file))) dir.create(dirname(config_file), recursive=TRUE)
  writeLines(yaml::as.yaml(cfg), config_file)
  
  # check for credentials
  cred_file <- '.httr-oauth'
  if(!file.exists(cred_file)) {
    warning(paste0("googledrive expects credentials at ", cred_file, " - see ??drive_auth to create this file"))
  }
  
  if(config_file != getOption("scipiper.gd_config_file")) {
    warning("config_file != default; consider setting options('scipiper.gd_config_file') in .Rprofile")
  }
}

#' Upload a file to Google Drive
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
#'   you are calling `gd_put` from within the same function that created the
#'   data_file. If instead you have separate recipes for (a) creating the
#'   original data_file, (b) posting the data_file, and (c) retrieving the
#'   data_file from google drive, then the 'a' and 'c' recipes must have
#'   different targets and this function's local_source argument should match
#'   the target of the 'a' recipe while this function's remote_ind argument
#'   should match the target of this recipe (=='b') and the data_file target of
#'   the 'c' recipe. See the examples.
#' @param mock_get character. if remote_ind and local_source imply different
#'   local file locations, should the current local file (implied by
#'   local_source) be left alone ('none'), or copied ('copy') or moved ('move')
#'   to the location implied by remote_ind? If 'copy' or 'move' are used, and if
#'   gd_get will be called in an upcoming command, this argument may help to
#'   avoid an unnecessary download from Google Drive back to this computer
#'   because `gd_get` skips the download if there's already a local file in the
#'   right place with the right contents (MD5 hash).
#' @param on_exists what to do if the file already exists - update, replace, or
#'   throw an error? the default is to update (using google drive's versioning
#'   functionality)
#' @param type media type as passed to drive_upload or drive_update
#' @param verbose logical, used in gd_put and passed onto
#'   googledrive::drive_update, drive_upload, and/or drive_rm
#' @param config_file character name of the YAML file containing
#'   project-specific configuration information for Google Drive
#' @param ind_ext the indicator file extension to expect at the end of
#'   remote_ind
#' @export
#' @examples
#' \dontrun{
#' #### using 2 recipes
#'
#' ## remake file
#' # create and post 1_data/out/mydata.rds (and an indicator for it) at once
#' 1_data/out/mydata.rds.ind:
#'   command: create_and_post_mydata(target_name)
#' # retrieve data file on demand
#' 1_data/out/mydata.rds:
#'   command: gd_get('1_data/out/mydata.rds.ind')
#'
#' ## function definitions
#' create_and_post_mydata <- function(ind_file) {
#'   # create data file (no need to make indicator yet)
#'   data_file <- as_data_file(ind_file)
#'   mydata # <- ...compute mydata here...
#'   write.csv(mydata, data_file)
#'   # post and create indicator file
#'   gd_put(remote_ind=ind_file, local_source=data_file)
#' }
#'
#' #### using 3 recipes
#'
#' ## remake file
#' # create 1_data/cache/mydata.rds (and an indicator for it) locally
#' 1_data/tmp/mydata.rds.ind:
#'   command: create_mydata(target_name)
#' # post 1_data/cache/mydata.rds to 1_data/out/mydata.rds on Drive
#' 1_data/out/mydata.rds.ind:
#'   command: gd_put(remote_ind=target_name, local_source='1_data/tmp/mydata.rds.ind')
#' # retrieve data file on demand
#' 1_data/out/mydata.rds:
#'   command: gd_get('1_data/out/mydata.rds.ind')
#'
#' ## function definitions
#' create_mydata <- function(ind_file) {
#'   data_file <- as_data_file(ind_file)
#'   mydata # <- ...compute mydata here...
#'   write.csv(mydata, data_file)
#'   sc_indicate(ind_file, data_file=data_file)
#' }
#'
#' }
gd_put <- function(
  remote_ind, local_source, mock_get=c('copy','move','none'),
  on_exists=c('update','replace','stop'), type=NULL, verbose=FALSE,
  config_file=getOption("scipiper.gd_config_file"),
  ind_ext=getOption("scipiper.ind_ext")) {
  
  # check arguments
  mock_get <- match.arg(mock_get)
  
  # decide whether local_source is an indicator or data file and find the data file
  local_file <- find_local_file(local_source, ind_ext)
  
  # identify the remote data file to be indicated by remote_ind
  data_file <- as_data_file(remote_ind, ind_ext=ind_ext)
  
  # prepare to use google drive
  require_libs('googledrive')
  gd_config <- yaml::yaml.load_file(config_file)
  
  # determine whether and where the remote file exists
  remote_path <- gd_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  
  # determine the last known parent, which is either already or soon to be the
  # proximate parent of the item to create
  parent <- remote_path %>% slice(nrow(remote_path)-1) %>% pull(id)
  
  # create the parent folder[s] on google drive as needed
  if(is.na(remote_id)) {
    # determine what exists
    remote_dirs <- remote_path %>%
      slice(-1) %>%slice(-nrow(.)) %>% # chop off the top parent-dir row and the bottom all-NA row
      pull(name)
    final_dirs <- strsplit(dirname(get_relative_path(data_file)), split='/')[[1]]
    
    # double-check that any overlapping path elements agree
    stopifnot(all.equal(final_dirs[seq_along(remote_dirs)], remote_dirs))
    
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
        parent <- googledrive::drive_mkdir(name=needed_dirs[i], parent=googledrive::as_id(parent))$id
      }
    }
  }
  
  # post the file (create or update) from the local data file to Google Drive
  if(is.na(remote_id)) {
    if(verbose) message("Uploading ", local_file, " to Google Drive")
    remote_id <- googledrive::drive_upload(media=local_file, path=googledrive::as_id(parent), type=type, verbose=verbose)$id
  } else {
    on_exists <- match.arg(on_exists)
    switch(
      on_exists,
      update={
        if(verbose) message("Updating ", local_file, " on Google Drive")
        remote_id <- googledrive::drive_update(googledrive::as_id(remote_id), media=local_file, verbose=verbose)$id
      },
      replace={
        if(verbose) message("Replacing ", local_file, " on Google Drive")
        googledrive::drive_rm(googledrive::as_id(remote_id), verbose=verbose)
        remote_id <- googledrive::drive_upload(media=local_file, path=googledrive::as_id(parent), type=type, verbose=verbose)$id
      },
      stop={
        stop('File already exists and on_exists==stop')
      }
    )
  }
  
  # write the indicator file (involves another check on Google Drive)
  gd_confirm_posted(ind_file=remote_ind, config_file=config_file)
  
  # if posting was successful, potentially bypass a superfluous download from
  # google drive by copying or moving local_file to data_file (the gd_get
  # destination)
  mock_move_copy(mock_get, local_file, data_file)
  
  invisible()
}

mock_move_copy <- function(mock_get, local_file, data_file) {
  if(mock_get %in% c('copy','move')) {
    if(data_file != local_file) {
      file.copy(from=local_file, to=data_file, overwrite=TRUE)
      if(mock_get == 'move') {
        file.remove(local_file)
      }
    }
  }
}

#' Download a file from Google Drive if needed
#'
#' Download a file from Google Drive to the local project based on the
#' information implied by the indicator file (including the location on google
#' drive and the local destination location). Skips the download if the local
#' file already exists and the remote and local hashes are identical.
#'
#' @param ind_file character name of the indicator file for which data should be
#'   downloaded. downloads the Google Drive object whose key equals the
#'   data_file basename
#' @param type see `type` argument to `googledrive::drive_download()`
#' @param overwrite see `overwrite` argument to `googledrive::drive_download()`
#' @param verbose see `verbose` argument to `googledrive::drive_download()`;
#'   also used to determine whether to include messages specific to `gd_get()`
#' @param config_file character name of the YAML file containing
#'   project-specific configuration information for Google Drive
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @examples
#' \dontrun{
#' gd_get('0_test/test_sheet.ind', type='xlsx', overwrite=TRUE)
#' }
#' @export
gd_get <- function(ind_file, type=NULL, overwrite=TRUE, verbose=FALSE,
                   config_file=getOption("scipiper.gd_config_file"),
                   ind_ext=getOption("scipiper.ind_ext")) {
  
  # infer the data file name from the ind_file. gd_get always downloads to that
  # location if it downloads at all
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  
  # bypass the download from google drive if the right local file already exists
  if(file.exists(data_file)) {
    remote_hash <- yaml::yaml.load_file(ind_file)$hash
    local_hash <- unname(tools::md5sum(data_file))
    if(remote_hash == local_hash) return(data_file)
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
      file=googledrive::as_id(remote_id), path=data_file,
      type=type, overwrite=overwrite, verbose=verbose)
  } else {
    stop(paste0("Could not locate ", data_file, " for download from Google Drive"))
  }
  
  return(data_file)
}

# Locate a file along a path relative to the gd_config folder, or return NA if not found
gd_locate_file <- function(file, config_file=getOption("scipiper.gd_config_file")) {
  # load the project's googledrive configuration
  gd_config <- yaml::yaml.load_file(config_file)
  
  # normalize the relative path for this file so we can use in confidently as a
  # relative path from both the local working directory and the google drive
  # parent folder
  relative_path <- get_relative_path(file)
  
  # query google drive for all possibly relevant files and add their parents as
  # a simple column
  relevant_files <- bind_rows(
    googledrive::drive_get(
      id=googledrive::as_id(gd_config$folder)),
    googledrive::drive_ls(
      path=googledrive::as_id(gd_config$folder), 
      pattern=sprintf("^%s$", gsub('.', '\\.', fixed=TRUE, x=gsub('/', '$|^', relative_path))),
      recursive=TRUE)
  ) %>%
    dplyr::mutate(parents=lapply(drive_resource, function(dr) {
      parent <- unlist(dr$parents)
      if(is.character(parent)) parent else NA
    })) %>%
    tidyr::unnest(parents, .preserve=c('drive_resource')) # make it a single row per item-parent combination
  
  # navigate from the outermost directory down to the file to identify the file
  # by both its name and its directory location
  path_elements <- strsplit(relative_path, split='/')[[1]]
  path_df <- filter(relevant_files, id==googledrive::as_id(gd_config$folder))
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
#' @param config_file character name of the YAML file containing project-specific
#'   configuration information
#' @export
gd_list <- function(..., config_file=getOption("scipiper.gd_config_file")) {
  
  require_libs('googledrive')
  
  message("Listing project files on Google Drive")
  gd_config <- yaml::yaml.load_file(config_file)
  folder_df <- googledrive::drive_ls(path=googledrive::as_id(gd_config$folder), ...)
  
  return(folder_df)  
}

#' Check whether a file is on Google Drive, and if so, write an indicator file
#'
#' @param ind_file character name of the indicator file to write locally once
#'   the file has been uploaded; will exactly correspond to the data_file on GD
#' @param config_file character name of the YAML file containing
#'   project-specific configuration information
#' @param ind_ext the indicator file extension to expect at the end of ind_file
#' @export
gd_confirm_posted <- function(
  ind_file, config_file=getOption("scipiper.gd_config_file"),
  ind_ext=getOption("scipiper.ind_ext")) {
  
  require_libs('googledrive')
  
  # look on Google Drive for the specified file
  # figure out whether and where the file exists on gdrive
  data_file <- as_data_file(ind_file, ind_ext=ind_ext)
  remote_path <- gd_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  if(is.na(remote_id)) stop(paste('File was not found on Google Drive:', data_file))
  remote_info <- remote_path %>% slice(n()) %>% pull(drive_resource) %>% .[[1]]
  
  # we could prepare a timestamp from modifiedTime...but checksum is even better
  sc_indicate(ind_file, hash=remote_info$md5Checksum)
  
  return(TRUE)
}
