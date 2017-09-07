as_timestamp <- function(datetime) {
  if(is.character(datetime)) datetime <- as.POSIXct(datetime)
  format(datetime, tz='UTC', format='%Y-%m-%d %H:%M:%S 0000')
}

# read existing status indicator file, if present
read_stamps <- function(target) {
  status <- if(file.exists(target)) {
    yaml::yaml.load_file(target)
  } else {
    list()
  }
  return(status)
}

stamp_build <- function(target, dependencies) {
  
  # read existing status indicator file, if present
  status <- read_stamps(target)
  
  # extract dependency information. for each dependency, copy its status file or
  # glean other info
  from <- do.call(c, lapply(dependencies, function(dep) {
    is_stamp_file <- tools::file_ext(dep) %in% c('s3','loc')
    if(is_stamp_file) {
      get.file <- file.path(dirname(dirname(normalizePath(dep, mustWork=FALSE))), 'log', paste0(basename(dep), '.yaml'))
      setNames(
        list(yaml::yaml.load_file(if(file.exists(get.file)) get.file else dep)),
        gsub(paste0('\\.',tools::file_ext(dep),'$'), '', dep))
    } else {
      setNames(
        list(list(modified = as_timestamp(file.info(dep)$mtime))),
        dep)
    }
  }))
  
  # replace the build information
  status$built <- list(
    at = as_timestamp(Sys.time()),
    by = Sys.getenv('USERNAME'),
    from = from
  )
  
  # write to file
  writeLines(yaml::as.yaml(status), target)
}

# for recording files that magically appeared on S3 or that we just posted there
stamp_cache <- function(target, remote_time=NA, by=Sys.getenv('USERNAME')) {
  
  # read existing status indicator file, if present
  status <- read_stamps(target)
  
  # replace the post information
  status$cached <- list(
    remote.at = as_timestamp(remote_time),
    by = by
  )
  
  # write to file
  writeLines(yaml::as.yaml(status), target)
  
}

stamp_get <- function(target, remote_time=NA) {

  # read existing status indicator file, if present
  status <- read_stamps(target)
  
  # replace the post information
  status$retrieved <- list(
    remote.at = as_timestamp(remote_time),
    local.at = as_timestamp(Sys.time()),
    by = Sys.getenv('USERNAME')
  )
  
  # write to file -- a .yml file, not the status indicator file, because this is
  # user-specific info. put it in the 'log' directory that is a sibling to the
  # target's current directory
  yaml.file <- file.path(dirname(dirname(normalizePath(target, mustWork=FALSE))), 'log', paste0(basename(target), '.yaml'))
  writeLines(yaml::as.yaml(status), yaml.file)
  
}
