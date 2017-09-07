#' Formats a POSIXct timestamp with UTC into a character string
#' @export
POSIX2char <- function(ptime) {
  format(ptime, tz='UTC', format="%Y-%m-%d %H:%M:%S", usetz=TRUE)
}
#' Formats a character string into POSIXct timestamp with UTC
#' @export
char2POSIX <- function(stime) {
  as.POSIXct(strptime(stime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
}

#' Export status files from .remake folder (binary form) to status folder
#' (versionable text)
#' @export
export_remake_status <- function() {
  # ensure there's a directory to receive the export
  dir.create('status', showWarnings=FALSE)
  
  # get info about the remake project
  obj <- remake:::remake(load_sources=FALSE)
  dbstore <- obj$store$db
  
  # figure out which targets to export: we stick to files that have keys
  # existing in the .remake database, remake_db namespace. we avoid objects
  # because if we included them we'd need to share the objects among developers,
  # too, which sounds a lot like writing and sharing files but would require a
  # second system on top of the one we're already supporting. and no sense in
  # trying to export targets for which we have no .remake status
  rtargs <- remake:::list_targets(type='file')
  rstats <- dbstore$list()
  to_export <- intersect(rtargs, rstats)
  
  sfiles <- lapply(seq_along(to_export), function(i) {
    status <- dbstore$get(to_export[i])
    status$version <- as.character(status$version)
    status$time <- POSIX2char(status$time)
    
    status_yml <- yaml::as.yaml(status)
    status_key <- basename(dbstore$driver$name_key(to_export[i], ''))
    status_file <- file.path('status', paste0(status_key, '.yml'))
    writeLines(status_yml, status_file)
    
    return(status_file)
  })
  
  extra_sfiles <- setdiff(dir('status', full.names=TRUE), sfiles)
  if(length(extra_sfiles) > 0) warning(paste("these status files may be obsolete:", paste(extra_sfiles, collapse=", ")))
}

# import from versionable text to .remake binary
import_remake_status <- function() {
  # ensure there's a directory to receive the export
  if(!dir.exists('.remake')) {
    remake:::remake() # creates the .remake dir as a storr
  }
  
  # get info about the remake project
  obj <- remake:::remake(load_sources = FALSE)
  dbstore <- obj$store$db
  mangled <- function(key) {
    basename(dbstore$driver$name_key(key, ''))
  }
  storekeys <- mangled(dbstore$driver$list_keys(dbstore$driver$list_namespaces()))
  
  # figure out which targets to import: don't import targets that don't exist in the current remake dependency tree
  rtargs <- remake:::list_targets(type='file')
  sfiles <- dir('status', full.names=TRUE)
  skeys <- gsub('\\.yml$', '', basename(sfiles))
  stargs <- storr::decode64(skeys) # i think we can leave mangle_key_pad, etc. to defaults...
  to_import <- data.frame(target=stargs, mkey=skeys, yaml=sfiles, stringsAsFactors=FALSE)[stargs %in% rtargs,]
  
  rfiles <- sapply(seq_len(nrow(to_import)), function(i) {
    status <- yaml::yaml.load_file(to_import$yaml[i])
    status$version <- as.package_version(status$version)
    status$time <- char2POSIX(status$time)
    
    dbstore$set(key=to_import$target[i], value=status)
    return(to_import$mkey[i])
  })
  
  extra_rfiles <- setdiff(rfiles, storekeys)
  if(length(extra_rfiles) > 0) warning(paste("these status files may be obsolete:", paste(extra_rfiles, collapse=", ")))
}

