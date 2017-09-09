#### timestamp helpers ####

#' Format a POSIXct timestamp with UTC into a character string
#' @export
POSIX2char <- function(ptime) {
  format(ptime, tz='UTC', format="%Y-%m-%d %H:%M:%S", usetz=TRUE)
}

#' Format a character string into POSIXct timestamp with UTC
#' @export
char2POSIX <- function(stime) {
  as.POSIXct(strptime(stime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
}
