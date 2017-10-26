#### timestamp helpers ####

#' Format a POSIXct timestamp with UTC into a character string
#' @name time_conversions
#' @param ptime time in POSIXct format
#' @return time in character string format
#' @export
POSIX2char <- function(ptime) {
  format(ptime, tz='UTC', format="%Y-%m-%d %H:%M:%S", usetz=TRUE)
}

#' Format a character string into POSIXct timestamp with UTC
#' @rdname time_conversions
#' @param stime time in character string format
#' @return time in POSIXct format
#' @export
char2POSIX <- function(stime) {
  as.POSIXct(strptime(stime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
}
