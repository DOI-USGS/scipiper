#' Take a named vector of directory paths, ensure they get created, and return
#' them as a named list
#'
#' This function is designed for use in a remake.yml. It allows for the creation
#' of several directories in the same remake command, and it both creates the
#' directories and returns a named list of the directory paths. Should be called
#' with directory names wrapped in `I()`; see examples.
#'
#' @param ... named arguments, each giving a directory name; will be returned as
#'   a named list
#' @examples
#' wqp_pull_folders:
#'   command: scipiper::create_dirs(
#'     cache=I('1_wqdata/cache/wqp'),
#'     out=I('1_wqdata/out/wqp'),
#'     log=I('1_wqdata/log/wqp'))
#' # wqp_pull_folders can be passed to other remake commands, in whose
#' # functions paths can be referenced as wqp_pull_folders$log, etc.
create_dirs <- function(...) {
  dirs <- list(...)
  lapply(dirs, function(dirname) {
    if(!dir.exists(dirname)) dir.create(dirname, recursive=TRUE)
  })
  return(dirs)
}
