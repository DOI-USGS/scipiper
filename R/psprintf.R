#' Paste together sprintf-style text and variable values
#'
#' Helper function to sprintf a bunch of key-value (string-variableVector) pairs,
#' then paste them together with a good separator for contructing remake recipes.
#'
#' @param ... list of sprintf arguments seperated by commas
#' @param sep separator for the list of arguments, defaults to new line
#'
#' @export
#' @examples
#' \dontrun{
#' psprintf(
#'   "plot_frame(",
#'   "png_file = target_name,",
#'   "date = date_%s," = "2018-01-23",
#'   "state = %s)" = "Wisconsin")
#' }
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  non_null_args <- which(!sapply(args, is.null))
  args <- args[non_null_args]
  argnames <- sapply(seq_along(args), function(i) {
    nm <- names(args[i])
    if(!is.null(nm) && nm!='') return(nm)
    val_nm <- names(args[[i]])
    if(!is.null(val_nm) && val_nm!='') return(val_nm)
    return('')
  })
  names(args) <- argnames
  strs <- mapply(function(template, variables) {
    spargs <- if(template == '') list(variables) else c(list(template), as.list(variables))
    do.call(sprintf, spargs)
  }, template=names(args), variables=args)
  paste(strs, collapse=sep)
}

