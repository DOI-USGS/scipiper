#' Paste together sprintf-style text and variable values
#'
#' Helper function to sprintf a bunch of key-value (string-variableVector) pairs,
#' then paste them together with a good separator for contructing remake recipes.
#'
#' @param ... arguments that are either (1) character strings to insert directly into the output or (2) named arguments, where the name is an sprintf pattern and the value is a list or vector of variables to apply to that particular sprintf pattern. After any relevant sprint evaluation, all arguments will be pasted, with the specified separator, in order to create a single character output.
#' @param sep separator for the list of arguments, defaults to new line
#'
#' @export
#' @examples
#' cat(psprintf(
#'   "plot_frame(",
#'   "png_file = target_name,",
#'   "date = date_%s," = "2018-01-23",
#'   "state = %s)" = "Wisconsin"))
#'
#' ##plot_frame(
#' ##     png_file = target_name,
#' ##     date = date_2018-01-23,
#' ##     state = Wisconsin)
#'
#' cat(psprintf(
#'   'cmd(',
#'   'a = %s, b = \'%s\','=c('dog','cat'),
#'   'c = "rat")'
#' ))
#' ## cmd(
#' ##      a = dog, b = 'cat',
#' ##      c = "rat")
#'
#' cat(psprintf(
#'   'cmd(',
#'   'a = %s, b = %0.1f,'=list('dog',7),
#'   'c = "rat")'
#' ))
#' ## cmd(
#' ##     a = dog, b = 7.0,
#' ##     c = "rat")
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

