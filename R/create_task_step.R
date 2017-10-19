#' Create an object that defines a step within a task
#'
#' The default values of each parameter are often acceptable, but all parameters
#' may be overridden. When constructing the task makefile or table, the
#' `target_name`, `depends`, and `command` elements are built in that order,
#' with each element optionally depending on the result of the previous
#' elements. They can also depend on the `step_name` (defined in this function
#' call) and/or the `task_name` (to be listed in a call to `create_task_plan()`,
#' where the definitions declared here will ultimately be evaluated)
#'
#' @param step_name a single character string naming this step. The default
#'   `target_name` combines this `step_name` with the `task_name`, and the
#'   default `command` assumes this `step_name` is the function name, but both
#'   defaults may be overridden (see next arguments)
#' @param target_name a character string or vector, or a function that produces
#'   a character string or vector, giving a unique name for the remake target
#'   for a specific application of this step to a specific task. If a function,
#'   should accept `...` and other args optionally including `task_name` and
#'   `step_name`
#' @param depends a character string or vector, or a function that produces a
#'   character string or vector, defining any dependencies that need to be
#'   declared in addition to those implied by `command`. If a function, should
#'   accept `...` and other args optionally including `task_name`, `step_name`,
#'   and `target_name` args optionally including `task_name` and `step_name`
#' @param command a character string or vector, or a function that produces a
#'   character string or vector, defining the command to be run for each
#'   application of this step to a specific task. If a function, should accept
#'   `...` and other args optionally including `task_name`, `step_name`,
#'   `target_name`, and `depends`
#' @md
#' @export
#' @examples
#' create_task_step(
#'   'plot',
#'   target_name=function(task_name, step_name, ...) {
#'     sprintf('~/MyProjects/thisproject/%s_%s.png', task_name, step_name)
#'   },
#'   command='plot_site(target_name)'
#' )
create_task_step <- function(
  step_name,
  target_name = function(task_name, step_name, ...) {
    sprintf('%s_%s', task_name, step_name)
  },
  depends = character(0),
  command = function(task_name, step_name, ...) {
    sprintf("%s('%s')", step_name, task_name)
  }
) {
  
  # create the step definition
  step_def <- list(step_name=step_name, target_name=target_name, depends=depends, command=command)
  class(step_def) <- 'task_step'
  
  # check the inputs for proper formatting
  for(elem_name in names(step_def)) {
    element <- step_def[[elem_name]]
    if(is.character(element)) {
      # this is fine
    } else if(is.function(element)) {
      # check that the arguments are as expected
      given_args <- names(formals(element))
      expected_args <- c(
        '...','task_name','step_name','steps', # all elements can see these (steps contains only the steps preceding the current one)
        if(elem_name == 'depends') c('target_name'),
        if(elem_name == 'command') c('target_name','depends'))
      unknown_args <- setdiff(given_args, expected_args)
      if(length(unknown_args) > 0) {
        stop(paste0("unknown argument name[s] ",paste("'",unknown_args,"'", collapse=", ")," in '", elem_name, "' element"))
      }
      if(!('...' %in% given_args)) {
        stop(paste0("'...' must be included in arguments to ", elem_name))
      }
    } else {
      stop('each element must be either a character string/vector or a function')
    }
  }
  
  return(step_def)
}
