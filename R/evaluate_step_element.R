#' Evaluate a step element into simple strings
#'
#' Evaluate a step element (target, depends, command) as a character
#' string/vector or as a function of the task and step elements (depending on
#' the class of the element)
#'
#' @param task_step a task_step as created by `create_task_step()`
#' @param element character naming a task_step element ('target_name',
#'   'depends', or 'command') to evaluate
#' @param task a task item: a list with fields for 'task_name' and possibly
#'   other info
#' @param step a step item: a list with fields for 'step_name' and possibly
#'   other info
#' @keywords internal
evaluate_step_element <- function(task_step, element, task, step) {
  # extract the desired task-step element (tse)
  if(!element %in% names(task_step)) {
    stop("task_step does not define the element '", element, "'")
  }
  tse <- task_step[[element]]
  
  # evaluate the tse according to its class
  if(is.character(tse)) {
    out <- tse
  } else if(is.function(tse)) {
    args <- c(task, step) # combine all available task-step info into a single list
    out <- do.call(tse, args) # apply tse as a function of that info
  } else {
    out <- tse # but we'll throw a warning below
  }
  
  if(!is.character(out)) {
    warning("output isn't character - that's probably bad")
  }
  if(any(grepl('"', out))) {
    warning('in task=', task$task_name,
            ', step=', step$step_name,
            ', "',element , '":',
            ' use \' instead of " to avoid html escaping')
  }
  return(out)
}
