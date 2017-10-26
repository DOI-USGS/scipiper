#' Define a set of tasks and steps within a single job
#'
#' Create a structured list that organizes tasks and their sub-tasks (steps) for
#' a large number of near-identical tasks
#'
#' @param task_names character vector of IDs for sets of work coordinates, with
#'   length equal to the number of tasks. You will usually use these character
#'   strings within the names for each step in each task. These names should
#'   therefore describe the aspect/aspects of a task that position it within the
#'   larger parameter space of work to be done. Examples: site IDs, model names,
#'   or character code identifying a unique row in a configuration file or
#'   data.frame.
#' @param task_steps list of definitions steps to perform within each task. Each
#'   step definition should be created by `create_task_step()`
#' @param final_steps vector of step indices (either step_names or integers)
#'   identifying those task_steps that must be completed to consider the entire
#'   task complete. If one step depends on all previous steps (as indicated by
#'   its remake `depends:` and `command:` fields) then only that one step need
#'   be included in `final_steps`.
#' @param add_complete logical. if TRUE, `indicator_dir` must be supplied and a
#'   final step named 'complete' will be added to the step list. This step will
#'   depend on all `final_steps` and will write an indicator file with a
#'   timestamp when all final steps are complete.
#' @param indicator_dir directory path, only used if `add_complete==TRUE`,
#'   specifying the location where task-specific indicator files should be
#'   written. Such files will always be named after their associated tasks, with
#'   '.ind' as a suffix
#' @return a structured list that can be passed to `create_task_makefile` or
#'   `create_task_table`
#' @export
#' @examples
#' task_config <- data.frame(
#'   id=c('AZ','CO','CA'),
#'   capital=c('Phoeniz','Denver','Sacramento')
#' )
#' step1 <- create_task_step(
#'   step_name = 'prep',
#'   target_name = function(task_name, step_name, ...) {
#'     sprintf('%s_%s', task_name, step_name)
#'   },
#'   depends = c('A','B'),
#'   command = "process(target_name, I('C'))"
#' )
#' step2 <- create_task_step(
#'   step_name = 'plot',
#'   command = function(target_name, task_name, ...) {
#'     capital <- task_config[task_config$id == task_name, 'capital']
#'     sprintf('visualize(\'%s\', \'%s\')', task_name, capital)
#'   }
#' )
#' step3 <- create_task_step('report')
#' task_plan <- create_task_plan(c('AZ','CA','CO'), list(step1, step2, step3),
#'   final_steps='report', indicator_dir='states/log')
create_task_plan <- function(
  task_names, task_steps, 
  final_steps=sapply(task_steps, `[[`, 'step_name'),
  indicator_dir, add_complete=TRUE
) {
  
  # munge the task_names from a character vector into the names in a list (the
  # list contents will be amended below)
  if(!is.character(task_names)) {
    stop('task_names must be a character vector')
  }
  task_names <- stats::setNames(as.list(rep(NA, length(task_names))), task_names)
  
  # check that task_steps is a list of task_steps
  if(!is.list(task_steps)) {
    stop('task_steps must be a list')
  } else if(any(!sapply(task_steps, function(ts) methods::is(ts, 'task_step')))) {
    stop('task_steps must be a list of task_step objects (see ?create_task_step)')
  }
  step_names <- sapply(task_steps, `[[`, 'step_name')
  if(any(step_names == 'complete')) {
    stop("'complete' is a reserved step name. it is added automatically by create_task_plan and should not be manually specified")
  }
  
  # force the evaluation of final_steps now so that it won't include task_is_complete
  force(final_steps)
  
  # check that final_steps is a valid vector of step_names
  if(length(final_steps) == 0) {
    stop('at least one final_steps is required if add_complete=TRUE')
  }
  unknown_choices <- setdiff(final_steps, step_names)
  if(length(unknown_choices) > 0) {
    stop(sprintf("unknown step %s in final_steps: %s",
                 if(length(unknown_choices) == 1) "index" else "indices",
                 paste0(ifelse(is.numeric(unknown_choices), unknown_choices, paste0("'", unknown_choices, "'")),
                        collapse=", ")))
  }
  
  # add a final step that ensures all other steps get run, produces an indicator
  # file named after the task
  if(add_complete) {
    complete_task <- create_task_step(
      step_name = 'complete',
      target_name = function(task_name, ...) {
        as_indicator(file.path(indicator_dir, task_name))
      },
      depends = function(steps, ...) {
        # when this final step is evaluated, steps will contain all preceding
        # steps in the task. subset to final_steps
        chosen_steps <- unname(steps[final_steps])
        # return the target_names from each of the chosen_steps
        sapply(chosen_steps, `[[`, 'target_name', USE.NAMES=FALSE)
      },
      command = "sc_indicate(target_name)"
    )
    task_steps <- c(task_steps, list(complete_task))
  }
  
  # prepare a list of the tasks and steps
  tasks <- list()
  for(i in seq_along(task_names)) {
    task <- list(
      task_name = names(task_names)[i],
      steps = list()
    )
    for(j in seq_along(task_steps)) {
      # isolate just the definitions for this step
      step_def <- task_steps[[j]]
      
      # every step is a list with step_name as the first element
      step <- list()
      step$step_name = step_def$step_name
      
      # use the user's step definitions to define the recipe
      step$target_name <- evaluate_step_element(step_def, 'target_name', task, step)
      step$depends <- evaluate_step_element(step_def, 'depends', task, step)
      step$command <- evaluate_step_element(step_def, 'command', task, step)
      
      # add this step to the task. wait until now to append because easier to
      # type/read 'step' than 'task$steps[[j]]' above
      task$steps[[step$step_name]] <- step
    }
    # append task to full tasks list. wait until now to append because faster
    # when task_names is a long list
    tasks[[task$task_name]] <- task
  }
  
  # attach the final set of final_steps as an attribute of the task list, for
  # use in creating the makefile & table
  if(add_complete) final_steps <- 'complete'
  attr(tasks, 'final_steps') <- final_steps
  
  # attach the indicator_dir to use as the default indicator_dir in
  # create_task_makefile
  attr(tasks, 'indicator_dir') <- indicator_dir
  
  return(tasks)
}
