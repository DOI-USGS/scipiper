# These functions are a first draft of the scipiper function. Let's see how it
# goes.

#' Create a .yml makefile for a multi-task job
#'
#' Create a .yml makefile (for use with remake or scipiper) for a set of tasks
#' that together form a single job.  The default target will be named after
#' `makefile` (specifically,
#' `indicator_file=tools::file_path_sans_ext(basename(makefile))`) and can be
#' evoked from another remake file as 
#' `make(I('indicatorfile'),remake_file='thismakefile')`
#' after replacing `indicatorfile` and `thismakefile` with their values.
#'
#' @param task_plan a task plan as produced by `create_task_plan()`
#' @param makefile character name of the remake file to create
#' @param indicator_dir directory path specifing the location where an overall
#'   job indicator file should be written once all tasks and steps are complete.
#'   This file will always be named after the makefile, but with '.ind' instead
#'   of '.yml' as a suffix
#' @param include character vector of any remake .yml files to include within
#'   this one. If any files must be quoted in the remake file, quote them with
#'   inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param packages character vector of any packages to load before running steps
#' @param sources character vector of any files that should be sourced before
#'   running steps. If any files must be quoted in the remake file, quote them
#'   with inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param template_file character name of the mustache template to render into
#'   `makefile`. The default is recommended
#' @return the file name of the makefile that was created (can be displayed with
#'   `cat(readLines(makefile), sep='\n')`).
#' @export
#' @examples
#' task_config <- data.frame(
#'   id=c('AZ','CO','CA'),
#'   capital=c('Phoeniz','Denver','Sacramento')
#' )
#' step1 <- create_task_step(
#'   step_name = 'prep',
#'   target = function(task_name, step_name, ...) {
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
#' task_makefile <- create_task_makefile(
#'   task_plan, makefile=file.path(tempdir(), 'states.yml'),
#'   file_extensions=c('ind'), packages='mda.streams')
#' cat(readLines(task_makefile), sep='\n')
create_task_makefile <- function(
  task_plan, makefile=NULL,
  include=c(), packages=c(), sources=c(), file_extensions=c('ind'),
  indicator_dir=attr(task_plan, 'indicator_dir'),
  template_file='../lib/task_makefile.mustache') {
  
  # prepare the overall job task: list every step of every job as a dependency.
  # first mutate the makefile file name into an object name to use as the
  # default/overall job target. this should be an acceptable target name (not
  # conflicting with other targets) and allows the calling remake file to use
  # indicator_file as a target (even though this target is the one responsible
  # for actually writing to indicator_file)
  job_target <- tools::file_path_sans_ext(basename(makefile))
  job_steps <- attr(task_plan, 'final_steps')
  indicator_file <- file.path(indicator_dir, paste0(job_target, '.ind'))
  job <- list(
    target_name = job_target,
    # even though target_name is an object (not file), job_command should write
    # to indicator_file - again so the calling remake file can use
    # indicator_file as its target
    command = sprintf("write_timestamp(I('%s'))", indicator_file),
    # as dependencies of this overall/default job, extract the target_name from
    # every task and all those steps indexed by job_steps. an alternative (or
    # complement) would be to create a dummy target for each task (probably with
    # indicator file, at least until
    # https://github.com/richfitz/remake/issues/92 is resolved) and then have
    # this overall target depend on those dummy targets.
    depends = unlist(lapply(task_plan, function(task) {
      lapply(task$steps[job_steps], function(step) {
        step$target_name
      })
    }), use.names=FALSE)
  )
  message(sprintf(
    "run all tasks with\n%s:\n  command: make(I('%s'), remake_file='%s')",
    indicator_file, job_target, makefile))
  
  # prepare the task list for rendering
  tasks <- unname(task_plan) # remove list element names where they'd interfere with whisker.render
  for(task in seq_along(tasks)) {
    # remove lmore ist element names where they'd interfere with whisker.render
    tasks[[task]]$steps <- unname(tasks[[task]]$steps)
    for(step in seq_along(tasks[[task]]$steps)) {
      # add a logical for has_depends
      tasks[[task]]$steps[[step]]$has_depends <- length(tasks[[task]]$steps[[step]]$depends) > 0
    }
  }
  
  # Gather info about how this function is being called
  this_fun <- as.character(sys.call(0)[[1]])
  calling_fun <- as.character(sys.call(-1)[[1]])
  call_info <- if(length(calling_fun) == 0) {
    sprintf('%s()', this_fun)
  } else {
    sprintf('%s() via %s()', this_fun, calling_fun)
  }
  
  # prepare the final list of variables to be rendered in the template
  params <- list(
    job = job,
    target_default = job_target,
    include = include,
    has_include = length(include) > 0,
    packages = packages,
    has_packages = length(packages) > 0,
    sources = sources,
    has_sources = length(sources) > 0,
    file_extensions = file_extensions,
    has_file_extensions = length(file_extensions) > 0,
    tasks = tasks,
    rendering_function = call_info
  )
  
  # read the template
  template <- readLines(template_file)
  
  # render the template
  yml <- whisker::whisker.render(template, data=params)
  yml <- gsub('[\n]{3,}', '\\\n\\\n', yml) # reduce 3+ line breaks to just 2
  
  # write the makefile and return the file path
  cat(yml, file=makefile)
  return(makefile)
}

write_timestamp <- function(job_target) {
  writeLines(text=format(Sys.time(), '%Y-%m-%d %H:%M:%S UTC', tz='UTC'), con=job_target)
}

#' Convert a task_plan into a status table
#'
#' Create a data.frame or .tsv file representing a task_plan and our status
#' within that plan
#' @param task_plan a task plan as produced by `create_task_plan()`
#' @param table_file a file name to write a tab-separated table to (as .tsv), or
#'   NULL to return as a data.frame
#' @export
create_task_table <- function(task_plan, table_file) {
  stop("sorry, not implemented yet")
}

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
#'   target = function(task_name, step_name, ...) {
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
  task_names <- setNames(as.list(rep(NA, length(task_names))), task_names)
  
  # check that task_steps is a list of task_steps
  if(!is.list(task_steps)) {
    stop('task_steps must be a list')
  } else if(any(!sapply(task_steps, function(ts) is(ts, 'task_step')))) {
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
      target = function(task_name, ...) {
        sprintf("'%s.ind'", file.path(indicator_dir, task_name))
      },
      depends = function(steps, ...) {
        # when this final step is evaluated, steps will contain all preceding
        # steps in the task. subset to final_steps
        chosen_steps <- unname(steps[final_steps])
        # return the target_names from each of the chosen_steps
        sapply(chosen_steps, `[[`, 'target_name', USE.NAMES=FALSE)
      },
      command = "write_timestamp(target_name)"
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
