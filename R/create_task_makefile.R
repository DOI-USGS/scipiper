#' Create a .yml makefile for a multi-task job
#'
#' Create a .yml makefile (for use with remake or scipiper) for a set of tasks
#' that together form a single job.  The default target will be named after
#' `makefile` (specifically,
#' `indicatorfile=tools::file_path_sans_ext(basename(makefile))`) and can be
#' evoked from another remake file as
#' `make(I('indicatorfile'),remake_file='thismakefile.yml')` after replacing
#' `indicatorfile` and `thismakefile.yml` with their values.
#'
#' @param task_plan a task plan as produced by `create_task_plan()`
#' @param makefile character name of the remake file to create
#' @param include character vector of any remake .yml files to include within
#'   this one. If any files must be quoted in the remake file, quote them with
#'   inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param packages character vector of any packages to load before running steps
#' @param sources character vector of any files that should be sourced before
#'   running steps. If any files must be quoted in the remake file, quote them
#'   with inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param file_extensions character vector of file extensions to be added to the
#'   defaults at `remake::file_extensions()`. Inclusion of `'ind'` is
#'   recommended because this indicator file extension is commonly used by
#'   scipiper.
#' @param template_file character name of the mustache template to render into
#'   `makefile`. The default is recommended
#' @param final_targets a file path or object name (or vector of either) specifying final target(s) 
#'   to be created by `finalize_funs`
#' @param finalize_funs a string function name (or vector of function names, of equal 
#'   length to `final_targets`). Using NULL for `finalize_funs` will turn the 
#'   final steps from the task plan into depends for the default/all target.
#' @param as_promises hides the actual `final_targets` file/object from remake, and uses a 
#'   dummy target name instead, with suffix "_promise". This allows us to avoid cyclic 
#'   dependencies. Naming convention for `_promise` variables is to drop any dir structure 
#'   from the `final_targets`
#' @return the file name of the makefile that was created, 
#'   or the string output (if makefile = NULL)
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
#'   final_steps='report', ind_dir='states/log')
#' task_makefile <- create_task_makefile(
#'   task_plan, makefile=file.path(tempdir(), 'states.yml'),
#'   file_extensions=c('ind'), packages='mda.streams')
#' cat(readLines(task_makefile), sep='\n')
#' @importFrom methods getPackageName
#' @importFrom utils packageVersion
create_task_makefile <- function(
  task_plan, makefile,
  include=c(), packages='scipiper', sources=c(), file_extensions=c('ind'),
  template_file=system.file('extdata/task_makefile.mustache', package='scipiper'),
  final_targets, 
  finalize_funs = 'combine_to_ind', 
  as_promises = TRUE) {
  
  # prepare the overall job task: list every step of every job as a dependency.
  # first mutate the makefile file name into an object name to use as the
  # default/overall job target. this should be an acceptable target name (not
  # conflicting with other targets) and allows the calling remake file to use
  # ind_file as a target (even though this target is the one responsible for
  # actually writing to ind_file)
  
  # default is to use the makefile naming to assign job name
  # when makefile isn't used (string output), we use the final_targets to create default job name
  if (is.null(makefile)){
    if (missing(final_targets) || length(final_targets) != 1){
      job_name <- "all_tasks"
    } else {
      job_name <- paste0(tools::file_path_sans_ext(basename(final_targets)), '_all')
    }
  } else {
    job_name <- tools::file_path_sans_ext(basename(makefile))
  }

  
  
  if(missing(final_targets)){
    ind_ext <- getOption("scipiper.ind_ext") # put this here because if you want to control the extension, specify the final_targets
    ind_dir <- attr(task_plan, 'ind_dir') # if you want to control the dir over default, specify the final_targets
    if (is.null(ind_dir)){
      final_targets <- as_ind_file(job_name, ind_ext)
    } else {
      final_targets <- file.path(ind_dir, as_ind_file(job_name, ind_ext))  
    }
  }
  
  if (!is.null(finalize_funs) & length(final_targets) != length(finalize_funs)){
    stop('when specifying function names for `finalize_funs`, an equal number of `final_targetss` need to be specified')
  }
  
  job_steps <- attr(task_plan, 'final_steps')
  
  targets_to_combine <- unlist(lapply(task_plan, function(task) {
    lapply(task$steps[job_steps], function(step) {
      step$target_name
    })
  }), use.names=FALSE)
  
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
  
  pos_extensions <- unique(c(remake::file_extensions(), file_extensions))
  
  formatted_final_targets <- sapply(X = targets_to_combine, function(target){
    if (remake:::target_is_file(target, file_extensions = pos_extensions)){
      paste0("'", target, "'")
    } else {
      target
    }
  })
  
  if (is.null(finalize_funs)){
    # when there is no finalize fun, the "jobs" are the final step targs and they have no new commands
    job <- lapply(seq_len(length(formatted_final_targets)), function(i){
      list(
        target_name = unname(formatted_final_targets[i])
      )
      })
    has_finalize_funs <- FALSE
  } else {
    has_finalize_funs <- TRUE
    job <- lapply(seq_len(length(finalize_funs)), function(i){
      list(
        target_name = ifelse(as_promises, paste0(basename(final_targets[i]), "_promise"), final_targets[i]), 
        command = {
          to_combine <- paste(formatted_final_targets, collapse=',\n      ')
          target_is_file <- remake:::target_is_file(final_targets[i], file_extensions = pos_extensions)
          # hide the object/file changes as input when using `promises`
          if (target_is_file){
            ifelse(as_promises, {
              sprintf("%s(I('%s'),\n      %s)", finalize_funs[i], final_targets[i], to_combine)
            }, {
              sprintf("%s(target_name,\n      %s)", finalize_funs[i], to_combine)
            })
          } else {
            combine_str <- "%s(\n      %s)"
            sprintf(combine_str, finalize_funs[i], to_combine) 
          }
        })
    })
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
    target_default = job_name,
    include = include,
    has_include = length(include) > 0,
    has_scipiper_version = TRUE, 
    scipiper_version = utils::packageVersion(methods::getPackageName()),
    packages = packages,
    has_packages = length(packages) > 0,
    sources = sources,
    has_sources = length(sources) > 0,
    file_extensions = file_extensions,
    has_file_extensions = length(file_extensions) > 0,
    tasks = tasks,
    has_finalize_funs = has_finalize_funs,
    rendering_function = call_info
  )
  # read the template
  template <- readLines(template_file)
  
  # render the template
  yml <- whisker::whisker.render(template, data=params)
  yml <- gsub('[\n]{3,}', '\\\n\\\n', yml) # reduce 3+ line breaks to just 2
  
  if (is.null(makefile)){
    makefile <- yml
  } else {
    cat(yml, file=makefile)
    if(has_finalize_funs) {
      how_to_run <- paste0(
        "Run all tasks and finalizers with:\n",
        paste(sapply(seq_along(job), function(job_id) {
          finalizer <- job[[job_id]]
          parent_target_name <- if(as_promises) final_targets[[job_id]] else sprintf("your_target_name_%d", job_id)
          sprintf(
            "%s:\n  command: scmake(I('%s'), remake_file='%s')",
            parent_target_name, finalizer$target_name, normalizePath(makefile, winslash='/'))
        }), collapse='\n')
      )
    } else {
      how_to_run <- sprintf(
        "Run all tasks with\n%s:\n  command: scmake(remake_file='%s')\n",
        final_targets, makefile)
    }
    message(how_to_run)
  }
  # write the makefile and return the file path
  
  return(makefile)
}
