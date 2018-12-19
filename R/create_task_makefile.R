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
#' @param ind_dir directory path specifing the location where an overall job
#'   indicator file should be written once all tasks and steps are complete.
#'   This file will always be named after the makefile, but with the indicator
#'   file extension (ind_ext) instead of '.yml' as a suffix
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
#' @param ind_ext the indicator file extension to use in creating the job
#'   indicator file
#' @param template_file character name of the mustache template to render into
#'   `makefile`. The default is recommended
#' @return the file name of the makefile that was created (can be displayed with
#'   `cat(readLines(makefile), sep="\n")`).
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
create_task_makefile <- function(
  task_plan, makefile,
  include=c(), packages=c(), sources=c(), file_extensions=c('ind'),
  ind_complete=NA,
  ind_dir=attr(task_plan, 'ind_dir'),
  ind_ext=getOption("scipiper.ind_ext"),
  template_file=system.file('extdata/task_makefile.mustache', package='scipiper'),
  target_name,
  finalize_fun = NULL) {
  
  # prepare the overall job task: list every step of every job as a dependency.
  # first mutate the makefile file name into an object name to use as the
  # default/overall job target. this should be an acceptable target name (not
  # conflicting with other targets) and allows the calling remake file to use
  # ind_file as a target (even though this target is the one responsible for
  # actually writing to ind_file)
  job_name <- tools::file_path_sans_ext(basename(makefile))
  job_steps <- attr(task_plan, 'final_steps')
  job_deps <- unlist(lapply(task_plan, function(task) {
    lapply(task$steps[job_steps], function(step) {
      step$target_name
    })
  }), use.names=FALSE)
  
  job_output <- if(isTRUE(ind_complete) || is.na(ind_complete)) {
    if(is.null(ind_dir)) stop('ind_dir must not be NULL when ind_complete=TRUE')
    file.path(ind_dir, as_ind_file(job_name, ind_ext))
  } else {
    sprintf('%s_%s', job_name, ind_ext)
  }
  
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
  job <- list()
  
  job <- list(list(
    target_name = ifelse(is.null(finalize_fun), job_name, target_name),
    # even though target_name is an object (not file), job_command should write
    # to ind_file - again so the calling remake file can use
    # ind_file as its target
    command = {
      if(!is.null(finalize_fun)){
        if(isTRUE(ind_complete) | is.na(ind_complete)) {
          message('ind_complete=', ind_complete, ' is incompatable with ', finalize_fun, '(). Using FALSE')
          ind_complete <- FALSE
        }
        if(length(job_deps) < 1){
          stop('need to specify at least one target when using a finalize function', call. = FALSE)
        }
        to_combine <- paste(job_deps, collapse=',\n      ')
        job_deps <- c() # these are no longer depends, they are inputs
        sprintf("%s(target_name = %s,\n      %s)", finalize_fun, target_name, to_combine) 
      } else {
        if(is.na(ind_complete)) {
          message('ind_complete=NA is deprecated; use TRUE or FALSE')
          sprintf("sc_indicate(I('%s'))", job_output) # the old way: use a timestamp
        } else {
          if(isTRUE(ind_complete)) {
            sprintf("sc_indicate(I('%s'), hash_depends=I(TRUE), depends_target=I('%s'), depends_makefile=I('%s'))",
                    job_output, target_name, makefile)
          } else {
            sprintf("hash_dependencies(target_name=target_name, remake_file=I('%s'))", makefile)
          }
        }
      }
    },
    # as dependencies of this overall/default job, extract the target_name from
    # every task and all those steps indexed by job_steps. an alternative (or
    # complement) would be to create a dummy target for each task (probably with
    # indicator file, at least until
    # https://github.com/richfitz/remake/issues/92 is resolved) and then have
    # this overall target depend on those dummy targets.
    depends = job_deps
  ))
  
  job[[1]]$has_depends <- length(job[[1]]$depends) > 0
  
  
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
  
  message(sprintf(
    "run all tasks with\n%s:\n  command: make(I('%s'), remake_file='%s')",
    job_output, job_name, makefile))
  
  return(makefile)
}
