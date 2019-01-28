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
#' @param target_name a file path (or vector of file paths) specifying final target(s) 
#'   to be created by `finalize_fun`
#' @param finalize_fun a string function name (or vector of function names, of equal 
#'   length to `target_name`)
#' @param as_promises ... details here...
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
  template_file=system.file('extdata/task_makefile.mustache', package='scipiper'),
  target_name, 
  finalize_fun = NULL, 
  as_promises = TRUE) {
  
  # prepare the overall job task: list every step of every job as a dependency.
  # first mutate the makefile file name into an object name to use as the
  # default/overall job target. this should be an acceptable target name (not
  # conflicting with other targets) and allows the calling remake file to use
  # ind_file as a target (even though this target is the one responsible for
  # actually writing to ind_file)
  job_name <- tools::file_path_sans_ext(basename(makefile))
  if(missing(target_name)){
    ind_ext <- getOption("scipiper.ind_ext") # put this here because if you want to control the extension, specify the target_name
    ind_dir <- attr(task_plan, 'ind_dir') # if you want to control the dir over default, specify the target_name
    target_name <- file.path(ind_dir, as_ind_file(job_name, ind_ext))
  }
  
  if (!is.null(finalize_fun) & length(target_name) != length(finalize_fun)){
    stop('when specifying function names for `finalize_fun`, an equal number of `target_name`(s) need to be specified', call. = FALSE)
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
  
  if (is.null(finalize_fun)){
    stop('currently not implemented')
  } else {
    job <- lapply(seq_len(length(finalize_fun)), function(i){
      list(
        target_name = ifelse(as_promises, paste0(basename(target_name[i]), "_promise"), target_name[i]), 
        command = {
          # TODO: if the combine targets are objects, they shouldn't be quoted!
          to_combine <- paste(paste0("'", targets_to_combine, "'"), collapse=',\n      ')
          pos_extensions <- unique(c(remake::file_extensions(), file_extensions))
          is_file <- remake:::target_is_file(target_name[i], file_extensions = pos_extensions)
          # hide the object/file changes as input when using `promises`
          if (is_file){
            ifelse(as_promises, {
              sprintf("%s(I('%s'),\n      %s)", finalize_fun[i], target_name[i], to_combine)
            }, {
              sprintf("%s(target_name,\n      %s)", finalize_fun[i], to_combine)
            })
          } else {
            combine_str <- "%s(\n      %s)"
            sprintf(combine_str, finalize_fun[i], to_combine) # use PR #90 when it is merged
          }
        })
    })
  }
  
  message(sprintf(
    "run all tasks with\n%s:\n  command: scmake(remake_file='%s')\n",
    target_name, makefile))
  
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

  return(makefile)
}
