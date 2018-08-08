#' Build a job target by looping over the tasks and steps in a task remake file
#'
#' @description {
#'
#'   Usual behavior: Attempts all steps in a task before moving on to the next
#'   task. This is especially useful if intermediate files are created and
#'   deleted over several steps within a task, and if those files would take up
#'   too much space if intermediate files from one step of all tasks were
#'   simultaneously present. This function also provides fault tolerance,
#'   retries, and using file existence as a first check on which tasks/steps are
#'   already built (faster than the remake practice of checking file hashes,
#'   which are only checked here after all files' presence suggest that
#'   everything is built).
#'
#'   Important: if you want the "usual behavior" from this function, delete any
#'   task targets that are files and are known to be out of date before
#'   starting. Out-of-date files will only be checked and rerun at the very end,
#'   whereas missing files will be tried as many as `num_tries` times.
#'
#'   }
#'
#' @param task_plan task plan as created by `create_task_plan()`
#' @param task_makefile file name of the .yml makefile for these tasks
#' @param task_names NULL to build all tasks, or character vector of specific
#'   tasks to build
#' @param step_names NULL to build all final_steps from the task plan (see
#'   `create_task_plan`), or character vector of specific steps to build
#' @param num_tries integer number of times to retry looping through all
#'   remaining tasks
#' @param sleep_on_error integer number of seconds to sleep immediately after a
#'   failed task. Especially useful if the error was likely to be inconsistent
#'   (e.g., a temporary network issue) and might not occur again if we wait a
#'   while
#' @param vebose define the format of task messages. Use TRUE for progress bar 
#'   for the status of each task, and FALSE for no output
#' @export
#' @import progress
loop_tasks <- function(
  task_plan, task_makefile,
  task_names=NULL, step_names=NULL,
  num_tries=30, sleep_on_error=0,
  ind_ext = getOption('scipiper.ind_ext'),
  verbose = TRUE) {
  
  # provide defaults for task_names (all tasks) and step_names (final_steps)
  target_default <- yaml::yaml.load_file(task_makefile)$target_default
  if(is.null(task_names) && is.null(step_names)) {
    job_target <- target_default
  } else {
    job_target <- NA
  }
  if(is.null(task_names)) {
    task_names <- names(task_plan)
  }
  if(is.null(step_names)) {
    step_names <- attr(task_plan, "final_steps")
  }
  # identify the task-step targets to be run, ordered by tasks and then steps
  # within tasks
  targets <- unlist(lapply(unname(task_plan[task_names]), function(task) {
    sapply(unname(task$steps[step_names]), `[[`, 'target_name')
  }))
  num_targets_overall <- length(targets)
  
  # a heuristic check for completeness: returns a vector of indices into target
  # that are known to be incomplete. any target that remake knows to be complete
  # will be excluded, and any file targets whose files exist are also treated as
  # complete for now. this is much quicker than asking remake to rehash every
  # model file to check for changes before we even get started. if we let some
  # files get out of date without deleting them, they will be skipped in these
  # loops, but remake will catch them in the final calls of this function when
  # we run remake on the entire job/all targets
  which_incomplete <- function(targets, task_makefile) {
    is_current <- get_remake_status(targets, task_makefile) %>%
      dplyr::right_join(data_frame(target=targets), by='target') %>%
      pull(is_current) %>%
      as.logical()
    incomplete_targets <- which(!(file.exists(targets) | is_current))
  }
  
  incomplete_targets <- which_incomplete(targets, task_makefile)
  total_num_tasks_incomplete <- length(incomplete_targets)
  total_num_tasks_complete <- 0
  if (verbose){
    pb <- progress::progress_bar$new(
      format = ":what [:bar] :percent  ",
      width = getOption('width')+4,
      show_after = 0, 
      clear = FALSE, total = total_num_tasks_incomplete)
  }
  
  # run the targets in a loop, with retries, so that we complete (or skip) one
  # task before trying the next. If we just ran scmake(job_target,
  # task_makefile) directly, remake would try to build the first step for all
  # tasks before proceeding to the second step for any task
  this_try <- 1
  while(this_try <= num_tries) {
    # identify remaining needs based on the presence or absence of the target
    # file. if the file exists, don't bother. this is much quicker than asking
    # remake to rehash every model file to check for changes before we even get
    # started. if we somehow messed up and let file file get out of date, remake
    # will catch it in the final calls of this function when we run remake on
    # the entire job, properly. if a target is an object rather than a file,
    # we'll build it every time
    num_targets <- length(incomplete_targets)
    target_complete <- rep(FALSE, num_targets)
    
    if(num_targets == 0) {
      if (verbose){
        if (total_num_tasks_incomplete > 0){ # was the prgress bar build to > 0?
          pb$update(1, tokens = list(what = "All tasks complete"))
        }
      }
      break
    }
    
    # if there are remaining targets, try to run them
    loop_start_msg <- sprintf(
      "### Starting loop attempt %s of %s with %s of %s tasks remaining:",
      this_try, num_tries, num_targets, num_targets_overall)
    if (verbose){
      pb$message(loop_start_msg)
    }
    this_try <- this_try + 1
    
    for(i in seq_len(num_targets)) {
      tryCatch({
        target_num_overall <- incomplete_targets[i]
        target <- targets[target_num_overall]
        task_name <- task_names[target_num_overall]
        if (verbose){
          pb$update(total_num_tasks_complete/total_num_tasks_incomplete, 
                    tokens = list(what = sprintf('  Building %s', 
                                                 task_name, target_num_overall, num_targets_overall)))
        }
        
        
        # the main action: run the task-step
        scmake(target, task_makefile, ind_ext=ind_ext, verbose=FALSE)
        target_complete[i] <- TRUE
        total_num_tasks_complete <- total_num_tasks_complete + 1
        
      }, error=function(e) {
        if(verbose){
          pb$message(sprintf("* Error in %s: %s; debug with scmake(\"%s\", \"%s\")", 
                             deparse(e$call), e$message, target, task_makefile))
        }
        
        # sleep for a while if requested
        if(sleep_on_error > 0) {
          Sys.sleep(sleep_on_error)
        }
      })
      # try to keep memory under control if possible; might be harder with
      # object targets, not sure if storr keeps them all in memory
      gc()
    }
    incomplete_targets <- incomplete_targets[!target_complete]
  }
  
  # check the indicator files one last time; if we didn't make it this far,
  # don't try remaking the entire job
  is_current <- get_remake_status(targets, task_makefile) %>%
    dplyr::right_join(data_frame(target=targets), by='target') %>%
    pull(is_current) %>%
    as.logical()
  incomplete_targets <- which(!(file.exists(targets) | is_current))
  num_targets <- length(incomplete_targets)
  if(num_targets > 0) {
    stop(sprintf("All tries are exhausted, but %s tasks remain", num_targets))
  }
  
  # if we've made it this far, remake every file to ensure we're done and to
  # write the job indicator file. this will take a few minutes because remake
  # will check the hashes of every file (the big model files take the longest).
  if (verbose){
    message(sprintf("\n### Final check for completeness of all targets"))
  }
  if(!is.na(job_target)) {
    scmake(job_target, task_makefile, ind_ext=ind_ext, verbose=verbose)
  } else {
    # check all file targets, which at this point will all exist even if they're
    # not up to date. there's no need to run non-file targets because we've
    # always attempted them in the loop above (every time through the loop, in
    # fact).
    file_targets <- targets[file.exists(targets)]
    num_files <- length(file_targets)
    for(i in seq_len(num_files)) {
      target <- file_targets[i]
      if (verbose){
        message(sprintf(
          "Checking file %s of %s: %s",
          i, num_files, target
        ))
      }
      scmake(target, task_makefile, ind_ext=ind_ext, verbose=FALSE)
    }
    msg <- paste(c(
      strwrap("Set task_names=NULL, step_names=NULL to build the job target. Until then, expect this error:"),
      "  'command for [target] did not create file'"),
      collapse="\n")
    if (verbose) message(msg)
  }
}
