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
#' @param ind_ext the indicator file extension passed to `scmake`, identifying
#'   those files for which build/status information will be shared via
#'   git-committable files in the build/status folder. You should git commit the
#'   resulting build/status files.
#' @param verbose define the format of task messages. Use TRUE for progress bar
#'   for the status of each task, and FALSE for no output
#' @param n_cores integer How many cores should be utilized when executing the task plan? 
#' Defaults to one (no parallelization).
#' @export
#' @import progress
loop_tasks <- function(
  task_plan, task_makefile,
  task_names=NULL, step_names=NULL,
  num_tries=30, sleep_on_error=0,
  ind_ext=getOption('scipiper.ind_ext'),
  verbose=TRUE,
  n_cores=1) {
  
  stopifnot(n_cores >= 1)
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
  # we run remake on the entire job/all targets. if a target is an object rather
  # than a file, we'll need to check it with get_remake_status every time, which
  # is part of why files are recommended over objects.
  which_incomplete <- function(targets, task_makefile) {
    is_current <- get_remake_status(targets, task_makefile) %>%
      dplyr::right_join(data_frame(target=targets), by='target') %>%
      pull(is_current) %>%
      as.logical()
    incomplete_targets <- which(!(file.exists(targets) | is_current))
  }
  
  incomplete_targets <- which_incomplete(targets, task_makefile)
  num_targets_incomplete <- length(incomplete_targets) # count of remaining targets to try in this loop
  num_targets_complete <- num_targets_overall - num_targets_incomplete
  if (verbose){
    pb <- progress::progress_bar$new(
      format = ":what [:bar] :percent  ",
      width = getOption('width')+4,
      show_after = 0, 
      clear = FALSE, total = num_targets_overall)
  }
  
  # run the targets in a loop, with retries, so that we complete (or skip) one
  # task before trying the next. If we just ran scmake(job_target,
  # task_makefile) directly, remake would try to build the first step for all
  # tasks before proceeding to the second step for any task
  this_try <- 1
  while(this_try <= num_tries) {
    # if we're done, update the progress bar and get out of the while loop
    if(num_targets_incomplete == 0) {
      if (verbose){
        if (num_targets_overall > 0){ # was the prgress bar build to > 0?
          pb$update(1, tokens = list(what = "All tasks complete"))
        }
      }
      break
    }
    # if there are remaining targets, try to run them
    if (verbose){
      loop_start_msg <- sprintf(
        "### Starting loop attempt %s of %s with %s of %s tasks remaining:",
        this_try, num_tries, num_targets_incomplete, num_targets_overall)
      pb$message(loop_start_msg)
    }
    
    error_function <- function(e) {
      if(verbose && !pb$finished){
        pb$message(sprintf(
          "* Error in %s: %s; debug with scmake(\"%s\", \"%s\")", 
          deparse(e$call), e$message, target, task_makefile))
      } else if(verbose && pb$finished) {
        message(sprintf(
          "* Error in %s: %s; debug with scmake(\"%s\", \"%s\")", 
          deparse(e$call), e$message, target, task_makefile))
      }
        
        # sleep for a while if requested
      if(sleep_on_error > 0) {
        Sys.sleep(sleep_on_error)
      }
    }
    if(n_cores == 1) {
      # prepare a vector to record successes/failures within this loop
      target_succeeded <- rep(FALSE, num_targets_incomplete)
      # attempt to build each of the incomplete targets
      for(i in seq_len(num_targets_incomplete)) {
        tryCatch({
          # get the names of the target and the task
          target_num_overall <- incomplete_targets[i]
          target <- targets[target_num_overall]
          task_name <- task_names[target_num_overall]
          
          # update the progress bar
          if (verbose){
            pb$update(
              num_targets_complete/num_targets_overall, 
              tokens = list(what = sprintf('  Building %s', task_name)))
          }
          # the main action: run the task-step
          scmake(target, task_makefile, ind_ext=ind_ext, verbose=FALSE)
          
          target_succeeded[i] <- TRUE
          num_targets_complete <- num_targets_complete + 1
        }, error = error_function
        )
        # try to keep memory under control if possible; might be harder with
        # object targets, not sure if storr keeps them all in memory
        gc()
      } 
      # revise and recount the list of incomplete targets for the next while loop iteration
      incomplete_targets <- incomplete_targets[!target_succeeded]
      num_targets_incomplete <- length(incomplete_targets) # count of remaining targets to try in this loop
    }else {
      #parallelized
      requireNamespace('parallel', quietly = TRUE)
      requireNamespace('doParallel', quietly = TRUE)
      requireNamespace('foreach', quietly = TRUE)
      `%dopar%` <- foreach::`%dopar%`
      cl <- parallel::makeCluster(n_cores)
      doParallel::registerDoParallel(cl, n_cores)
      target_succeeded <- foreach::foreach(i=seq_len(num_targets_incomplete))  %dopar% {
        tryCatch({
          # get the names of the target and the task
          target_num_overall <- incomplete_targets[i]
          target <- targets[target_num_overall]
          
          # the main action: run the task-step
          scmake(target, task_makefile, ind_ext=ind_ext, verbose=FALSE)
          return(TRUE)
        }, error = function(e) {
          error_function(e)
          return(FALSE)}
        )
      }
      target_succeeded <- as.logical(target_succeeded) #convert list to vector
      num_targets_complete <- sum(target_succeeded)
      # revise and recount the list of incomplete targets for the next while loop iteration
      incomplete_targets <- incomplete_targets[!target_succeeded]
      num_targets_incomplete <- length(incomplete_targets) # count of remaining targets to try in this loop
      # update the progress bar, if there are targets left.  If all targets complete, 
      # pb will be updated at top of while loop before breaking from loop
      frac_complete <- num_targets_complete/num_targets_overall
      if (verbose && frac_complete < 1){
        pb$update(
          frac_complete, 
          tokens = list(what = sprintf('  Finished try %s, %s targets left', this_try, num_targets_incomplete)))
      }
      parallel::stopCluster(cl)
    }
    this_try <- this_try + 1
    
  }
  # check for completeness the quick way one last time; if we didn't make it
  # this far even according to our heuristic (values in incomplete_targets,
  # based on file presence and success/failure of individual task builds), then
  # don't try remake-checking the entire job
  num_targets_incomplete <- length(incomplete_targets)
  if(num_targets_incomplete > 0) {
    stop(sprintf("All tries are exhausted, but %s tasks remain", num_targets_incomplete))
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
