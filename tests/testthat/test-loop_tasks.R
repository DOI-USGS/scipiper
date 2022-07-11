context("loop_tasks")

test_that("can create task_plan", {
  dirinfo <- setup_tasks_demo()
  expect_true(file.exists('remake.yml')) # determines whether the following testthat has a prayer of working
  task_plan <- scmake('task_plan')
  # some actual tests here would be nice
  cleanup_tasks_demo(dirinfo)
})
test_that("can create task_makefile", {
  dirinfo <- setup_tasks_demo()
  scmake('models.yml')
  expect_true(file.exists('models.yml'))
  # more tests here would be nice
  cleanup_tasks_demo(dirinfo)
})
test_that("can run loop_tasks to completion even when tasks fail sometimes", {
  dirinfo <- setup_tasks_demo()

  # with verbose=TRUE, should see errors but also completion
  suppressWarnings(RNGversion("3.5.0")) # R versions >3.5.0 changes set.seed behavior
  set.seed(10)
  options('scipiper.test_verbose'=TRUE)
  output <- capture_messages(scmake('models.ind'))
  expect_true(all(file.exists('models.ind','AZ.ind','CA.ind','CO.ind')), info='all files get created')
  expect_true(grepl('Build completed', tail(output, 1)), info='completes successfully')
  expect_gt(length(grep('Starting loop attempt', output)), 1) # at least two loop attempts
  az_viz_attempts <- grep('- processing AZ, resource_C=C', output)
  az_viz_errors <- grep('\\* Error in process\\("AZ", "C"\\)', output)
  expect_gt(length(az_viz_errors), 0) # at least one error in visualizing AZ
  expect_equal(length(az_viz_attempts), length(az_viz_errors)+1, info='1 more attempt than num errors')
  expect_true(max(az_viz_attempts) > max(az_viz_errors), info='last one should be processing, not error')

  cleanup_tasks_demo(dirinfo)
})

test_that("parallel loop_tasks completes while handling errors", {
  skip(paste(
    'these tests work fine interactivecly as of 8/30/2019 but are failing within the R CMD check',
    'environment, where it appears it might be forbidden to use 4 simultaneous processes.'
  ))
  
  dirinfo <- setup_tasks_demo()
  # with verbose=TRUE, should see errors but also completion
  # error messages don't seem to be passed in from parallel processes?
  suppressWarnings(RNGversion("3.5.0")) # R versions >3.5.0 changes set.seed behavior
  set.seed(100)
  options('scipiper.test_verbose'=TRUE)
  output <- capture_messages(scmake('models_parallel.ind'))
  expect_true(all(file.exists('models_parallel.ind','AZ.ind','CA.ind','CO.ind')), info='all files get created')
  expect_true(grepl('Build completed', tail(output, 1)), info='completes successfully')
  expect_gt(length(grep('Starting loop attempt', output)), 1) # at least two loop attempts
  cleanup_tasks_demo(dirinfo)
})

test_that("with verbose=FALSE, should see just one progress bar per loop attempt", {
  
  # skip this whole bundle of tests with an explanation
  skip(paste(
    'progress package seems not to print progress bars in the test environment, so',
    'this test bundle works locally as of 12/19/18 but fails with devtools::test()'))

  dirinfo <- setup_tasks_demo()

  # with verbose=FALSE, should see just one progress bar per loop attempt (shows
  # up in output as a consecutive series of written-over progress bars with just
  # one that's not written over by a \r)
  suppressWarnings(RNGversion("3.5.0")) # R versions >3.5.0 changes set.seed behavior
  set.seed(100)
  options('scipiper.test_verbose'=NULL)
  output <- capture_messages(scmake('models.ind'))
  pb_lines <- grep('\\[=*>*-*\\]', output)
  r_lines <- grep('\r', output)
  final_pb_lines <- output[pb_lines[-which((pb_lines+1) %in% r_lines)]]
  expect_equal(length(final_pb_lines), 1)
  expect_true(all(grepl('All tasks complete', final_pb_lines)))

  # to manually view output, including hidden lines:
  # cat(gsub('\r', '\n', output), sep='')
  cleanup_tasks_demo(dirinfo)
})

test_that("loop_tasks can force rebuild", {
  dirinfo <- setup_tasks_demo()
  
  # Kick off first build of loop tasks
  task_plan <- scmake("task_plan")
  scmake('models.yml')
  expect_true(file.exists('models.yml'))
  options('scipiper.test_verbose'=TRUE)
  loop_tasks(task_plan, 'models.yml')
  expect_true(all(file.exists("AZ.ind", "CA.ind", "CO.ind", "models.ind")))
  
  # Now, force a rebuild in loop_tasks
  options('scipiper.test_verbose'=TRUE)
  output <- capture_messages(loop_tasks(task_plan, 'models.yml', force=TRUE))
  expect_true(all(file.exists("AZ.ind", "CA.ind", "CO.ind", "models.ind")))
  start_final_phase <- grep('### Final check', output)
  initial_phase <- output[seq_len(start_final_phase-1)]
  final_phase <- output[start_final_phase:length(output)]
  
  # Expect AZ.ind, CA.ind, and CO.ind to be built during the initial phase
  # Right now, progress bars don't output messages during non-interactive executions, 
  # so we have to look for deletions of the ind files rather than building the new ones
  # expect_true(any(grepl('processing AZ', initial_phase)))
  expect_true(any(grepl('file.remove\\("./AZ.ind"', initial_phase)))
  expect_true(any(grepl('file.remove\\("./CA.ind"', initial_phase)))
  expect_true(any(grepl('file.remove\\("./CO.ind"', initial_phase)))
  expect_false(any(grepl('combine_to_ind', initial_phase))) #\\("./models.ind"
  
  # Expect models.ind to be built during the final phase
  expect_false(all(grepl('Building AZ', final_phase)))
  expect_false(all(grepl('Building CA', final_phase)))
  expect_false(all(grepl('Building CO', final_phase)))
  expect_true(any(grepl('combine_to_ind', final_phase))) #\\("./models.ind"
  
  options('scipiper.test_verbose'=NULL)
  cleanup_tasks_demo(dirinfo)
})
