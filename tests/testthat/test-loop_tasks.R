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
  set.seed(100)
  options('scipiper.test_verbose'=TRUE)
  output <- capture_messages(scmake('models.ind'))
  expect_true(all(file.exists('models.ind','AZ.ind','CA.ind','CO.ind')), info='all files get created')
  expect_true(grepl('Build completed', tail(output, 1)), info='completes successfully')
  expect_gt(length(grep('Starting loop attempt', output)), 1) # at least two loop attempts
  az_viz_attempts <- grep('- visualizing with prep=PROCESSED_AZ', output)
  az_viz_errors <- grep('\\* Error in visualize\\(AZ_prep, "Phoenix"\\)', output)
  expect_gt(length(az_viz_errors), 0) # at least one error in visualizing AZ
  expect_equal(length(az_viz_attempts), length(az_viz_errors)+1, info='1 more attempt than num errors')
  expect_true(max(az_viz_attempts) > max(az_viz_errors), info='last one should be processing, not error')
  
  # reset so we can try again with verbose=NULL
  scdel(scipiper::list_all_targets('models.yml'),'models.yml')
  scdel('models.ind')
  
  # with verbose=FALSE, should see just one progress bar per loop attempt (shows
  # up in output as a consecutive series of written-over progress bars with just
  # one that's not written over by a \r)
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

test_that("loop_tasks skips files initially", {
  dirinfo <- setup_tasks_demo()
  
  # if we already have CA.ind, the inital looping phase shouldn't try to build
  # CA, but if it's out of date, the final looping phase should
  writeLines('out-of-date file', 'CA.ind')
  options('scipiper.test_verbose'=TRUE)
  output <- capture_messages(scmake('models.ind'))
  start_final_phase <- grep('### Final check', output)
  initial_phase <- output[seq_len(start_final_phase-1)]
  final_phase <- output[start_final_phase:length(output)]
  expect_false(all(grepl('processing CA', initial_phase)))
  expect_true(any(grepl('processing CA', final_phase)))
  options('scipiper.test_verbose'=NULL)
  
  cleanup_tasks_demo(dirinfo)
})