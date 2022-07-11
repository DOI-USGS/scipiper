context("Retrieve files from the shared cache")

test_that("sc_retrieve looks to getters.yml by default", {
  dirinfo <- setup_tasks_demo('tasks_demo_sc')
  expect_error(expect_warning(sc_retrieve('out/my_table.csv.ind'), info="expected: can't retrieve files that don't exist"))
  scmake('out/my_table.csv.ind')
  expect_true(file.exists('out/my_table.csv.ind'))
  expect_true(file.exists('out/my_table.csv'))
  expect_equal(sc_retrieve('out/my_table.csv.ind'), 'out/my_table.csv', info="assumes getters.yml")
  expect_equal(scmake('nrow_table'), 5)
  cleanup_tasks_demo(dirinfo)
  
  # Should also be able to build nrow_table directly without double builds
  dirinfo <- setup_tasks_demo('tasks_demo_sc')
  build_msgs <- testthat::capture_messages(scmake('nrow_table'))
  builds_of_ind <- grep('\\] out/my_table.csv.ind[[:space:]]+', build_msgs, value=TRUE)
  expect_equal(length(builds_of_ind), 1) # should build just once
  cleanup_tasks_demo(dirinfo)
})

