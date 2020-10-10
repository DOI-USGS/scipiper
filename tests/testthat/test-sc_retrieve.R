context("Retrieve files from the shared cache")

test_that("sc_retrieve looks to getters.yml by default", {
  dirinfo <- setup_tasks_demo('tasks_demo_sc')
  
  expect_equal(sc_retrieve('my_table.csv.ind'), 'my_table.csv', info="build table via sc_retrieve; assumes getters.yml")
  expect_true(file.exists('my_table.csv'))
  expect_equal(scmake('nrow_table'), 5)

  cleanup_tasks_demo(dirinfo)
})

