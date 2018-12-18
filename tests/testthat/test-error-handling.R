context("Update build files even after errors")
test_that("Build files are still updated after errors",{
  dirinfo <- setup_tasks_demo()
  build_files <- list.files('build/status')
  file.remove(build_files)
  file.remove(list.files(pattern = ".ind"))
  expect_error(scmake("test_fail"))
  build_files <- list.files('build/status')
  expect_equal(length(build_files), 4)
  cleanup_tasks_demo(dirinfo)
})

