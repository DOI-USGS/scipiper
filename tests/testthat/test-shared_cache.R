context("remake - shared cache")

test_that("a1", {
  # from scripts only
  di <- setup_demo(NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "note A; get A; make B; cache B; note B; noget B")
})

test_that("a2", {
  # fresh start
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
})

test_that("a3", {
  # odd: built remotely, make up a B.rds locally
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  saveRDS('A1B1', 'B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
})

test_that("a4", {
  # new to project; only have .st
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a5", {
  # already had A; adding step B
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('A.txt')
  msg <- capture_make('B.rds')
  expect_equal(msg, "noget A; make B; cache B; note B; noget B")
})

test_that("a6", {
  # pull built sts, get B
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})

test_that("a7a", {
  # pull built sts, get A, make up the correct B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('A.txt')
  saveRDS('A1B1', 'B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "noget B")
})
test_that("a7b", {
  # pull built sts, get A, make up an incorrect B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('A.txt')
  saveRDS('A1B7', 'B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a8", {
  # pulled A,B.st; got A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('A.txt')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a9", {
  # B is fully built
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  msg1 <- capture_make('B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})
