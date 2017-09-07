context("remake - shared cache")

test_that("a1", {
  # from scripts only
  di <- setup_demo(Ac=1, Ai=NA, Ad=NA, R=1, Bc=NA, Bi=NA, Bd=NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "note A; get A; make B; cache B; note B; noget B")
})

test_that("a2", {
  # fresh start
  di <- setup_demo(Ac=1, Ai=1, Ad=NA, R=1, Bc=NA, Bi=NA, Bd=NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
})

test_that("a3a", {
  # odd: built, then deleted A.txt & B.rst.st
  di <- setup_demo(Ac=1, Ai=1, Ad=NA, R=1, Bc=NA, Bi=NA, Bd=1)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B")
  # remake doesn't bother checking B.rds, even though it just remade B.rds, because B.rds's dependencies are looking fine by the time we get there
})
test_that("a3b", {
  # odd: built, then deleted A.txt & B.rst.st
  di <- setup_demo(Ac=1, Ai=1, Ad=NA, R=1, Bc=NA, Bi=NA, Bd=1)
  on.exit(cleanup_demo(di))
  file.copy('B.rds','B.rds.save')
  remake::delete('B.rds')
  file.copy('B.rds.save','B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
  # remake DOES bother checking B.rds, even though it just remade B.rds, because we deleted B.rds's dependency expectation
})

test_that("a4", {
  # new to project; only have .st
  di <- setup_demo(Ac=1, Ai=1, Ad=NA, R=1, Bc=1, Bi=1, Bd=NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a5", {
  # adding step B
  di <- setup_demo(Ac=1, Ai=1, Ad=1, R=1, Bc=NA, Bi=NA, Bd=NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "noget A; make B; cache B; note B; noget B")
})

test_that("a6", {
  # pull built sts, get B
  di <- setup_demo(Ac=1, Ai=1, Ad=NA, R=1, Bc=1, Bi=1, Bd=1)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})

test_that("a7", {
  # odd: built, then deleted B.st
  di <- setup_demo(Ac=1, Ai=1, Ad=1, R=1, Bc=NA, Bi=NA, Bd=1)
  on.exit(cleanup_demo(di))
  file.copy('B.rds','B.rds.save')
  remake::delete('B.rds')
  file.copy('B.rds.save','B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "noget A; make B; cache B; note B; noget B")
})

test_that("a8", {
  # pulled A,B.st; got A
  di <- setup_demo(Ac=1, Ai=1, Ad=1, R=1, Bc=1, Bi=1, Bd=NA)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a9", {
  # B is fully built
  di <- setup_demo(Ac=1, Ai=1, Ad=1, R=1, Bc=1, Bi=1, Bd=1)
  on.exit(cleanup_demo(di))
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})
