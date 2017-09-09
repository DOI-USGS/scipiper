context("shared cache - a")
# "a" series of scenarios: git pull a fresh repo from remote repository, use
# scmake to pick up where collaborators left off with respect to the shared
# cache

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
  capture_make('A.txt')
  msg <- capture_make('B.rds')
  expect_equal(msg, "make B; cache B; note B; noget B")
})

test_that("a6", {
  # pull built sts, get B
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})

test_that("a7a", {
  # pull built sts, get A, make up the correct B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  saveRDS('A1B1', 'B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "noget B")
})
test_that("a7b", {
  # pull built sts, get A, make up an incorrect B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  saveRDS('A1B7', 'B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a8", {
  # pulled A,B.st; got A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  msg <- capture_make('B.rds')
  expect_equal(msg, "get B")
})

test_that("a9", {
  # B is fully built
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  msg <- capture_make('B.rds')
  expect_equal(msg, "")
})

context("shared cache - b")
# "b" series of scenarios: git pull twice, once when A.txt='A1' and then later
# when it's 'A2'. Use scmake to notice and handle the changes to the shared
# cache

test_that("b1-RDSify", {
  # pull once, remote updates to A2, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  stat <- get_remake_status('B.rds')
  expect_equal(stat$dirty, c(T,T,T)) # before RDSify or first build, remake figures we're all dirty
  scipiper:::RDSify_build_status()
  stat <- get_remake_status('B.rds')
  expect_equal(stat$dirty, c(F,T,T)) # RDSifying saves us from wanting to rebuild A
})

test_that("b1", {
  # pull once, remote updates to A2, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  loc1 <- unname(inspect_local(di))
  expect_equal(loc1, c('A2','[A2]','---','1','A1B1','[A1B1]','---'))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
  loc2 <- unname(inspect_local(di))
  expect_equal(loc2, c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b2", {
  # pull once, get B, remote updates to A2 and deletes B, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target=c('A.txt.st','B.rds.st'), remove='B.rds.cache')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','---','1','---','---','A1B1'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b3", {
  # pull once, get A, remote updates to A2 and deletes B, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target=c('A.txt.st','B.rds.st'), remove='B.rds.cache')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','---','---','---'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b4", {
  # pull once, get B, remote updates to A2 and doesn't touch B, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('B.rds'))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target=c('A.txt.st'))
  stat <- get_remake_status('B.rds')
  expect_equal(stat$dirty | stat$dirty_by_descent, c(F,T,T))
  msg <- capture_make('B.rds')
  expect_equal(msg, "get A; make B; cache B; note B; noget B")
})

test_that("b5", {
  # pull once, get B, remote updates to A2 & build all, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('B.rds'))
  develop_remote_A2(di, remote_target='B.rds', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','---','1','A2B1','[A2B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','---','1','A2B1','[A2B1]','A2B1'))
})

# b6 is silly - B.rds=B2 while B.rds.cache=B1
# b7 is silly - B.rds=B1 while B.rds.cache is missing
# b8 is silly - B.rds=B1 while B.rds.cache is missing
# b9 is silly - B.rds=B2 while B.rds.cache is missing

test_that("b10", {
  # pull once, get A, remote updates to A2, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt'))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b11", {
  # pull once, remote updates to A2, pull again, get A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  capture_make(c('A.txt'))
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b12", {
  # pull once, get A, remote updates to A2 & builds, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt'))
  develop_remote_A2(di, remote_target='B.rds', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','---'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','A2B1'))
})

test_that("b13", {
  # pull once, get A, remote updates to A2 & builds, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt','B.rds'))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b14", {
  # pull once, get B, remote updates to A2 & builds, pull again, get A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  capture_make('A.txt')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

test_that("b15", {
  # pull once, get A & B, remote updates to A2 & full build, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt','B.rds'))
  develop_remote_A2(di, remote_target='B.rds', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','A2B1'))
})

test_that("b16", {
  # pull once, get A & B, remote updates to A2 & full build, pull again, get A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt','B.rds'))
  develop_remote_A2(di, remote_target='B.rds', rdelete_target='A.txt.st')
  capture_make('A.txt')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
})

# b17 is silly - B.rds=B1 while B.rds.cache is missing
# b18 is silly - B.rds=B1 while B.rds.cache is missing

test_that("b19", {
  # pull once, get A, remote updates to A2 & full build, pull again, get B
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  develop_remote_A2(di, remote_target='B.rds', rdelete_target='A.txt.st')
  capture_make('B.rds')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','A2B1'))
  expect_equal(capture_make('B.rds'), "")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A1','1','A2B1','[A2B1]','A2B1'))
})
