context("shared cache - a")
# "a" series of scenarios: git pull a fresh repo from remote repository, use
# scmake to pick up where collaborators left off with respect to the shared
# cache

test_that("a1", {
  # from scripts only
  di <- setup_demo(NA)
  on.exit(cleanup_demo(di))
  expect_equal(unname(inspect_local(di)), c('A1','---','---','1','---','---','---'))
  expect_equal(capture_make('B.rds'), "note A; get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a2", {
  # fresh start
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','---','---','---'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a3", {
  # odd: built remotely, make up a B.rds locally
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  saveRDS('A1B1', 'B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','---','---','A1B1'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a4", {
  # new to project; only have .st
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("a5", {
  # already had A; adding step B
  di <- setup_demo('A.txt.st')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','---','---','---'))
  expect_equal(capture_make('B.rds'), "make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a6", {
  # pull built sts, get B
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("a7a", {
  # pull built sts, get A, make up the correct B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  saveRDS('A1B1', 'B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})
test_that("a7b", {
  # pull built sts, get A, make up an incorrect B.rds locally
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  saveRDS('A1B7', 'B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B7'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a8", {
  # pulled A,B.st; got A
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','1','A1B1','[A1B1]','A1B1'))
})

test_that("a9", {
  # B is fully built
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
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
  expect_equal(stat$dirty, c(T,F,T,T)) # before RDSify or first build, remake figures we're all dirty
  scipiper:::RDSify_build_status()
  stat <- get_remake_status('B.rds')
  expect_equal(stat$dirty, c(F,F,T,T)) # RDSifying saves us from wanting to rebuild A
})

test_that("b1", {
  # pull once, remote updates to A2, pull again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_remote_A2(di, remote_target='A.txt.st', rdelete_target='A.txt.st')
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','---','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
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
  expect_equal(stat$dirty | stat$dirty_by_descent, c(F,F,T,T))
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','---','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A2','[A2]','A2','1','A2B1','[A2B1]','A2B1'))
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

context("shared cache - c")
# "c" series of scenarios: git pull twice, once when R.R='R <- 1' and then later
# when it's 'R <- 3'. Use scmake to notice and handle the changes required

test_that("c1", {
  # pull once, update R3
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_local_R3(di)
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','3','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get A; make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B3','[A1B3]','A1B3'))
})

test_that("c2", {
  # pull once, get A, update R3
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('A.txt')
  develop_local_R3(di)
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B3','[A1B3]','A1B3'))
})

# c3 is redundant with a3 because everything's locally up to date already, and
# silly b/c how would you get B3 status but no B.txt if B3 is due to local
# edits?

test_that("c4", {
  # pull once, get A, update R3
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make(c('A.txt','B.rds'))
  develop_local_R3(di)
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "make B; cache B; note B; noget B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B3','[A1B3]','A1B3'))
})

# c5 is redundant with b16 because only B.txt is out of date, and silly b/c how
# would you get B3 status but B1 file if B3 is due to local edits?

test_that("c6", {
  # pull once, get A, update R3, build all, try to build again
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  develop_local_R3(di)
  capture_make('B.rds')
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B3','[A1B3]','A1B3'))
  expect_equal(capture_make('B.rds'), "")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','A1','3','A1B3','[A1B3]','A1B3'))
})

context("shared cache - d")
# "d" series of scenarios: Various forms of corruption: missing or out of date
# cached files or .st files

test_that("d1", {
  # pull once, delete A.st or collab forgot to commit it or we've just never created A.st
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_output(scdel('A.txt.st'))
  expect_equal(unname(inspect_local(di)), c('A1','---','---','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "note A; get B")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("d2", {
  # pull once, make B, delete A.st
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  capture_output(scdel('A.txt.st'))
  expect_equal(unname(inspect_local(di)), c('A1','---','---','1','A1B1','[A1B1]','A1B1'))
  expect_equal(capture_make('B.rds'), "note A")
  expect_equal(unname(inspect_local(di)), c('A1','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("d3", {
  # pull once, delete A.st AND A.cache
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_output(scdel('A.txt.st'))
  file.remove('A.txt.cache')
  expect_equal(unname(inspect_local(di)), c('---','---','---','1','A1B1','[A1B1]','---'))
  expect_error(capture_make('B.rds'), "A.txt is missing from the cache")
})

test_that("d4", {
  # pull once, make B, delete A.st AND A.cache
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  capture_make('B.rds')
  capture_output(scdel('A.txt.st'))
  file.remove('A.txt.cache')
  expect_equal(unname(inspect_local(di)), c('---','---','---','1','A1B1','[A1B1]','A1B1'))
  expect_error(capture_make('B.rds'), "A.txt is missing from the cache")
})

test_that("d5", {
  # pull once, delete A.cache (corrupt cache)
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  file.remove('A.txt.cache')
  expect_equal(unname(inspect_local(di)), c('---','[A1]','---','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('---','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("d5", {
  # pull once, delete A.cache (corrupt cache)
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  file.remove('A.txt.cache')
  expect_equal(unname(inspect_local(di)), c('---','[A1]','---','1','A1B1','[A1B1]','---'))
  expect_equal(capture_make('B.rds'), "get B")
  expect_equal(unname(inspect_local(di)), c('---','[A1]','---','1','A1B1','[A1B1]','A1B1'))
})

test_that("d6", {
  # pull once, delete A.cache (corrupt cache), edit R
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  file.remove('A.txt.cache')
  develop_local_R3(di)
  expect_equal(unname(inspect_local(di)), c('---','[A1]','---','3','A1B1','[A1B1]','---'))
  expect_error(capture_make('B.rds'), "despite A.txt.st, missing A.txt.cache")
})

test_that("d7", {
  # pull once, delete A.cache (corrupt cache), edit R
  di <- setup_demo('B.rds')
  on.exit(cleanup_demo(di))
  writeLines('A2', 'A.txt.cache')
  develop_local_R3(di)
  expect_equal(unname(inspect_local(di)), c('A2','[A1]','---','3','A1B1','[A1B1]','---'))
  expect_error(capture_make('B.rds'), "despite A.txt.st, badhash A.txt.cache")
})
