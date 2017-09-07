context("remake - shared cache")

source('tests/testthat/helper-shared_cache.R')

test_that("a1", {
  di <- setup_demo()
  on.exit(cleanup_demo(di))
  
  make_file("A.txt.cache", "A1")
  make_file("R.R",         "R <- 1")
  msg <- capture_make('B.rds')
  expect_equal(msg, "note A; get A; make B; cache B; note B; noget B")
})

test_that("a2", {
  di <- setup_demo()
  on.exit(cleanup_demo(di))
  
  make_file("A.txt.cache", "A1")
  msg <- capture_make('B.rds')
  expect_equal(msg, "note A; get A; make B; cache B; note B; noget B")
})

# 
# scenario <- function(scenario_code) {
#   remake::make('clean', verbose=FALSE) # 'tidy' removes objects; 'clean' removes objects & files
#   suppressWarnings(file.remove(dir(pattern='.cache')))
#   switch(
#     scenario_code,
#     a1 = {
#       make_file("A.txt.cache", "A1")
#       make_file("R.R",         "R <- 1")
#       message("make B.rds should pull A.txt, make B.rds/.cache/.st, not re-pull B.rds")
#     },
#     a2 = {
# 
#     },
#     a3 = {
#       make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
#       make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
#       message("make B.rds should pull B.rds")
#     },
#     a4 = {
#       make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt",      "11", "2017-08-25 11:00:00 UTC")
#       make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
#       message("make B.rds should pull B.rds from cache")
#     },
#     a5 = {
#       make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
#       make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds",                            "10", "2017-08-25 07:00:00 UTC")
#       message("make B.rds should pull B.rds from cache")
#     },
#     a6 = {
#       make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
#       make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds",      "10", "2017-08-25 10:00:00 UTC")
#       message("make B.rds should do nothing")
#     },
#     a7 = {
#       make_file("A.txt.cache", "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt.st",    "8", "2017-08-25 08:00:00 UTC")
#       make_file("A.txt",       "8", "2017-08-25 08:00:00 UTC")
#       make_file("B.rds.cache", "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds.st",    "9", "2017-08-25 09:00:00 UTC")
#       make_file("B.rds",      "10", "2017-08-25 10:00:00 UTC")
#       message("make B.rds should do nothing")
#     },
#     b1 = {
# 
#       message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
#     },
#     b2 = {
# 
#       message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
#     },
#     b3 = {
# 
#       message("make B.rds should make B.rds/.st/.cache, not re-pull B.rds")
#     },
#     b4 = {
# 
#       message("make B.rds should pull A.txt, make B.rds/.st/.cache, not re-pull B.rds")
#     },
#     c1 = {
# 
#       message("make B.rds should give error about missing A.txt.cache")
#     },
#     c2 = {
# 
#       message("make B.rds should give error about missing A.txt.cache")
#     },
#     c3 = {
# 
#       message("make B.rds should give error about missing A.txt.cache")
#     },
#     c4 = {
# 
#       message("make B.rds should give error about missing A.txt.cache")
#     }
#   )
# }
# 
