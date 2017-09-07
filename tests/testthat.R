Sys.setenv("R_TESTS" = "")
library(testthat)
library(scipiper)
test_check('scipiper', reporter="summary")
