context("Combine task targets into summary files or objects")

test_that("combine_to_tibble works on all reasonable numbers of files", {
  tfiles <- tempfile(pattern=as.character(1:3))
  for(i in 1:3) readr::write_lines(i, file=tfiles[i])
  expect_s3_class(combine_to_tibble(), 'tbl_df')
  expect_equal(nrow(combine_to_tibble()), 0)
  expect_equal(combine_to_tibble(tfiles[1])$name, tfiles[1])
  expect_equal(combine_to_tibble(tfiles[1])$hash, unname(tools::md5sum(tfiles[1])))
  expect_equal(combine_to_tibble(tfiles[1]), combine_to_tibble(tfiles)[1,])
})

