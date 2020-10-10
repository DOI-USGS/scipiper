build_share_table <- function(x, y, out_ind) {
  dir.create(dirname(out_ind), showWarnings = FALSE)
  tibble(x=x, y=y) %>%
    readr::write_csv(as_data_file(out_ind))
  gd_put(out_ind, dry_put=TRUE)
}

count_table_rows <- function(tbl_ind) {
  tbl <- sc_retrieve(tbl_ind, verbose=TRUE) %>%
    readr::read_csv(col_types=cols())
  return(nrow(tbl))
}

require_ind_exists <- function(ind) {
  if(!file.exists(ind)) {
    stop(sprintf('%s must be built before it can be retrieved', ind))
  }
}
