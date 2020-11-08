# rounding functions to allow testthat snapshotting to work

round_5_xs <- function(x) {
  lapply(x, round, 5)
}

round_5_xss <- function(x) {
  lapply(x, function(y) { lapply(y, round, 5) })
}


round_numeric_df <- function(x) {
  numeric_cols <- sapply(x, is.numeric)
  x[numeric_cols] <- round(x[numeric_cols], 5)
  x
}

round_list_of_dfs <- function(x) {
  lapply(x, function(y) {
    round_numeric_df(y)
  })
}
