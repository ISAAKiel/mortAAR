# rounding functions to allow testthat snapshotting to work

round_5_xs <- function(x) {
  lapply(x, round, 5)
}

round_5_xss <- function(x) {
  lapply(x, function(y) { lapply(y, round, 5) })
}
