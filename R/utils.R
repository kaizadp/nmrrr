
# From utils.R in https://github.com/ropensci/drake/
# (but originally written by BBL)

weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}

weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::tibble(...)
  }
}
