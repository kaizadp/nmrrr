
test_that("weak_as_tibble works", {
  if(requireNamespace("tibble", quietly = TRUE)) {
    crs <- weak_as_tibble(cars)
    expect_s3_class(crs, "tbl")
    crs_df <- weak_as_tibble(cars, .force_df = TRUE)
    expect_s3_class(crs, "data.frame")
    expect_true(!"tbl" %in% class(crs_df))
    # tibble and data.frame versions should be equivalent
    expect_equal(crs, crs_df, ignore_attr = TRUE)
  } else {
    # not much we can test without tibble!
    crs <- weak_as_tibble(cars)
    expect_s3_class(crs, "data.frame")
    expect_identical(x, weak_tibble(x = 1, .force_df = TRUE))
  }
})


test_that("weak_tibble works", {
  if(requireNamespace("tibble", quietly = TRUE)) {
    x <- weak_tibble(x = 1)
    expect_s3_class(x, "tbl")
    x_df <- weak_tibble(x = 1, .force_df = TRUE)
    expect_s3_class(x_df, "data.frame")
    expect_true(!"tbl" %in% class(x_df))
    # tibble and data.frame versions should be equivalent
    expect_equal(x, x_df, ignore_attr = TRUE)
  } else {
    # not much we can test without tibble!
    x_df <- weak_tibble(x = 1)
    expect_s3_class(x_df, "data.frame")
    expect_identical(x_df, weak_tibble(x = 1, .force_df = TRUE))
  }
})

