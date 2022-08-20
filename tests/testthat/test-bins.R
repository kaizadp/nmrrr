
test_that("bin datasets are well structured", {

  # Get the names of our bin datasets
  d <- data(package = "nmrrr")
  nm <- d$results[, "Item"]
  nm <- nm[grep("^bins_", nm)]

  for(bin_name in nm) {
    bd <- get(bin_name)
    expect_s3_class(bd, "data.frame")

    # should have default column names
    expect_identical(sort(colnames(bd)),
                     sort(c("number", "group", "start", "stop", "description")),
                     info = bin_name)
    # number column should be 1..n
    expect_identical(seq_len(nrow(bd)), bd$number, info = bin_name)
    # start values should be strictly ascending
    expect_true(all(bd$start[-1] > head(bd$start, -1)), info = bin_name)
    # stop values should be >= start values
    expect_true(all(bd$stop >= bd$start), info = bin_name)
    # stop values should be >= the previous start values
    expect_true(all(bd$stop[-1] > head(bd$start, -1)), info = bin_name)
  }

})
