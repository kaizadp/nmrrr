

test_that("binsets work",{
  expect_error(set_bins("20"))

  clem_old <- read.csv("compdata/Clemente2012_bins.csv")
  clem_new <- set_bins("Clemente2012")

  expect_equal(dim(clem_old), dim(clem_new))

  expect_type(clem_new$number, "double")

})
