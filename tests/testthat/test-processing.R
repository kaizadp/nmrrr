

test_that("binsets work",{
  expect_error(set_bins("Clem"), "too many options!")
  expect_error(set_bins("aaa"), "option not available")

  clem_old <- read.csv("compdata/Clemente2012_bins.csv")
  clem_new <- set_bins("Clemente2012")

  expect_equal(dim(clem_old), dim(clem_new))

  expect_type(clem_new$number, "integer")

})

test_that("import-spectra works",{
  # files in correct format
  expect_error(import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra-error"),
               "no .csv files found!")

  spectra_test = import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra")

  expect_type(spectra_test, "list")
  expect_type(spectra_test$source, "character")
  expect_named(spectra_test, c("ppm", "intensity", "source"))
  # ^^ modify this so we can test the presence of "ppm" and "intensity", in any order.

  spectra_old = read.csv("compdata/spectra_processed_test.csv") %>% mutate(source = as.character(source))

  expect_equal(dim(spectra_test), dim(spectra_old))
  expect_equal(spectra_test, spectra_old, ignore_attr = TRUE)
})

