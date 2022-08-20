
test_that("binsets work", {
  expect_error(set_bins("Clem"), "too many options!")
  expect_error(set_bins("aaa"), "option not available")

  clem_old <- read.csv("compdata/Clemente2012_bins.csv")
  clem_new <- set_bins("Clemente2012")

  expect_equal(dim(clem_old), dim(clem_new))

  expect_type(clem_new$number, type = "integer")
})

test_that("import-spectra works", {
  # files in correct format
  expect_error(import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra-error",
                                       METHOD = "mnova"),
    regexp = "no .csv files found!"
  )

  spectra_test <- import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra",
                                          METHOD = "mnova")

  expect_type(spectra_test, "list")
  expect_type(spectra_test$sampleID, "character")
  expect_named(spectra_test, c("ppm", "intensity", "sampleID"))
  # ^^ modify this so we can test the presence of "ppm" and "intensity", in any order.

  spectra_old <- read.csv("compdata/spectra_processed_test2.csv")
  spectra_old$sampleID <- as.character(spectra_old$sampleID)

  expect_equal(dim(spectra_test), dim(spectra_old))
  expect_equal(spectra_test, spectra_old, ignore_attr = TRUE)
})

test_that("binset-assignment works", {
  spectra_binsets_old <- read.csv("compdata/spectra_binset_test.csv")
  spectra_binsets_old$sampleID <- as.character(spectra_binsets_old$sampleID)

  spectra_test <- import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra", METHOD = "mnova")
  spectra_binsets_new <- assign_compound_classes(dat = spectra_test, BINSET = "Clemente")

  expect_equal(dim(spectra_binsets_new), dim(spectra_binsets_old))
  expect_equal(names(spectra_binsets_new), names(spectra_binsets_old))
})
