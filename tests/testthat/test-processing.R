
test_that("binsets work", {
  expect_error(set_bins("Clem"), "too many options!")
  expect_error(set_bins("aaa"), "option not available")

  clem_old <- read.csv("compdata/Clemente2012_bins.csv")
  clem_new <- set_bins("Clemente2012")

  expect_equal(dim(clem_old), dim(clem_new))

  expect_type(clem_new$number, type = "integer")
})

test_that("import-spectra works", {
  # Handles empty directory
  expect_error(import_nmr_spectra_data(SPECTRA_FILES = "./",
                                       METHOD = "mnova"),
    regexp = "No files found!"
  )

  # Handles bad method
  sdir <- "compdata/spectra"
  expect_error(import_nmr_spectra_data(SPECTRA_FILES = sdir,
                                       METHOD = "not_a_valid_method"),
               regexp = "Appropriate methods are"
  )

  # Imports data in expected format
  spectra_test <- import_nmr_spectra_data(SPECTRA_FILES = sdir,
                                          METHOD = "mnova")

  expect_s3_class(spectra_test, "data.frame")
  expect_identical(sort(names(spectra_test)),
                   sort(c("ppm", "intensity", "sampleID")))
  expect_type(spectra_test$ppm, "double")
  expect_type(spectra_test$intensity, "double")
  expect_type(spectra_test$sampleID, "character")
})

test_that("binset-assignment works", {
  spectra_binsets_old <- read.csv("compdata/spectra_binset_test.csv")
  spectra_binsets_old$sampleID <- as.character(spectra_binsets_old$sampleID)

  spectra_test <- import_nmr_spectra_data(SPECTRA_FILES = "compdata/spectra", METHOD = "mnova")
  spectra_binsets_new <- assign_compound_classes(dat = spectra_test, BINSET = "Clemente")

  expect_equal(dim(spectra_binsets_new), dim(spectra_binsets_old))
  expect_equal(names(spectra_binsets_new), names(spectra_binsets_old))
})
