
test_that("import_nmr_spectra_data works", {
  # Handles empty directory
  expect_error(import_nmr_spectra_data(path = "./", method = "mnova"),
    regexp = "No files found!"
  )

  # Handles bad method
  sdir <- "compdata/spectra"
  expect_error(import_nmr_spectra_data(path = sdir, method = "not_valid"),
               regexp = "Appropriate methods are"
  )

  # Imports data in expected format
  spectra_test <- import_nmr_spectra_data(path = sdir, method = "mnova")

  expect_s3_class(spectra_test, "data.frame")
  expect_identical(sort(names(spectra_test)),
                   sort(c("ppm", "intensity", "sampleID")))
  expect_type(spectra_test$ppm, "double")
  expect_type(spectra_test$intensity, "double")
  expect_type(spectra_test$sampleID, "character")
})

test_that("assign_compound_classes works", {

  spectra_test <- import_nmr_spectra_data(path = "compdata/spectra",
                                          method = "mnova")
  spectra_binsets_new <- assign_compound_classes(dat = spectra_test,
                                                 BINSET = bins_Clemente2012)

  expect_equal(dim(spectra_binsets_new), dim(spectra_binsets_old))
  expect_equal(names(spectra_binsets_new), names(spectra_binsets_old))
})
