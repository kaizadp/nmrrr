
test_that("import_nmr_spectra_data works", {
  # Handles empty directory
  expect_error(import_nmr_spectra_data(path = "./", method = "mnova", quiet = TRUE),
               regexp = "No files found!"
  )

  # Handles bad method
  sdir <- "compdata/spectra"
  expect_error(import_nmr_spectra_data(path = sdir, method = "not_valid", quiet = TRUE),
               regexp = "Appropriate methods are"
  )

  # Respects quiet parameter
  expect_message(import_nmr_spectra_data(path = sdir, method = "mnova", quiet = FALSE),
                 regexp = "Found")
  expect_silent({
    spectra_test <- import_nmr_spectra_data(path = sdir, method = "mnova", quiet = TRUE)
  })

  # Imports data in expected format
  expect_s3_class(spectra_test, "data.frame")
  expect_identical(sort(names(spectra_test)),
                   sort(c("ppm", "intensity", "sampleID")))
  expect_type(spectra_test$ppm, "double")
  expect_type(spectra_test$intensity, "double")
  expect_type(spectra_test$sampleID, "character")
})

test_that("assign_compound_classes works", {

  spectra_test <- import_nmr_spectra_data(path = "compdata/spectra",
                                          method = "mnova", quiet = TRUE)
  spectra_binsets_new <- assign_compound_classes(dat = spectra_test,
                                                 binset = bins_Clemente2012)

  # 'group' character column added
  expect_identical(nrow(spectra_test), nrow(spectra_binsets_new))
  expect_type(spectra_binsets_new$group, "character")
  # ...and its entries are all from the binset
  g <- na.omit(spectra_binsets_new$group)
  expect_true(all(unique(g) %in% bins_Clemente2012$group))
})
