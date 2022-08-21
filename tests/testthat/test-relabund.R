
test_that("relabund works", {

  # Handles bad method
  expect_error(compute_relabund_cores(dat = 1, method = "not_valid"),
               regexp = "Available methods")

  # AUC: expected data format
  spectra_test <- import_nmr_spectra_data(path = "compdata/spectra",
                                          method = "mnova", quiet = TRUE)
  spectra_binset <- assign_compound_classes(dat = spectra_test,
                                            binset = bins_Clemente2012)
  x <- compute_relabund_cores(dat = spectra_binset, method = "AUC")

  expect_s3_class(x, "data.frame")
  expect_named(x, expected = c("sampleID", "group", "relabund"))
  expect_type(x$sampleID, "character")
  expect_type(x$group, "character")
  expect_type(x$relabund, "double")

  # ...each group should sum to ~100
  vals <- sapply(split(x$relabund, x$sampleID), sum)
  expect_true(all(vals - 100.0 < 1e-6))

  # peaks: handles bad data
  expect_error(compute_relabund_cores(dat = spectra_binset, method = "peaks"),
               regexp = "No 'Area' column")

  # peaks/single: expected data format
  single_dir <- "compdata/peaks_mnova_single"
  single_test <- process_peaks(path = single_dir,
                               method = "single column", quiet = TRUE)
  single_binset <- assign_compound_classes(dat = single_test,
                                           binset = bins_Clemente2012)
  x <- compute_relabund_cores(dat = single_binset, method = "peaks")

  expect_s3_class(x, "data.frame")
  expect_named(x, expected = c("sampleID", "group", "relabund"))
  expect_type(x$sampleID, "character")
  expect_type(x$group, "character")
  expect_type(x$relabund, "double")

  # ...each group should sum to ~100
  vals <- sapply(split(x$relabund, x$sampleID), sum)
  expect_true(all(vals - 100.0 < 1e-6))

})
