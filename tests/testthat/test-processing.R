
test_that("nmr_import_spectra works", {
  # Handles empty directory
  expect_error(nmr_import_spectra(path = "./", method = "mnova", quiet = TRUE),
    regexp = "No files found!"
  )

  # Handles bad method
  sdir <- "compdata/spectra"
  expect_error(nmr_import_spectra(path = sdir, method = "not_valid", quiet = TRUE),
    regexp = "Available methods"
  )

  # Respects quiet parameter
  expect_message(nmr_import_spectra(path = sdir, method = "mnova", quiet = FALSE),
    regexp = "Found"
  )
  expect_silent({
    spectra_test <- nmr_import_spectra(path = sdir, method = "mnova", quiet = TRUE)
  })

  # Imports data in expected format
  expect_s3_class(spectra_test, "data.frame")
  expect_identical(
    sort(names(spectra_test)),
    sort(c("ppm", "intensity", "sampleID"))
  )
  expect_type(spectra_test$ppm, "double")
  expect_type(spectra_test$intensity, "double")
  expect_type(spectra_test$sampleID, "character")
})


test_that("nmr_assign_bins works", {
  bin_midpoints <- rowSums(bins_Clemente2012[c("start", "stop")]) / 2

  spectra_test <- data.frame(ppm = c(
    bin_midpoints,
    # two out-of-range ppm values
    min(bins_Clemente2012$start) - 1,
    max(bins_Clemente2012$stop) + 1
  ))
  spectra_binsets_new <- nmr_assign_bins(
    dat = spectra_test,
    binset = bins_Clemente2012
  )

  # 'group' character column added
  expect_identical(nrow(spectra_test), nrow(spectra_binsets_new))
  expect_type(spectra_binsets_new$group, "character")
  # ...and its entries are all from the binset, with NAs for the o-o-b values
  expect_identical(
    c(bins_Clemente2012$group, NA_character_, NA_character_),
    spectra_binsets_new$group
  )
})


test_that("nmr_import_peaks works", {
  # Handles empty directory
  expect_error(nmr_import_peaks(path = "./", method = "topspin", quiet = TRUE),
    regexp = "No files found!"
  )

  # Handles bad method
  topspin_dir <- "compdata/peaks_topspin"
  expect_error(nmr_import_peaks(path = topspin_dir, method = "not_valid", quiet = TRUE),
    regexp = "Available methods"
  )

  # Respects quiet parameter
  expect_message(nmr_import_peaks(path = topspin_dir, method = "topspin", quiet = FALSE),
    regexp = "Found"
  )
  expect_silent({
    topspin_test <- nmr_import_peaks(path = topspin_dir, method = "topspin", quiet = TRUE)
  })

  # topspin: expected data format
  expect_s3_class(topspin_test, "data.frame")
  expect_identical(
    sort(names(topspin_test)),
    sort(c("peak", "ppm", "Intensity", "Annotation", "sampleID"))
  )
  expect_type(topspin_test$peak, "integer")
  expect_type(topspin_test$ppm, "double")
  expect_type(topspin_test$Intensity, "double")
  expect_type(topspin_test$Annotation, "character")
  expect_type(topspin_test$sampleID, "character")
  expect_identical(
    length(unique(topspin_test$sampleID)),
    length(list.files(topspin_dir, pattern = "*.csv"))
  )

  # mnova multiple: detects bad format
  expect_error(nmr_import_peaks(
    path = topspin_dir,
    method = "multiple columns", quiet = TRUE
  ),
  regexp = "Formatting problem"
  )
  # mnova multiple: expected data format
  multiple_dir <- "compdata/peaks_mnova_multiple"
  multiple_test <- nmr_import_peaks(
    path = multiple_dir,
    method = "multiple columns", quiet = TRUE
  )
  expect_identical(
    sort(names(multiple_test)),
    sort(c(
      "Obs", "ppm", "Intensity", "Width", "Area", "Type",
      "Flags", "Impurity/Compound", "Annotation", "sampleID"
    ))
  )
  expect_type(multiple_test$Obs, "integer")
  expect_type(multiple_test$ppm, "double")
  expect_type(multiple_test$Intensity, "double")
  expect_type(multiple_test$Width, "double")
  expect_type(multiple_test$Area, "double")
  expect_type(multiple_test$Type, "character")
  expect_type(multiple_test$Flags, "character")
  expect_type(multiple_test$`Impurity/Compound`, "character")
  expect_type(multiple_test$Annotation, "character")
  expect_type(multiple_test$sampleID, "character")
  expect_identical(
    length(unique(multiple_test$sampleID)),
    length(list.files(multiple_dir, pattern = "*.csv"))
  )

  # single: expected data format
  single_dir <- "compdata/peaks_mnova_single"
  single_test <- nmr_import_peaks(
    path = single_dir,
    method = "single column", quiet = TRUE
  )
  expect_identical(
    sort(names(single_test)),
    sort(c(
      "ppm", "Intensity", "Width", "Area",
      "Impurity/Compound", "Annotation", "sampleID"
    ))
  )
  expect_type(single_test$ppm, "double")
  expect_type(single_test$Intensity, "double")
  expect_type(single_test$Width, "double")
  expect_type(single_test$Area, "double")
  expect_type(single_test$`Impurity/Compound`, "character")
  expect_type(single_test$Annotation, "character")
  expect_type(single_test$sampleID, "character")
  expect_identical(
    length(unique(single_test$sampleID)),
    length(list.files(single_dir, pattern = "*.csv"))
  )
})
