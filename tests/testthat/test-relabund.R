
test_that("relabund works", {
  spectra_test <- import_nmr_spectra_data(path = "compdata/spectra",
                                          method = "mnova", quiet = TRUE)
  spectra_binset <- assign_compound_classes(dat = spectra_test,
                                            binset = bins_Clemente2012)

  expect_error(compute_relabund_cores(DAT = spectra_binset, METHOD = "x"),
               regexp = "choose correct method")
  #  expect_error(compute_relabund_cores(DAT = spectra_binset,
  #                                             METHOD = "peaks"), "peaks data needed")
  expect_type(compute_relabund_cores(DAT = spectra_binset, METHOD = "AUC"),
              type = "list") ## <-- list??

  expect_named(compute_relabund_cores(DAT = spectra_binset, METHOD = "AUC"),
               expected = c("sampleID", "group", "relabund")
  )
})
