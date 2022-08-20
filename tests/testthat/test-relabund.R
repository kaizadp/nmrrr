
test_that("relabund works", {
  spectra_binset <- read.csv("compdata/spectra_binset_test.csv")
  spectra_binset$sampleID <- as.character(spectra_binset$sampleID)

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
