


test_that("relabund works",{


  spectra_binset = read.csv("compdata/spectra_binset_test.csv") %>% mutate(source = as.character(source))


  expect_error(compute_relabund_cores(SPECTRA_WITH_BINS = spectra_binset,
                                             METHOD = "x"), "choose correct method")
  expect_error(compute_relabund_cores(SPECTRA_WITH_BINS = spectra_binset,
                                             METHOD = "peaks"), "peaks data needed")
  expect_type(compute_relabund_cores(SPECTRA_WITH_BINS = spectra_binset,
                                              METHOD = "AUC"), "list") ## <-- list??

  expect_named(compute_relabund_cores(SPECTRA_WITH_BINS = spectra_binset, METHOD = "AUC"),
               c("source", "group", "relabund"))


})
