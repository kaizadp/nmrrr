


test_that("relabund works",{


  spectra_binset = read.csv("compdata/spectra_binset_test.csv") %>% mutate(sampleID = as.character(sampleID))


  expect_error(compute_relabund_cores(DAT = spectra_binset,
                                             METHOD = "x"), "choose correct method")
#  expect_error(compute_relabund_cores(DAT = spectra_binset,
#                                             METHOD = "peaks"), "peaks data needed")
  expect_type(compute_relabund_cores(DAT = spectra_binset,
                                              METHOD = "AUC"), "list") ## <-- list??

  expect_named(compute_relabund_cores(DAT = spectra_binset, METHOD = "AUC"),
               c("sampleID", "group", "relabund"))


})
