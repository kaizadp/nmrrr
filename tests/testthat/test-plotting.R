
test_that("nmr_plot_spectra works", {

  # Errors on bad input
  expect_error(nmr_plot_spectra(cars, cars, mapping = 1),
               regexp = "must be a ggplot2::aes")

  sdir <- "compdata/spectra_mnova"
  spectra_test <- nmr_import_spectra(path = sdir, method = "mnova", quiet = TRUE)

  p <- nmr_plot_spectra(spectra_test, bins_Clemente2012, label_position = 5,
                  aes(x = ppm, y = intensity), stagger = 0.5)
  expect_s3_class(p, "ggplot")

  # The y values for the last geom should be changed from what was passed in
  # (because staggered)
  pg <- ggplot_build(p)
  lastgeom <- length(pg$data)
  expect_false(all(pg$data[[lastgeom]]$y == spectra_test$intensity))
})
