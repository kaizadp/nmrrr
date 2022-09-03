
#' Plot NMR spectra
#'
#' @description Plot NMR spectra, with line-brackets denoting binned regions.
#' Use spectra data processed in MestreNova or TopSpin.
#'
#' @param dat Processed spectral data, output from (a) \code{\link{nmr_import_spectra}}
#' and \code{\link{nmr_assign_bins}}; or (b) \code{\link{nmr_import_peaks}}
#' @param BINSET spectral binning
#' @param LABEL_POSITION y-axis position for bin labels
#' @param mapping aes(x = , y = )
#' @param STAGGER how much to stagger the labels? KP_TO (not clear)
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @importFrom tidyr drop_na replace_na
#' @importFrom utils head
#' @export
#' @examples
#' sdir <- system.file("extdata", "kfp_hysteresis", "spectra_mnova", package = "nmrrr")
#' spec <- nmr_import_spectra(path = sdir, method = "mnova")
#' library(ggplot2)
#' p_aes <- aes(x = ppm, y = intensity)
#' p <- nmr_plot_spectra(spec, bins_Clemente2012, 5, p_aes, STAGGER = 0.5)
#' p + ylim(0, 6)
#'
#' tdir <- system.file("extdata", "meb_burn", "spectra_topspin", package = "nmrrr")
#' spec <- nmr_import_spectra(path = tdir, method = "topspin")
#' p_aes <- aes(x = ppm, y = intensity, color = sampleID)
#' p <- nmr_plot_spectra(spec, bins_Hertkorn2013, 6e+06, p_aes, STAGGER = 1e+06)
#' p + ylim(0, 8e+06)
nmr_plot_spectra <- function(dat, BINSET, LABEL_POSITION, mapping, STAGGER) {
  # Quiet R CMD CHECK notes
  start <- number <- sampleID <- newsource <- NULL

  bins_dat <- BINSET

  # create spectra-base plot ----
  odds <- bins_dat[seq(1, nrow(bins_dat), by = 2),]
  evens <- bins_dat[seq(2, nrow(bins_dat), by = 2),]
  spectra_base <-
    ggplot() +
    # stagger bracketing lines for odd vs. even rows
    geom_segment(
      data = evens,
      aes(x = start, xend = stop, y = LABEL_POSITION, yend = LABEL_POSITION),
      color = "black"
    ) +
    geom_segment(
      data = odds,
      aes(x = start, xend = stop, y = LABEL_POSITION - 0.2, yend = LABEL_POSITION - 0.2),
      color = "black"
    ) +
    # stagger numbering like the lines
    geom_text(
      data = evens,
      aes(x = (start + stop) / 2, y = LABEL_POSITION + 0.1, label = number)
    ) +
    geom_text(
      data = odds,
      aes(x = (start + stop) / 2, y = LABEL_POSITION - 0.1, label = number)
    ) +
    scale_x_reverse(limits = c(10, 0)) +
    xlab("shift, ppm") +
    ylab("intensity")

  # add staggering factor ----

  stagger_factor <- 1 / STAGGER
  dat_y_stagger <- weak_tibble(
    sampleID = unique(dat$sampleID),
    y_factor = (seq_along(sampleID) - 1) / stagger_factor
  )

  spectra_new <- merge(dat, dat_y_stagger, by = "sampleID")
  spectra_new$intensity <- spectra_new$intensity + spectra_new$y_factor

  # combined plot ----

  spectra_base +
    geom_path(data = spectra_new, mapping)
}
