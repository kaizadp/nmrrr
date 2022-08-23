
# I. Plot NMR spectra -----------------------------------------------------


#' Compute relative abundance for each sample
#'
#' @description Compute relative abundance of compound classes for each sample
#' and plot NMR spectra, with line-brackets denoting binned regions.
#'
#' @param dat Processed spectral data, output from (a) \code{\link{nmr_import_spectra}}
#' and \code{\link{nmr_assign_bins}}; or (b) \code{\link{nmr_import_peaks}}
#' @param BINSET spectral binning
#' @param LABEL_POSITION y-axis position for bin labels
#' @param mapping aes(x = , y = )
#' @param STAGGER how much to stagger the labels?
#'
#' @return A dataframe with columns describing ... KP_TODO
#'
#' @import ggplot2
#' @importFrom dplyr %>% distinct filter mutate select left_join rename
#' @importFrom tidyr drop_na replace_na
#' @importFrom utils head
#' @export
gg_spectra <- function(dat, BINSET, LABEL_POSITION, mapping, STAGGER) {
  # Quiet R CMD CHECK notes
  start <- number <- sampleID <- newsource <- y_factor <-
    intensity <- intensity_new <- row_number <- NULL

  bins_dat <- BINSET

  # create spectra-base plot ----
  spectra_base <-
    ggplot() +
    # stagger bracketing lines for odd vs. even rows
    geom_segment(
      data = bins_dat %>% filter(row_number() %% 2 == 0),
      aes(x = start, xend = stop, y = LABEL_POSITION, yend = LABEL_POSITION), color = "black"
    ) +
    geom_segment(
      data = bins_dat %>% filter(row_number() %% 2 == 1),
      aes(x = start, xend = stop, y = LABEL_POSITION - 0.2, yend = LABEL_POSITION - 0.2), color = "black"
    ) +
    # stagger numbering like the lines
    geom_text(
      data = bins_dat %>% filter(row_number() %% 2 == 0),
      aes(x = (start + stop) / 2, y = LABEL_POSITION + 0.1, label = number)
    ) +
    geom_text(
      data = bins_dat %>% filter(row_number() %% 2 == 1),
      aes(x = (start + stop) / 2, y = LABEL_POSITION - 0.1, label = number)
    ) +
    scale_x_reverse(limits = c(10, 0)) +

    # geom_path(data = dat, aes(x = ppm, y = intensity, color = source))+
    xlab("shift, ppm") +
    ylab("intensity") +
    theme_classic()

  # add staggering factor ----

  stagger_factor <- 1 / STAGGER
  dat_y_stagger <-
    dat %>%
    distinct(sampleID) %>%
    mutate(newsource = sampleID != c(NA, head(sampleID, -1))) %>%
    drop_na() %>%
    mutate(y_factor = cumsum(newsource) / stagger_factor) %>%
    select(sampleID, y_factor)

  spectra_new <-
    dat %>%
    left_join(dat_y_stagger) %>%
    replace_na(list(y_factor = 0)) %>%
    mutate(intensity_new = intensity + y_factor) %>%
    select(-intensity) %>%
    rename(intensity = intensity_new)

  # combined plot ----

  spectra_base +
    geom_path(data = spectra_new, mapping)
}
