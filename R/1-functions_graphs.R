
# I. Plot NMR spectra -----------------------------------------------------


#' Compute relative abundance for each sample
#'
#' @description Compute relative abundance of compound classes for each sample.
#' This function will create a plot of NMR spectra, with line-brackets denoting binned regions
#'
#' @param dat Processed spectral data, output from (a) `import_nmr_spectra_data` and `assign_compound_classes`; or (b) process_peaks
#' @param BINSET spectral binning
#' @param LABEL_POSITION y-axis position for bin labels
#' @param mapping aes(x = , y = )
#' @param STAGGER how much to stagger the labels?
#'
#' @return The output will be a dataframe with columns describing ...
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom dplyr %>% distinct filter mutate select left_join rename
#' @importFrom tidyr drop_na replace_na
#' @importFrom utils head
gg_spectra <- function(dat, BINSET, LABEL_POSITION, mapping, STAGGER) {
  bins_dat <- set_bins(BINSET)

  # create spectra-base plot ----
  spectra_base <-
    ggplot() +
    # stagger bracketing lines for odd vs. even rows
    geom_segment(
      data = bins_dat %>% dplyr::filter(row_number() %% 2 == 0),
      aes(x = start, xend = stop, y = LABEL_POSITION, yend = LABEL_POSITION), color = "black"
    ) +
    geom_segment(
      data = bins_dat %>% dplyr::filter(row_number() %% 2 == 1),
      aes(x = start, xend = stop, y = LABEL_POSITION - 0.2, yend = LABEL_POSITION - 0.2), color = "black"
    ) +
    # stagger numbering like the lines
    geom_text(
      data = bins_dat %>% dplyr::filter(row_number() %% 2 == 0),
      aes(x = (start + stop) / 2, y = LABEL_POSITION + 0.1, label = number)
    ) +
    geom_text(
      data = bins_dat %>% dplyr::filter(row_number() %% 2 == 1),
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
    dplyr::select(sampleID, y_factor)

  spectra_new <-
    dat %>%
    left_join(dat_y_stagger) %>%
    replace_na(list(y_factor = 0)) %>%
    mutate(intensity_new = intensity + y_factor) %>%
    dplyr::select(-intensity) %>%
    rename(intensity = intensity_new)

  # combined plot ----

  spectra_base +
    geom_path(data = spectra_new, mapping)
}
