

#' Compute relative abundance for each sample
#'
#' @description Compute relative abundance of compound classes for each sample.
#'
#' @param dat Processed spectral data, output from (a) \code{\link{nmr_import_spectra}}
#' and \code{\link{nmr_assign_bins}}; or (b) \code{\link{nmr_import_peaks}}
#' @param method The method for calculating relative abundance.
#' Options include (a) "AUC", integrating the spectral region within each bin;
#' (b) "peaks", adding areas of peaks if a peak-picked file is provided.
#'
#' @return A dataframe with columns describing relative contributions of compound classes. Compound classes are determined by selecting the desired bin set.
#'
#' @importFrom dplyr group_by mutate summarise filter select %>%
#' @importFrom tidyr complete
#' @importFrom DescTools AUC
#' @export
#' @examples
#' tdir <- system.file("extdata", "meb_burn", "spectra_topspin", package = "nmrrr")
#' spec <- nmr_import_spectra(path = tdir, method = "topspin")
#' spec <- nmr_assign_bins(spec, bins_Hertkorn2013)
#' nmr_relabund(spec, "AUC")
#'
#' pdir <- system.file("extdata", "amp_tempest", "peaks_mnova_single", package = "nmrrr")
#' peaks <- nmr_import_peaks(path = pdir, method = "single column")
#' peaks <- nmr_assign_bins(peaks, bins_Clemente2012)
#' nmr_relabund(peaks, "peaks")
nmr_relabund <- function(dat, method) {
  # Quiet R CMD CHECK notes
  sampleID <- group <- ppm <- intensity <- total <-
    where <- relabund <- Area <- area <- . <- NULL

  if (method == "AUC") {
    dat %>%
      group_by(sampleID, group) %>%
      summarise(
        AUC = AUC(x = ppm, y = intensity, from = min(ppm), to = max(ppm)),
        .groups = "drop_last"
      ) %>%
      mutate(
        AUC = replace_na(AUC, 0),
        relabund = (AUC / sum(AUC)) * 100
      ) %>%
      select(-AUC) %>%
      # Fill in any missing ID x group combinations with zeroes
      complete(sampleID, group, fill = list(relabund = 0))
  } else {
    if (method == "peaks") {
      if (!"Area" %in% colnames(dat)) {
        stop("No 'Area' column; peaks data needed")
      }

      dat %>%
        group_by(sampleID, group) %>%
        summarise(area = sum(Area), .groups = "drop_last") %>%
        mutate(relabund = area / sum(area) * 100) %>%
        select(sampleID, group, relabund) %>%
        # KP_TODO: let user decide about filtering out NA groups?
        # filter(!is.na(group)) %>%
        replace_na(list(relabund = 0)) %>%
        # Fill in any missing ID x group combinations with zeroes
        complete(sampleID, group, fill = list(relabund = 0))
    } else {
      stop("Available methods: 'AUC' or 'peaks'")
    }
  }
}
