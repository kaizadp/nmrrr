

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
#' @return A \code{\link{data.frame}} with columns describing relative contributions of
#' compound classes. Compound classes are determined by selecting the desired
#' binset.
#'
#' @importFrom tidyr complete
#' @importFrom DescTools AUC
#' @export
#' @author Kaizad Patel
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
  sampleID <- group <- NULL

  # Helper function
  compute_relabund <- function(x) {
    x$AUC <- tidyr::replace_na(x$AUC, 0)
    # Split by sample ID, compute relative abundance, drop AUC
    x_list <- split(x, list(x$sampleID))
    x_list <- lapply(x_list, function(x)
    { x$relabund <- x$AUC / sum(x$AUC) * 100; x })
    x_relabund <- do.call("rbind", x_list)
    x_relabund$AUC <- NULL
    # Fill in any missing ID x group combinations with zeroes
    complete(x_relabund, sampleID, group, fill = list(relabund = 0))
  }

  if (method == "AUC") {
    # Compute AUC
    dat_list <- split(dat, list(dat$sampleID, dat$group))
    dat_list <- lapply(dat_list, function(x) {
      weak_tibble(sampleID = unique(x$sampleID),
                  group = unique(x$group),
                  AUC = AUC(x = x$ppm, y = x$intensity,
                            from = min(x$ppm), to = max(x$ppm)))
    })
    dat_auc <- do.call("rbind", dat_list)

    compute_relabund(dat_auc)

  } else {
    if (method == "peaks") {
      if (!"Area" %in% colnames(dat)) {
        stop("No 'Area' column; peaks data needed")
      }

      # Compute AUC
      dat_list <- split(dat, list(dat$sampleID, dat$group))
      dat_list <- lapply(dat_list, function(x) {
        weak_tibble(sampleID = unique(x$sampleID),
                    group = unique(x$group),
                    AUC = sum(x$Area))
      })
      dat_auc <- do.call("rbind", dat_list)
      compute_relabund(dat_auc)

    } else {
      stop("Available methods: 'AUC' or 'peaks'")
    }
  }
}
