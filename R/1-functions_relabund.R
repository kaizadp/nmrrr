

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
#' @return A dataframe with columns describing ... KP_TODO
#'
#' @importFrom dplyr group_by mutate summarise filter select %>%
#' @importFrom tidyr complete
#' @importFrom DescTools AUC
#' @export
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
      mutate(AUC = replace_na(AUC, 0),
             relabund = (AUC / sum(AUC)) * 100) %>%
      select(-AUC) %>%
      # Fill in any missing ID x group combinations with zeroes
      complete(sampleID, group, fill = list(relabund = 0))
  } else {
    if (method == "peaks") {
      if(!"Area" %in% colnames(dat)) {
        stop("No 'Area' column; peaks data needed")
      }

      dat %>%
        group_by(sampleID, group) %>%
        summarize(area = sum(Area), .groups = "drop_last") %>%
        mutate(relabund = area / sum(area) * 100) %>%
        select(sampleID, group, relabund) %>%
        # KP_TODO: let user decide about filtering out NA groups?
        #filter(!is.na(group)) %>%
        replace_na(list(relabund = 0)) %>%
        # Fill in any missing ID x group combinations with zeroes
        complete(sampleID, group, fill = list(relabund = 0))
    } else {
      stop("Available methods: 'AUC' or 'peaks'")
    }
  }
}


# II. Compute relative abundance summary ----------------------------------

#' Compute relative abundance summary by treatment
#'
#' @description Compute relative-abundance summaries for each treatment.
#'
#' @param RELABUND_CORES Dataframe with relative abundance for each sample (sampleID)
#' @param COREKEY Dataframe containing sample key
#' @param TREATMENTS columns being used for grouping to summarize the data
#'
#' @return A dataframe with columns describing ... KP_TODO
#'
#' @importFrom dplyr group_by mutate summarize left_join mutate_all %>%
#' @importFrom stats sd
#' @importFrom utils read.csv
#' @export
compute_relabund_treatments <- function(RELABUND_CORES, TREATMENTS, COREKEY) {
  # Quiet R CMD CHECK notes
  group <- relabund <- n <- NULL

  corekey <- read.csv(COREKEY) %>% mutate_all(as.character)

  RELABUND_CORES %>%
    left_join(corekey) %>%
    group_by(!!!TREATMENTS, group) %>%
    summarize(
      relabund_mean = round(mean(relabund), 2),
      relabund_se = round(sd(relabund, na.rm = TRUE) / sqrt(n()), 2)
    )
}
