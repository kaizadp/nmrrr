


# I. Compute relative abundance per sample --------------------------------

#' Compute relative abundance for each sample
#'
#' @description Compute relative abundance of compound classes for each sample.
#'
#' @param DAT Processed spectral data, output from (a) `import_nmr_spectra_data`
#' and `assign_compound_classes`; or (b) process_peaks
#' @param METHOD Choose the method for calculating relative abundance.
#' Options include (a) "AUC", integrating the spectral region within each bin;
#' (b) "peaks", adding areas of peaks if a peak-picked file is provided.
#'
#' @return A dataframe with columns describing ...
#'
#' @importFrom dplyr group_by mutate summarise filter select %>%
#' @importFrom tidyr pivot_wider pivot_longer replace_na
#' @importFrom DescTools AUC
compute_relabund_cores <- function(DAT, METHOD) {
  # Quiet R CMD CHECK notes
  sampleID <- group <- ppm <- intensity <- total <- method <-
    where <- relabund <- Area <- area <- . <- NULL

  if (METHOD == "AUC") {
    relabund_temp1 <-
      DAT %>%
      mutate(sampleID = as.character(sampleID)) %>%
      group_by(sampleID, group) %>%
      summarise(
        AUC = DescTools::AUC(
          x = ppm, y = intensity,
          from = min(ppm), to = max(ppm)
        ),
        method = "trapezoid"
      ) %>%
      mutate(AUC = replace_na(AUC, 0)) %>%
      mutate(
        total = sum(AUC),
        relabund = (AUC / total) * 100
      )

    relabund_temp2_wide <-
      relabund_temp1 %>%
      select(-AUC, -method, -total) %>%
      pivot_wider(names_from = "group", values_from = "relabund")

    relabund_cores <-
      relabund_temp2_wide %>%
      pivot_longer(where(is.numeric), values_to = "relabund", names_to = "group") %>%
      replace_na(list(relabund = 0)) %>%
      mutate(relabund = round(relabund, 3))

    relabund_cores
  } else {
    if (METHOD == "peaks") {
      #      stop("peaks data needed")

      rel_abund_cores1 <-
        DAT %>%
        group_by(sampleID, group) %>%
        summarize(area = sum(Area)) %>%
        group_by(sampleID) %>%
        mutate(
          total = sum(area),
          relabund = round((area / total) * 100, 2)
        ) %>%
        select(sampleID, group, relabund) %>%
        filter(!is.na(group)) %>%
        replace(is.na(.), 0)

      rel_abund_wide1 <-
        rel_abund_cores1 %>%
        pivot_wider(names_from = "group", values_from = "relabund")

      rel_abund_cores <-
        rel_abund_wide1 %>%
        pivot_longer(where(is.numeric), values_to = "relabund", names_to = "group") %>%
        replace_na(list(relabund = 0))

      rel_abund_cores
    } else {
      stop("choose correct method: `AUC` or `peaks` ")
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
