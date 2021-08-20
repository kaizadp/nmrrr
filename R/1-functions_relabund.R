


# I. Compute relative abundance per sample --------------------------------

#' Assign compound classes using the chosen binset
#'
#' @description Use this function to compute relative abundance of compound classes for each sample.
#'
#' @param SPECTRA_WITH_BINS Processed spectral data, output from `import_nmr_spectra_data` and `assign_compound_classes`
#' @param METHOD Choose the method for calculating relative abundance. Options include (a) "AUC", integrating the spectral region within each bin;
#' (b) "peaks", adding areas of peaks if a peak-picked file is provided.
#'
#' @return The output will be a dataframe with columns describing ...
#'
#' @examples
#'
#' @references

#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom magrittr %>%
#' @importFrom DescTools AUC
#'

compute_relabund_cores = function(SPECTRA_WITH_BINS, METHOD){
if(METHOD == "AUC"){
  relabund_temp1 =
    SPECTRA_WITH_BINS %>%
    mutate(source = as.character(source)) %>%
    group_by(source, group) %>%
    dplyr::summarise(AUC = DescTools::AUC(x = ppm, y = intensity,
                                          from = min(ppm), to = max(ppm)),
                     method = "trapezoid") %>%
    mutate(total = sum(AUC),
           relabund = (AUC/total)*100)
} else {
  if(METHOD == "peaks"){
    stop("peaks data needed")

  } else {
    stop ("choose correct method ")
  }
}

  relabund_temp2_wide =
    relabund_temp1 %>%
    dplyr::select(-AUC, -method, -total) %>%
    pivot_wider(names_from = "group", values_from = "relabund")

  relabund_cores =
    relabund_temp2_wide %>%
    pivot_longer(where(is.numeric), values_to = "relabund", names_to = "group") %>%
    replace_na(list(relabund = 0)) %>%
    mutate(relabund = round(relabund, 3))

  relabund_cores
}


# II. Compute relative abundance summary ----------------------------------


