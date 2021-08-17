
# I. NMR BINS -------------------------------------------------------------

#' Choose a bin set to group and integrate the NMR peaks
#'
#' @description The NMR spectrum can be split into several bins, based on chemical shift (ppm).
#'   Choose a binset to group the peaks.
#'   Binsets are specific to nuclei and solvents.
#'
#' @param BINSET Choose the binset you want.
#'   Options include: "Clemente2012", "Lynch2019", "Hertkorn2013_MeOD", "Mitchell2018"
#'
#' @return The output will be a dataframe with columns describing
#'   the group name (sometimes abbreviated), start and stop boundaries, and a longer, more complete description of the group.
#'
#' @examples
#'
#' @references
#' JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance Methods for the Analysis of Organic Matter
#' Composition from Soil Density and Particle Fractions.”
#' Environmental Chemistry. https://doi.org/10.1071/EN11096.
#'
#' LM Lynch et al. 2019. “Dissolved Organic Matter Chemistry and Transport along an Arctic Tundra Hillslope.”
#' Global Biogeochemical Cycles. https://doi.org/10.1029/2018GB006030.
#'
#' P Mitchell et al. 2018.
#' “Nuclear Magnetic Resonance Analysis of Changes in Dissolved Organic Matter Composition
#' with Successive Layering on Clay Mineral Surfaces.”
#' Soil Systems. https://doi.org/10.3390/soils2010008.

#' @importFrom dplyr arrange
#' @importFrom dplyr row_number
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom stats start
#' @importFrom utils read.delim

set_bins = function(BINSET){
  filePath_bins <- list.files(path = "bins", pattern = BINSET, full.names = TRUE)

  if(length(filePath_bins) > 1){
    stop("too many options!")
  }
  if(length(filePath_bins) == 0 ){
    stop("option not available")
  }

  #  read.delim(filePath_bins, header = TRUE) %>%
  #    dplyr::arrange(start) %>%
  #    dplyr::mutate(number = row_number())

  a = read.delim(filePath_bins, header = TRUE)
  b = dplyr::arrange(a, start)
  c = dplyr::mutate(b, number = dplyr::row_number())
  c
}


#

# II. IMPORT SPECTRA DATA -------------------------------------------------

#' Import and process NMR spectral data
#'
#' @description Use this function to import multiple spectra files, combine them,
#' and then process/clean the data.
#'
#' @param
#' SPECTRA_FILES path/directory where the spectra files are saved
#'
#' @return The output will be a dataframe with columns describing
#'   the group name (sometimes abbreviated), start and stop boundaries, and a longer, more complete description of the group.
#'
#' @examples
#'
#' @references


#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @importFrom utils read.table

import_nmr_spectra_data = function(SPECTRA_FILES){
  # import and combine spectra data files
  filePaths_spectra <- list.files(path = SPECTRA_FILES,pattern = "*.csv", full.names = TRUE)

  if(length(filePaths_spectra) == 0){
    stop("no .csv files found!")
  }
  spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
    # the files are tab-delimited, so read.csv will not work. import using read.table
    # there is no header. so create new column names
    # then add a new column `source` to denote the file name
    df <- read.table(path, header=FALSE, col.names = c("ppm", "intensity"))
    df[["source"]] <- rep(path, nrow(df))
    df}))

  # clean the spectral data
  spectra_dat %>%
    mutate(source = str_remove(source, paste0(SPECTRA_FILES, "/"))) %>%
    mutate(source = str_remove(source, ".csv")) %>%
    mutate(source = as.character(source)) %>%
    arrange(source, ppm) %>%
    force()
}

#
# III. ASSIGN BINS --------------------------------------------------------

#' Assign compound classes using the chosen binset
#'
#' @description Use this function to import multiple spectra files, combine them,
#' and then process/clean the data.
#'
#' @param dat Input dataframe. This could be spectral data, or peak picked data. Must include a `ppm` column for compound class assignment.
#' @param BINSET Choose the binset you want. Options include: "Clemente2012", "Lynch2019", "Hertkorn2013_MeOD", "Mitchell2018"
#'
#' @return The output will be a dataframe with columns describing
#'   the group name (sometimes abbreviated), start and stop boundaries, and a longer, more complete description of the group.
#'
#' @examples
#'
#' @references
#' JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance Methods for the Analysis of Organic Matter
#' Composition from Soil Density and Particle Fractions.”
#' Environmental Chemistry. https://doi.org/10.1071/EN11096.
#'
#' LM Lynch et al. 2019. “Dissolved Organic Matter Chemistry and Transport along an Arctic Tundra Hillslope.”
#' Global Biogeochemical Cycles. https://doi.org/10.1029/2018GB006030.
#'
#' P Mitchell et al. 2018.
#' “Nuclear Magnetic Resonance Analysis of Changes in Dissolved Organic Matter Composition
#' with Successive Layering on Clay Mineral Surfaces.”
#' Soil Systems. https://doi.org/10.3390/soils2010008.


#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom utils read.table
#'
assign_compound_classes = function(dat, BINSET){
  # load binsets
  bins_dat = set_bins(BINSET) %>% dplyr::select(group, start, stop)

  # assign bins to each point
  subset(merge(dat, bins_dat),
         start <= ppm & ppm <= stop) %>%
    dplyr::select(-start, -stop)
}

