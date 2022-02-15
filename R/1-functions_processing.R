
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
    rename(sampleID = source) %>%
    arrange(sampleID, ppm) %>%
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



# IV. PROCESS PEAKS -------------------------------------------------------


#' Process picked peaks data
#'
#' @description Use this function to process data of peaks picked with NMR software.
#'
#' @param PEAKS_FILES file path for peaks data (input). All the peaks files are saved as individual .csv files
#' @param METHOD format of input data, depending on how the data were exported. "multiple columns": use when data are in split-column format, obtained by pasting "peaks table" in MNova.
#' "single column": use when data are in single-column format, exported from MNova as "peaks script".
#' @param BINSET Choose the binset you want. Options include: "Clemente2012", "Lynch2019", "Hertkorn2013_MeOD", "Mitchell2018"
#'
#' @return The output will be a dataframe with columns describing
#'   sample ID, ppm, intensity, area, group name.
#'
#' @examples
#'

#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @importFrom utils read.table
#'
#'

process_peaks = function(PEAKS_FILES, METHOD){
  # import and process picked peaks data
  # data are typically saved as multiple files
  # import and compile


  ## then, set the file path for the peaks data
  filePaths_peaks <- list.files(path = PEAKS_FILES,pattern = "*.csv", full.names = TRUE)

  if(METHOD == "multiple columns"){
    ## if peaks data are provided in split-column format
    peaks_rawdat <- do.call(bind_rows, lapply(filePaths_peaks, function(path) {
      # this function will import all the data files and combine for all samples
      # first, we run the function to clean a single file
      # the input data are spread across multiple columns, so use this function to align columns

      align_columns = function(path){
        # Step 1. import file.
        # check.names=FALSE because columns have duplicate names, and we want to leave as is
        df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

        # Step 2. confirm that the data are in 9-column groups
        noname_cols <- which(names(df) == "")
        if(!all(diff(noname_cols) == 9)) {
          stop("Formatting problem: data don't appear to be in 9-column groups")
        }
        names(df)[noname_cols] <- "Obs"  # give them a name

        # Step 3. Extract each group in turn and store temporarily in a list
        nmr_list <- lapply(noname_cols, function(x) df[x:(x + 8)])

        # Step 4. Finally, bind everything into a single data frame
        # This uses dplyr but we could also use base R: do.call("rbind", nmr_list)
        nmr_dat <- dplyr::bind_rows(nmr_list)

        # Step 5. Create a new column that includes source sample name
        nmr_dat[["source"]] <- rep(path, nrow(df))

        nmr_dat
      }

      # now create an object from the function
      align_columns(path)
      # this will be repeated for each file in the input folder

    }))

  } else {
    if(METHOD == "single column"){
      ## if peaks data are provided in single-column format
      peaks_rawdat <- do.call(rbind, lapply(filePaths_peaks, function(path) {
        # the files are tab-delimited, so read.csv will not work. import using read.table
        # there is no header. so create new column names
        # then add a new column `source` to denote the file name
        df <- read.delim(path,
                         col.names = c("ppm", "Intensity", "Width", "Area", "Type",
                                       "Flags", "Impurity/Compound", "Annotation"))
        df[["source"]] <- rep(path, nrow(df))
        df}))

    } else {
      if(METHOD == "topspin"){
        ## if peaks data are provided in topspin format
        peaks_rawdat <- do.call(rbind, lapply(filePaths_peaks, function(path) {
          # the files are tab-delimited, so read.csv will not work. import using read.table
          # there is no header. so create new column names
          # then add a new column `source` to denote the file name
          df <- read.csv(path,
                           col.names = c("peak", "ppm", "Intensity", "Annotation"))
          df[["source"]] <- rep(path, nrow(df))
          df}))

    } else {
      stop("choose correct method. options are `multiple columns` and `single column` and `topspin`")
    }

  }}
  # process the dataset


}


process_peaks_data = function(peaks_rawdat, BINSET){

  ## first, set the bins
  bins_dat = set_bins(BINSET)


  processed =
    peaks_rawdat %>%
    filter(ppm >= 0 & ppm <= 10) %>%
    filter(Intensity > 0) %>%
    filter(!is.na(ppm)) %>%
    # filter(!Flags == "Weak") %>%
    mutate(source = str_remove(source, paste0(PEAKS_FILES, "/"))) %>%
    mutate(source = str_remove(source, ".csv")) %>%
    rename(sampleID = source) %>%
    force()

  bins_dat2 =
    bins_dat %>%
    dplyr::select(group, start, stop)

  # merge the peaks data with the  bins
  subset(merge(processed, bins_dat2), start <= ppm & ppm <= stop) %>%
    dplyr::select(-start, -stop)
}
